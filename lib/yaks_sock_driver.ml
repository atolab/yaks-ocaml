open Lwt.Infix
open Yaks_types
open Yaks_sock_types
open Yaks_sock_types.Message
open Yaks_fe_sock_codec

module WorkingMap = Map.Make(Apero.Vle)
module ListenersMap = Map.Make(String)
module EvalsMap = Map.Make(Path)

type state = {
  sock : Lwt_unix.file_descr
; working_set : (Yaks_fe_sock_types.message list Lwt.u * Yaks_fe_sock_types.message list) WorkingMap.t
; subscribers : listener_t ListenersMap.t
; evals : eval_callback_t EvalsMap.t
; buffer_pool : Abuf.t Lwt_pool.t
}

type t = state MVar.t

let max_buffer_size = 64 * 1024
let max_buffer_count = 32


let check_socket sock = 
  let _ =
    match Lwt_unix.state sock with
    | Opened -> ignore @@ Logs_lwt.info (fun m -> m "Socket is open")
    | Closed -> ignore @@ Logs_lwt.info (fun m -> m "Socket is closed")
    | Aborted e -> ignore @@ Logs_lwt.info (fun m -> m "Socket is aborted: %s" (Printexc.to_string e))
  in ()

let send_to_socket msg pool sock =
  let open Apero in
  Lwt_pool.use pool (fun buf ->
    Abuf.clear buf;
    Yaks_fe_sock_codec.encode_message msg buf;
    Lwt_pool.use pool (fun lbuf ->
      Abuf.clear lbuf;
      (try encode_vle (Vle.of_int @@ Abuf.readable_bytes buf) lbuf
      with e -> Logs.err (fun m -> m "Failed in encoding messge: %s" (Printexc.to_string e)); raise e);
      Lwt.return @@ Abuf.wrap [lbuf; buf]
    )
  ) >>= fun buf ->
  Logs_lwt.debug (fun m -> m "Sending message to socket") >>= fun _ ->
  check_socket sock;
  Lwt.catch (fun () -> Net.write_all sock buf)
            (fun e ->  Logs_lwt.err (fun m -> m "Failed in writing message: %s" (Printexc.to_string e)) >>= fun () -> Lwt.fail e) >>= fun bs ->
  Logs_lwt.debug (fun m -> m "Sended %d bytes" bs)


let process_eval selector (driver:t) =
  let open Apero in
  MVar.read driver >>= fun self ->
  let params = match Selector.properties selector with
    | Some props -> Properties.of_string props
    | None -> Properties.empty
  in
  let matching_evals = EvalsMap.filter (fun path _ -> Selector.is_matching_path path selector) self.evals in
  if (EvalsMap.is_empty matching_evals) then
    let%lwt _ = Logs_lwt.warn (fun m -> m "No matching eval for GET on: %s" (Selector.to_string selector)) in
    Lwt.return []
  else
    EvalsMap.fold (fun path eval l -> (path,eval)::l) matching_evals [] |>
    Lwt_list.map_p (fun (path, eval) -> eval path params >>= fun result -> Lwt.return (path, result) )

let receiver_loop (driver:t) = 
  let open Apero in
  let open Apero.Infix in
  MVar.read driver >>= fun self ->
  let%lwt len = Net.read_vle self.sock >>= Vle.to_int %> Lwt.return in
  let%lwt _ = Logs_lwt.debug (fun m -> m "Message lenght : %d" len) in
  let buf = Abuf.create len in
  let%lwt n = Net.read_all self.sock buf len in
  let () = check_socket self.sock in
  let%lwt _ = Logs_lwt.debug (fun m -> m "Read %d bytes out of the socket" n) in
  (try decode_message buf
  with e ->  Logs.err (fun m -> m "Failed in parsing message %s" (Printexc.to_string e)) ; raise e) |> fun msg ->
  match (msg.header.mid, msg.body) with
  | (NOTIFY, YNotification (subid, data)) ->
    MVar.read driver >>= fun self ->
    (match ListenersMap.find_opt subid self.subscribers with
    | Some cb ->
      (* Run listener's callback in future (catching exceptions) *)
      let _ =  Lwt.try_bind (fun () -> let%lwt _ = Logs_lwt.debug (fun m -> m "Notify received. Call listener for subscription %s" subid) in cb data)
        (fun () -> Lwt.return_unit)
        (fun ex -> let%lwt _ = Logs_lwt.warn (fun m -> m "Listener's callback of subscription %s raised an exception: %s\n %s" subid (Printexc.to_string ex) (Printexc.get_backtrace ())) in Lwt.return_unit)
      in
      (* Return unit immediatly to release socket reading thread *)
      Lwt.return_unit
    | None ->  
      let%lwt _ = Logs_lwt.debug (fun m -> m "Received notification with unknown subscriberid %s" subid) in
      Lwt.return_unit)

  | (EVAL, YSelector s) ->
    (* Process eval in future (catching exceptions) *)
    let _ = Lwt.try_bind
      (fun () -> process_eval s driver >>= fun results ->
      make_values msg.header.corr_id results >>= fun rmsg ->
      send_to_socket rmsg self.buffer_pool self.sock)
        (fun () -> Lwt.return_unit)
        (fun ex -> let%lwt _ = Logs_lwt.warn (fun m -> m "Eval's callback raised an exception: %s\n %s" (Printexc.to_string ex) (Printexc.get_backtrace ())) in
          make_error msg.header.corr_id INTERNAL_SERVER_ERROR >>= fun rmsg ->
          send_to_socket rmsg self.buffer_pool self.sock)
    in
    (* Return unit immediatly to release socket reading thread *)
    Lwt.return_unit

  | (_, _) -> MVar.guarded driver @@ (fun self ->
    (match WorkingMap.find_opt msg.header.corr_id self.working_set with 
    | Some (resolver, msg_list) ->
      let msg_list = List.append msg_list [msg] in
      if (Yaks_fe_sock_types.has_incomplete_flag msg.header.flags) then
        MVar.return () {self with working_set = WorkingMap.add msg.header.corr_id (resolver, msg_list) self.working_set}
      else
        let _ = Lwt.wakeup_later resolver msg_list in
        MVar.return () {self with working_set = WorkingMap.remove msg.header.corr_id self.working_set}
    | None -> let%lwt _ = Logs_lwt.warn (fun m -> m "Received message with unknown correlation id %Ld" msg.header.corr_id) in
      MVar.return () self)
    )

let rec loop driver  () =
  receiver_loop driver >>= loop driver

let create locator = 
  let sock = Lwt_unix.socket PF_INET SOCK_STREAM 0 in
  Apero_net.connect sock locator >>= fun s ->
  let driver = MVar.create {
    sock = s;
    working_set = WorkingMap.empty;
    subscribers = ListenersMap.empty;
    evals = EvalsMap.empty;
    buffer_pool = Lwt_pool.create max_buffer_count (fun () -> Lwt.return @@ Abuf.create_bigstring ~grow:8192 max_buffer_size) }
  in
  let _ = loop driver () in
  Lwt.return driver

let destroy driver =
  MVar.read driver >>= fun d ->
  Apero_net.safe_close d.sock

let process (msg:Yaks_fe_sock_types.message) driver = 
  MVar.guarded driver @@
  fun self ->
  let promise, completer = Lwt.wait () in
  send_to_socket msg self.buffer_pool self.sock
  >>= fun _ ->
  MVar.return_lwt promise {self with working_set = WorkingMap.add msg.header.corr_id (completer,[]) self.working_set}

let check_reply_ok corr_id (replymsgs:Yaks_fe_sock_types.message list) =
  if List.length replymsgs <> 1 then
    Lwt.fail_with @@ Printf.sprintf "[YAS]: Expected 1 OK reply, but get %d" (List.length replymsgs)
  else
    let replymsg = List.hd replymsgs in
    if replymsg.header.corr_id <> corr_id then
      Lwt.fail_with "Correlation Id is different!"
    else
      let open Yaks_fe_sock_codes in
      match (replymsg.header.mid, replymsg.body) with
      | (OK, YEmpty) -> Lwt.return_unit
      | (ERROR, YErrorInfo e) ->
        let errno = Apero.Vle.to_int e in
        Lwt.fail_with @@ Printf.sprintf "[YAS]: Received ERROR reply with ErrNo: %d" errno
      | (_, _) ->
        Lwt.fail_with @@ Printf.sprintf "[YAS]: Received unexpected reply!!"

let process_login props (driver:t) =
  let _ = Logs_lwt.info (fun m -> m "[YASD]: LOGIN") in
  make_login props
  >>= fun msg -> process msg driver
  >>= check_reply_ok msg.header.corr_id

let process_logout (driver:t) =
  let _ = Logs_lwt.info (fun m -> m "[YASD]: LOGOUT") in
  make_logout ()
  >>= fun msg -> process msg driver
  >>= check_reply_ok msg.header.corr_id

let process_workspace path (driver:t) =
  let _ = Logs_lwt.info (fun m -> m "[YASD]: WORKSPACE") in
  make_workspace path
  >>= fun msg -> process msg driver
  >>= fun rmsgs ->
    if List.length rmsgs <> 1 then
      Lwt.fail_with @@ Printf.sprintf "[YAS]: Expected 1 OK reply, but get %d replies" (List.length rmsgs)
    else
      let rmsg = List.hd rmsgs in
      let wsid = Apero.Properties.find Yaks_properties.Admin.workspaceid rmsg.header.properties in
      Lwt.return wsid

let process_get ?quorum ?workspace selector (driver:t) =
  let _ = Logs_lwt.info (fun m -> m "[YASD]: GET on %s" (Selector.to_string selector)) in
  make_get ?quorum ?workspace selector
  >>= fun msg -> process msg driver
  >>= fun rmsgs ->
    if List.length rmsgs = 0 then
      Lwt.fail_with @@ Printf.sprintf "[YAS]: GET on %s: expected at least 1 reply, but get 0 reply" (Selector.to_string selector)
    else
      let hd = List.hd rmsgs in
      match hd.body with
      | YPathValueList _ -> Lwt.return @@
        List.fold_left (fun l (rmsg:Yaks_fe_sock_types.message) ->
          match rmsg.body with
          | YPathValueList pvs -> List.append l pvs
          | _ -> 
            let _ = Logs_lwt.warn (fun m -> m "[YASD]: GET on %s: Received an invalid reply (wrong body type)" (Selector.to_string selector)) in
            l
          ) [] rmsgs
      | YErrorInfo e ->
        let errno = Apero.Vle.to_int e in
        Lwt.fail_with @@ Printf.sprintf "[YAS]: GET on %s: ErrNo %d" (Selector.to_string selector) errno
      | _ -> Lwt.fail_with @@ Printf.sprintf "[YAS]: GET on %s: Received an invalid reply (wrong body type)" (Selector.to_string selector)

let process_put ?quorum ?workspace path value (driver:t) =
  let _ = Logs_lwt.info (fun m -> m "[YASD]: PUT on %s -> %s" (Path.to_string path) (Value.to_string value)) in
  make_put ?quorum ?workspace path value
  >>= fun msg -> process msg driver
  >>= check_reply_ok msg.header.corr_id

let process_update ?quorum ?workspace path value (driver:t) =
  let _ = Logs_lwt.info (fun m -> m "[YASD]: PUT on %s -> %s" (Path.to_string path) (Value.to_string value)) in
  make_update ?quorum ?workspace path value
  >>= fun msg -> process msg driver
  >>= check_reply_ok msg.header.corr_id

let process_remove ?quorum ?workspace path (driver:t) =
  let _ = Logs_lwt.info (fun m -> m "[YASD]: REMOVE") in
  make_remove ?quorum ?workspace path
  >>= fun msg -> process msg driver
  >>= check_reply_ok msg.header.corr_id

let process_subscribe ?workspace ?(listener=fun _ -> Lwt.return_unit) selector (driver:t) =
  let _ = Logs_lwt.info (fun m -> m "[YASD]: SUB on %s" (Selector.to_string selector)) in
  make_sub ?workspace selector
  >>= fun msg ->  process msg driver
  >>= fun rmsgs ->
    if List.length rmsgs <> 1 then
      Lwt.fail_with @@ Printf.sprintf "[YAS]: Expected 1 OK reply, but get %d replies" (List.length rmsgs)
    else
      let rmsg = List.hd rmsgs in
      let subid = Apero.Properties.find Yaks_properties.Admin.subscriberid rmsg.header.properties in
      MVar.guarded driver @@ fun self ->
      MVar.return subid {self with subscribers = ListenersMap.add subid listener self.subscribers}

let process_unsubscribe subid (driver:t) =
  let _ = Logs_lwt.info (fun m -> m "[YASD]: UNSUB on %s" subid) in
  make_unsub subid
  >>= fun msg ->  process msg driver
  >>= check_reply_ok msg.header.corr_id
  >>= fun () ->
    MVar.guarded driver @@ fun self ->
    MVar.return () {self with subscribers = ListenersMap.remove subid self.subscribers}


let to_absolute_path ?workpath path =
  if Path.is_relative path then
    match workpath with
    | Some p -> Path.add_prefix ~prefix:p path
    | None -> Path.add_prefix ~prefix:(Path.of_string "/") path
  else path

let process_register_eval ?workspace ?workpath path eval_callback (driver:t) =
  let _ = Logs_lwt.info (fun m -> m "[YASD]: REG_EVAL on %s" (Path.to_string path)) in
  make_reg_eval ?workspace path
  >>= fun msg ->  process msg driver
  >>= check_reply_ok msg.header.corr_id
  >>= fun () ->
    (* Note: callbacks are locally registered with absolute path since evals will come with absolute selectors *)
    let abspath = to_absolute_path ?workpath path in
    MVar.guarded driver @@ fun self ->
    MVar.return () {self with evals = EvalsMap.add abspath eval_callback self.evals}

let process_unregister_eval ?workspace ?workpath path (driver:t) =
  let _ = Logs_lwt.info (fun m -> m "[YASD]: UNREG_EVAL on %s" (Path.to_string path)) in
  make_unreg_eval ?workspace path
  >>= fun msg ->  process msg driver
  >>= check_reply_ok msg.header.corr_id
  >>= fun () ->
    let abspath = to_absolute_path ?workpath path in
    MVar.guarded driver @@ fun self ->
    MVar.return () {self with evals = EvalsMap.remove abspath self.evals}

let process_eval ?multiplicity ?workspace selector (driver:t) =
  let _ = Logs_lwt.info (fun m -> m "[YASD]: EVAL on %s" (Selector.to_string selector)) in
  make_eval ?multiplicity ?workspace selector
  >>= fun msg -> process msg driver
  >>= fun rmsgs ->
    if List.length rmsgs = 0 then
      Lwt.fail_with @@ Printf.sprintf "[YAS]: EVAL on %s: expected at least 1 reply, but get 0 reply" (Selector.to_string selector)
    else
      let hd = List.hd rmsgs in
      match hd.body with
      | YPathValueList _ -> Lwt.return @@
        List.fold_left (fun l (rmsg:Yaks_fe_sock_types.message) ->
          match rmsg.body with
          | YPathValueList pvs -> List.append l pvs
          | _ -> 
            let _ = Logs_lwt.warn (fun m -> m "[YASD]: EVAL on %s: Received an invalid reply (wrong body type)" (Selector.to_string selector)) in
            l
          ) [] rmsgs
      | YErrorInfo e ->
        let errno = Apero.Vle.to_int e in
        Lwt.fail_with @@ Printf.sprintf "[YAS]: EVAL on %s: ErrNo %d" (Selector.to_string selector) errno
      | _ -> Lwt.fail_with @@ Printf.sprintf "[YAS]: EVAL on %s: Received an invalid reply (wrong body type)" (Selector.to_string selector)


