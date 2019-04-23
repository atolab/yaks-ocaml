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
  match Lwt_unix.state sock with
  | Opened -> Logs.info (fun m -> m "Socket is open")
  | Closed -> Logs.info (fun m -> m "Socket is closed")
  | Aborted e -> Logs.info (fun m -> m "Socket is aborted: %s" (Printexc.to_string e))

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
  Logs.debug (fun m -> m "Sending message to socket");
  check_socket sock;
  Lwt.catch (fun () -> Net.write_all sock buf)
            (fun e ->  Logs.err (fun m -> m "Failed in writing message: %s" (Printexc.to_string e)); Lwt.fail e)
  >>= fun bs -> Logs.debug (fun m -> m "Sent %d bytes" bs); Lwt.return_unit


let process_incoming_eval selector (driver:t) =
  let open Apero in
  MVar.read driver >>= fun self ->
  let params = match Selector.properties selector with
    | Some props -> Properties.of_string props
    | None -> Properties.empty
  in
  let matching_evals = EvalsMap.filter (fun path _ -> Selector.is_matching_path path selector) self.evals in
  if (EvalsMap.is_empty matching_evals) then (
    Logs.warn (fun m -> m "No matching eval for GET on: %s" (Selector.to_string selector));
    Lwt.return [])
  else
    EvalsMap.fold (fun path eval l -> (path,eval)::l) matching_evals [] |>
    Lwt_list.map_p (fun (path, eval) -> eval path params >>= fun result -> Lwt.return (path, result) )

let receiver_loop (driver:t) = 
  let open Apero in
  let open Apero.Infix in
  MVar.read driver >>= fun self ->
  let%lwt len = Net.read_vle self.sock >>= Vle.to_int %> Lwt.return in
  Logs.debug (fun m -> m "Message lenght : %d" len);
  let buf = Abuf.create len in
  let%lwt n = Net.read_all self.sock buf len in
  let () = check_socket self.sock in
  Logs.debug (fun m -> m "Read %d bytes out of the socket" n);
  (try decode_message buf
  with e ->  Logs.err (fun m -> m "Failed in parsing message %s" (Printexc.to_string e)) ; raise e) |> fun msg ->
  match (msg.header.mid, msg.body) with
  | (NOTIFY, YNotification (subid, data)) ->
    MVar.read driver >>= fun self ->
    (match ListenersMap.find_opt subid self.subscribers with
    | Some cb ->
      (* Run listener's callback in future (catching exceptions) *)
      let _ =  Lwt.try_bind (fun () -> Logs.debug (fun m -> m "Notify received. Call listener for subscription %s" subid); cb data)
        (fun () -> Lwt.return_unit)
        (fun ex -> Logs.warn (fun m -> m "Listener's callback of subscription %s raised an exception: %s\n %s" subid (Printexc.to_string ex) (Printexc.get_backtrace ())); Lwt.return_unit)
      in
      (* Return unit immediatly to release socket reading thread *)
      Lwt.return_unit
    | None ->  
      Logs.debug (fun m -> m "Received notification with unknown subscriberid %s" subid);
      Lwt.return_unit)

  | (EVAL, YSelector s) ->
    (* Process eval in future (catching exceptions) *)
    let _ = Lwt.try_bind
      (fun () -> process_incoming_eval s driver >>= fun results ->
      let rmsg = make_values msg.header.corr_id results in
      send_to_socket rmsg self.buffer_pool self.sock)
        (fun () -> Lwt.return_unit)
        (fun ex -> Logs.warn (fun m -> m "Eval's callback raised an exception: %s\n %s" (Printexc.to_string ex) (Printexc.get_backtrace ()));
          let rmsg = make_error msg.header.corr_id INTERNAL_SERVER_ERROR in
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
    | None -> Logs.warn (fun m -> m "Received message with unknown correlation id %Ld" msg.header.corr_id);
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

let no_reply = Lwt.return []

let process (msg:Yaks_fe_sock_types.message) ?(expect_reply=true) driver = 
  MVar.guarded driver @@
  fun self ->
  send_to_socket msg self.buffer_pool self.sock
  >>= fun _ ->
  if expect_reply then
    let promise, completer = Lwt.wait () in
    MVar.return_lwt promise {self with working_set = WorkingMap.add msg.header.corr_id (completer,[]) self.working_set}
  else
    MVar.return_lwt no_reply self

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
  Logs.info (fun m -> m "[YASD]: LOGIN");
  let msg = make_login props in
  process msg driver
  >>= check_reply_ok msg.header.corr_id

let process_logout (driver:t) =
  Logs.info (fun m -> m "[YASD]: LOGOUT");
  let msg = make_logout () in
  process msg driver
  >>= check_reply_ok msg.header.corr_id

let process_workspace path (driver:t) =
  Logs.info (fun m -> m "[YASD]: WORKSPACE");
  let msg = make_workspace path in
  process msg driver
  >>= fun rmsgs ->
    if List.length rmsgs <> 1 then
      Lwt.fail_with @@ Printf.sprintf "[YAS]: Expected 1 OK reply, but get %d replies" (List.length rmsgs)
    else
      let rmsg = List.hd rmsgs in
      let wsid = Apero.Properties.find Yaks_properties.Admin.workspaceid rmsg.header.properties in
      Lwt.return wsid

let process_get ?quorum ws_props selector (driver:t) =
  Logs.info (fun m -> m "[YASD]: GET on %s" (Selector.to_string selector));
  let msg = make_get ?quorum ws_props selector in
  process msg driver
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
            Logs.warn (fun m -> m "[YASD]: GET on %s: Received an invalid reply (wrong body type)" (Selector.to_string selector));
            l
          ) [] rmsgs
      | YErrorInfo e ->
        let errno = Apero.Vle.to_int e in
        Lwt.fail_with @@ Printf.sprintf "[YAS]: GET on %s: ErrNo %d" (Selector.to_string selector) errno
      | _ -> Lwt.fail_with @@ Printf.sprintf "[YAS]: GET on %s: Received an invalid reply (wrong body type)" (Selector.to_string selector)

let process_put ?quorum ws_props path value (driver:t) =
  Logs.info (fun m -> m "[YASD]: PUT on %s -> %s" (Path.to_string path) (Value.to_string value));
  let msg = make_put ?quorum ws_props path value in
  process msg ~expect_reply:false driver
  >>= fun _ -> Lwt.return_unit

let process_update ?quorum ws_props path value (driver:t) =
  Logs.info (fun m -> m "[YASD]: PUT on %s -> %s" (Path.to_string path) (Value.to_string value));
  let msg = make_update ?quorum ws_props path value in
  process msg driver
  >>= check_reply_ok msg.header.corr_id

let process_remove ?quorum ws_props path (driver:t) =
  Logs.info (fun m -> m "[YASD]: REMOVE");
  let msg = make_remove ?quorum ws_props path in
  process msg driver
  >>= check_reply_ok msg.header.corr_id

let process_subscribe ws_props ?(listener=fun _ -> Lwt.return_unit) selector (driver:t) =
  Logs.info (fun m -> m "[YASD]: SUB on %s" (Selector.to_string selector));
  let msg = make_sub ws_props selector in
   process msg driver
  >>= fun rmsgs ->
    if List.length rmsgs <> 1 then
      Lwt.fail_with @@ Printf.sprintf "[YAS]: Expected 1 OK reply, but get %d replies" (List.length rmsgs)
    else
      let rmsg = List.hd rmsgs in
      let subid = Apero.Properties.find Yaks_properties.Admin.subscriberid rmsg.header.properties in
      MVar.guarded driver @@ fun self ->
      MVar.return subid {self with subscribers = ListenersMap.add subid listener self.subscribers}

let process_unsubscribe subid (driver:t) =
  Logs.info (fun m -> m "[YASD]: UNSUB on %s" subid);
  let msg = make_unsub subid in
   process msg driver
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

let process_register_eval ws_props ?workpath path eval_callback (driver:t) =
  Logs.info (fun m -> m "[YASD]: REG_EVAL on %s" (Path.to_string path));
  let msg = make_reg_eval ws_props path in
   process msg driver
  >>= check_reply_ok msg.header.corr_id
  >>= fun () ->
    (* Note: callbacks are locally registered with absolute path since evals will come with absolute selectors *)
    let abspath = to_absolute_path ?workpath path in
    MVar.guarded driver @@ fun self ->
    MVar.return () {self with evals = EvalsMap.add abspath eval_callback self.evals}

let process_unregister_eval ws_props ?workpath path (driver:t) =
  Logs.info (fun m -> m "[YASD]: UNREG_EVAL on %s" (Path.to_string path));
  let msg = make_unreg_eval ws_props path in
   process msg driver
  >>= check_reply_ok msg.header.corr_id
  >>= fun () ->
    let abspath = to_absolute_path ?workpath path in
    MVar.guarded driver @@ fun self ->
    MVar.return () {self with evals = EvalsMap.remove abspath self.evals}

let process_eval ?multiplicity ws_props selector (driver:t) =
  Logs.info (fun m -> m "[YASD]: EVAL on %s" (Selector.to_string selector));
  let msg = make_eval ?multiplicity ws_props selector in
  process msg driver
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
            Logs.warn (fun m -> m "[YASD]: EVAL on %s: Received an invalid reply (wrong body type)" (Selector.to_string selector));
            l
          ) [] rmsgs
      | YErrorInfo e ->
        let errno = Apero.Vle.to_int e in
        Lwt.fail_with @@ Printf.sprintf "[YAS]: EVAL on %s: ErrNo %d" (Selector.to_string selector) errno
      | _ -> Lwt.fail_with @@ Printf.sprintf "[YAS]: EVAL on %s: Received an invalid reply (wrong body type)" (Selector.to_string selector)


