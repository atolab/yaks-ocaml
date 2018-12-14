open Lwt.Infix
open Yaks_types
open Yaks_sock_types
open Yaks_sock_types.Message
open Yaks_fe_sock_codec

module WorkingMap = Map.Make(Apero.Vle)
module ListenersMap = Map.Make(SubscriberId)
module EvalsMap = Map.Make(Path)

type state = {
  sock : Lwt_unix.file_descr
; working_set : Yaks_fe_sock_types.message Lwt.u WorkingMap.t
; subscribers : listener_t ListenersMap.t
; evals : eval_callback_t EvalsMap.t
}

type t = state MVar.t 

let max_size = 64 * 1024


let check_socket sock = 
  let _ =
    match Lwt_unix.state sock with
    | Opened -> ignore @@ Logs_lwt.info (fun m -> m "Socket is open")
    | Closed -> ignore @@ Logs_lwt.info (fun m -> m "Socket is closed")
    | Aborted e -> ignore @@ Logs_lwt.info (fun m -> m "Socket is aborted: %s" (Printexc.to_string e))
  in ()

let send_to_socket msg sock = 
  let open Apero in
  let buf = IOBuf.create max_size in
  let concat bl bd =
    let open Apero.Result.Infix in
    let data = IOBuf.create @@ (IOBuf.limit bl) + (IOBuf.limit bd) in
    IOBuf.put_buf bl data >>= IOBuf.put_buf bd
  in
  match Yaks_fe_sock_codec.encode_message msg buf with
  | Ok buf ->
    let lbuf = IOBuf.create 16 in
    let fbuf = IOBuf.flip buf in
    (match encode_vle (Vle.of_int @@ IOBuf.limit fbuf) lbuf with
     | Ok lbuf ->
       let%lwt _ = Logs_lwt.debug (fun m -> m "Sending message to socket") in
       let _ = check_socket in
       let lbuf = IOBuf.flip lbuf in
       let data = IOBuf.flip @@ Apero.Result.get @@ concat lbuf fbuf in 
       Net.write_all sock data >>= fun bs ->
       let%lwt _ = Logs_lwt.debug (fun m -> m "Sended %d bytes" bs) in
       Lwt.return_unit
     | Error e -> 
       let%lwt _ = Logs_lwt.err (fun m -> m "Falied in writing message: %s" (Apero.show_error e)) in
       Lwt.fail @@ Exception e )
  | Error e -> 
    let%lwt _ = Logs_lwt.err (fun m -> m "Falied in encoding messge: %s" (Apero.show_error e)) in
    Lwt.fail @@ Exception e


let process_get_on_evals selector (driver:t) =
  let open Apero in
  MVar.read driver >>= fun self ->
  match Selector.properties selector with
  | None -> Lwt.fail_with @@ Printf.sprintf "[YAS]: Invalid selector (without properties) for a get on Evals: %s" (Selector.to_string selector)
  | Some props ->
      let params = Properties.of_string ~prop_sep:"&" props in
      let matching_evals = EvalsMap.filter (fun path _ -> Selector.is_matching_path path selector) self.evals in
      EvalsMap.fold (fun path eval l -> (path,eval)::l) matching_evals [] |>
      Lwt_list.map_p (fun (path, eval) -> eval path params >>= fun result -> Lwt.return (path, result) )

let receiver_loop (driver:t) = 
  let open Apero in
  MVar.read driver >>= fun self ->
  let lbuf = IOBuf.create 16 in
  let%lwt len = Net.read_vle self.sock lbuf in
  let%lwt _ = Logs_lwt.debug (fun m -> m "Message lenght : %d" (Vle.to_int len)) in
  let buf = IOBuf.create (Vle.to_int len) in
  let%lwt n = Net.read_all self.sock buf in
  let _ = check_socket self.sock
  in
  let%lwt _ = Logs_lwt.debug (fun m -> m "Read %d bytes out of the socket" n) in
  match decode_message buf with
  | Ok (msg, _) -> 
    (match (msg.header.mid, msg.body) with
    | (NOTIFY, YNotification (sid, data)) ->
      MVar.read driver >>= fun self ->
      (match ListenersMap.find_opt (SubscriberId.of_string sid) self.subscribers with
      | Some cb -> cb data
      | None ->  
        let%lwt _ = Logs_lwt.debug (fun m -> m "Received notification with unknown subscriberid %s" sid) in
        Lwt.return_unit)
    | (GET, YSelector s) ->
      process_get_on_evals s driver >>= fun results ->
      make_values msg.header.corr_id results >>= fun rmsg ->
      send_to_socket rmsg self.sock
    | (_, _) -> MVar.guarded driver @@ (fun self ->
      (match WorkingMap.find_opt msg.header.corr_id self.working_set with 
      | Some resolver ->  let _ = Lwt.wakeup_later resolver msg in
        MVar.return () {self with working_set = WorkingMap.remove msg.header.corr_id self.working_set}
      | None -> let%lwt _ = Logs_lwt.debug (fun m -> m "Received message with unknown correlation id %d" @@ Vle.to_int msg.header.corr_id ) in
        MVar.return () self)
      ))
  | Error e -> 
    let%lwt _ = Logs_lwt.err (fun m -> m "Failed in parsing message %s" (Apero.show_error e)) in
    Lwt.fail @@ Exception e

let rec loop driver  () =
  receiver_loop driver >>= loop driver

let create locator = 
  let sock = Lwt_unix.socket PF_INET SOCK_STREAM 0 in
  Apero_net.connect sock locator >>= fun s ->
  let driver = MVar.create {sock = s; working_set = WorkingMap.empty; subscribers = ListenersMap.empty; evals = EvalsMap.empty } in
  let _ = loop driver () in
  Lwt.return driver

let destroy driver =
  MVar.read driver >>= fun d ->
  Apero_net.safe_close d.sock

let process (msg:Yaks_fe_sock_types.message) driver = 
  MVar.guarded driver @@
  fun self ->
  let promise, completer = Lwt.wait () in
  send_to_socket msg self.sock
  >>= fun _ ->
  MVar.return_lwt promise {self with working_set = WorkingMap.add msg.header.corr_id completer self.working_set}

let check_reply_ok corr_id (replymsg:Yaks_fe_sock_types.message) =
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


let process_get selector accessid (driver:t) =
  let _ = Logs_lwt.info (fun m -> m "[YASD]: GET on %s" (Selector.to_string selector)) in
  make_get (IdAccess accessid) selector
  >>= fun msg -> process msg driver
  >>= fun rmsg ->
  if rmsg.header.corr_id <> msg.header.corr_id then
    Lwt.fail_with "Correlation Id is different!"
  else
    match rmsg.body with
    | YPathValueList l -> Lwt.return l
    | YErrorInfo e ->
      let errno = Apero.Vle.to_int e in
      Lwt.fail_with @@ Printf.sprintf "[YAS]: GET ErrNo: %d" errno
    | _ -> Lwt.fail_with "Message body is wrong"

let process_put path accessid value (driver:t) =
  let _ = Logs_lwt.info (fun m -> m "[YASD]: PUT on %s -> %s" (Path.to_string path) (Value.to_string value)) in
  make_put (IdAccess accessid) path value
  >>= fun msg -> process msg driver
  >>= check_reply_ok msg.header.corr_id

let process_patch path accessid value (driver:t) =
  let _ = Logs_lwt.info (fun m -> m "[YASD]: PUT on %s -> %s" (Path.to_string path) (Value.to_string value)) in
  make_put (IdAccess accessid) path value
  >>= fun msg -> process msg driver
  >>= check_reply_ok msg.header.corr_id

let process_remove ?(delete_type=`Resource) ?(path) deleteid (driver:t) =
  let _ = Logs_lwt.info (fun m -> m "[YASD]: REMOVE") in
  match path with
  | None ->  make_delete ~delete_type deleteid
    >>= fun msg -> process msg driver
    >>= check_reply_ok msg.header.corr_id
  | Some p ->
    MVar.read driver >>= fun self ->
    (match EvalsMap.find_opt p self.evals with
    | Some _ ->
      let _ = Logs_lwt.info (fun m -> m "[YASD]: REMOVE Eval %s" (Path.to_string p)) in
      MVar.guarded driver @@ fun self ->
      MVar.return () {self with evals = EvalsMap.remove p self.evals}
    | None -> Lwt.return_unit)
    >>= fun () ->
    let _ = Logs_lwt.info (fun m -> m "[YASD]: REMOVE %s" (Path.to_string p)) in
    make_delete ~delete_type ~path:p deleteid
    >>= fun msg -> process msg driver
    >>= check_reply_ok msg.header.corr_id
   

let process_subscribe ?(listener=fun _ -> Lwt.return_unit) selector accessid (driver:t) =
  let _ = Logs_lwt.info (fun m -> m "[YASD]: SUB on %s" (Selector.to_string selector)) in
  make_sub (IdAccess accessid) selector
  >>= fun msg ->  process msg driver
  >>= fun rmsg ->
  if rmsg.header.corr_id <> msg.header.corr_id then
    Lwt.fail_with "Correlation Id is different!"
  else
    let sid = Apero.Properties.find Yaks_properties.Access.Key.subscription_id rmsg.header.properties in
    let subid = SubscriberId.of_string sid in
    MVar.guarded driver @@ fun self ->
    MVar.return subid {self with subscribers = ListenersMap.add subid listener self.subscribers}

let process_unsubscribe subid accessid (driver:t) =
  let _ = Logs_lwt.info (fun m -> m "[YASD]: UNSUB on %s" (SubscriberId.to_string subid) ) in
  make_unsub (IdAccess accessid) (IdSubscription subid)
  >>= fun msg ->  process msg driver
  >>= check_reply_ok msg.header.corr_id
  >>= fun () ->
    MVar.guarded driver @@ fun self ->
    MVar.return () {self with subscribers = ListenersMap.remove subid self.subscribers}

let process_eval path eval_callback accessid (driver:t) =
  let _ = Logs_lwt.info (fun m -> m "[YASD]: EVAL on %s" (Path.to_string path)) in
  make_eval (IdAccess accessid) path
  >>= fun msg ->  process msg driver
  >>= check_reply_ok msg.header.corr_id
  >>= fun () ->
    MVar.guarded driver @@ fun self ->
    MVar.return () {self with evals = EvalsMap.add path eval_callback self.evals}
