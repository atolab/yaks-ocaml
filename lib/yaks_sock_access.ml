open Lwt.Infix
open Yaks_types
open Yaks_sock_types

module Access = struct 

  module EvalId = Apero.Uuid
  module SubscriberMap =  Map.Make(SubscriberId)
  module EvalMap = Map.Make(EvalId)

  type state = {
    aid : AccessId.t
  ; path : Path.t
  ; properties : Apero.properties
  ; encoding : Value.encoding option
  ; subscriptions : (listener_t) SubscriberMap.t
  ; evals : (eval_callback_t) EvalMap.t
  ; driver : Yaks_sock_driver.t
  }

  type t = state MVar.t

  let get_id access = 
    MVar.read access >>= fun a -> Lwt.return a.aid

  let create id path properties driver =
    let open Apero.Option.Infix in
    let encoding = Apero.Properties.get Yaks_properties.Access.Key.encoding properties >|= Value.encoding_of_string in
    let acc = {
      aid = id
    ; path = path
    ; properties = properties
    ; encoding = encoding
    ; subscriptions = SubscriberMap.empty
    ; evals = EvalMap.empty
    ; driver
    } in 
    MVar.create acc

  let transcode access v = match access.encoding with
    | Some enc -> (match Value.transcode v enc with
      | Ok v' -> Some v'
      | Error e ->
        let _ = Logs_lwt.warn (fun m -> m "[YA]: Error transcoding value from %s to %s: %s"
          (Value.encoding_to_string @@ Value.encoding v) (Value.encoding_to_string enc) (Yaks_common_errors.show_yerror e))
        in None
      )
    | None -> Some v

  let get selector access = 
    MVar.read access >>= fun access ->
    let _ = Logs_lwt.info (fun m -> m "[YA]: GET on %s" (Selector.to_string selector)) in
    Yaks_sock_driver.process_get selector access.aid access.driver 
    >|= List.map (fun (p,v) ->  (p, transcode access v))                                    (* Try to transcode each value *)
    >|= List.fold_left (fun l (p,o) -> match o with | Some v -> (p,v)::l | None -> l) []    (* Keep only the succesfully transcoded ones *)

  let put path value access =
    MVar.read access >>= fun access ->
    let _ = Logs_lwt.info (fun m -> m "[YA]: PUT on %s -> %s" (Path.to_string path) (Value.to_string value)) in
    Yaks_sock_driver.process_put path access.aid value access.driver

  let delta_put path value access = 
    MVar.read access >>= fun access ->
    let _ = Logs_lwt.info (fun m -> m "[YA]: DELTA_PUT on %s -> %s" (Path.to_string path) (Value.to_string value)) in
    Yaks_sock_driver.process_patch path access.aid value access.driver

  let remove path access =
    MVar.read access >>= fun access ->
    let _ = Logs_lwt.info (fun m -> m "[YA]: REMOVE on %s" (Path.to_string path)) in
    Yaks_sock_driver.process_remove ~delete_type:`Resource  ~path (IdAccess access.aid)  access.driver


  let subscribe ?(listener=(fun _ -> Lwt.return_unit)) selector access =
    MVar.read access >>= fun access ->
    let _ = Logs_lwt.info (fun m -> m "[YA]: SUBSCRIBE on %s" (Selector.to_string selector)) in
    Yaks_sock_driver.process_subscribe ~listener selector access.aid access.driver


  let unsubscribe subscriber_id access= 
    MVar.read access >>= fun access ->
    let _ = Logs_lwt.info (fun m -> m "[YA]: UNSUBSCRIBE ID: %s" (SubscriberId.to_string subscriber_id)) in
    Yaks_sock_driver.process_unsubscribe subscriber_id access.aid access.driver

  let get_subscriptions access = 
    MVar.read access >>= fun access ->
    let _ = Logs_lwt.info (fun m -> m "[YAS]: Getting all subscriptions" ) in 
    Lwt.return (List.map (fun (id, _) -> id) (SubscriberMap.bindings access.subscriptions))

  let eval path f access =
    MVar.guarded access @@ fun access ->
    let _ = Logs_lwt.info (fun m -> m "[YAS]: EVAL on: %s" (Path.to_string path)) in
    let open Lwt.Infix in
    Yaks_sock_driver.process_eval path f access.aid access.driver >>= fun () ->
    MVar.return () {access with evals = EvalMap.add (EvalId.make ()) f access.evals}

end