open Lwt.Infix
open Yaks_common_types
open Yaks_sock_types

module Access = struct 

  module EvalId = Apero.Uuid
  module SubscriberMap =  Map.Make(SubscriberId)
  module EvalMap = Map.Make(EvalId)

  type state = {
    subscriptions : (listener_t) SubscriberMap.t
  ; encoding : Yaks_fe_sock_codes.value_encoding
  ; path : Path.t
  ; cache_size : int
  ; evals : (eval_callback_t) EvalMap.t
  ; aid : AccessId.t
  ; driver : Yaks_sock_driver.t
  }

  type t = state MVar.t

  let get_id access = 
    MVar.read access >>= fun a -> Lwt.return a.aid

  let create cache_size encoding path id driver = 
    let acc = {
      aid = id
    ; path = path
    ; cache_size = cache_size
    ; encoding = encoding
    ; subscriptions = SubscriberMap.empty
    ; evals = EvalMap.empty
    ; driver
    } in 
    MVar.create acc

  let get selector access = 
    MVar.read access >>= fun access ->
    let _ = Logs_lwt.info (fun m -> m "[YA]: GET on %s" (Selector.to_string selector)) in
    Yaks_sock_driver.process_get selector access.aid access.driver 
    >>= fun d -> 
    let enc = ( Value.encoding_of_string (Yaks_fe_sock_codes.value_encoding_to_string access.encoding)) in
    Lwt.return d
    >>= Lwt_list.map_p (fun (p,v) ->  Lwt.return @@ (p,Apero.Result.get @@ Value.transcode v enc)) 

  let put path value access =
    MVar.read access >>= fun access ->
    let _ = ignore @@ Logs_lwt.info (fun m -> m "[YA]: PUT on %s -> %s" (Path.to_string path) (Value.to_string value)) in
    Yaks_sock_driver.process_put path access.aid value access.driver

  let delta_put path value access = 
    MVar.read access >>= fun access ->
    let _ = Logs_lwt.info (fun m -> m "[YA]: DELTA_PUT on %s -> %s" (Path.to_string path) (Value.to_string value)) in
    Yaks_sock_driver.process_patch path access.aid value access.driver

  let remove selector access =
    MVar.read access >>= fun access ->
    let _ = Logs_lwt.info (fun m -> m "[YA]: REMOVE on %s" (Selector.to_string selector)) in
    Yaks_sock_driver.process_remove ~delete_type:`Resource  ~selector (IdAccess access.aid)  access.driver


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
    let _ = ignore @@ Logs_lwt.info (fun m -> m "[YAS]: Getting all subscriptions" ) in 
    Lwt.return (List.map (fun (id, _) -> id) (SubscriberMap.bindings access.subscriptions))

  let eval path f access =
    MVar.guarded access @@ fun access ->
    let _ = ignore @@ Logs_lwt.info (fun m -> m "[YAS]: EVAL on: %s" (Path.to_string path)) in
    let new_access = {access with evals = EvalMap.add (EvalId.make ()) f access.evals} in  
    MVar.return () new_access
end