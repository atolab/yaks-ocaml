open Apero
open Yaks_types

module MVar = Apero.MVar_lwt

type listener_t = (Path.t * Value.t) list -> unit Lwt.t
type eval_callback_t = Path.t -> properties -> Value.t Lwt.t

type entity_type = [
  | `Access
  | `Storage
  | `Resource
]
type id =
  | IdAccess of AccessId.t
  | IdStorage of StorageId.t
  | IdSubscription of SubscriberId.t

module Message = struct

  type t = Yaks_fe_sock_types.message


  let make_msg ?corrid mid flags properties payload = 
    (* NOTE: no need to set PROPERTY in flags. If properties is not empty, make_header below add the flag *)
    let corr_id = 
      (match corrid with
       | Some i -> i
       | None -> 
         Random.self_init (); 
         Random.int64 Int64.max_int
      ) in
    let header = Yaks_fe_sock_types.make_header mid flags corr_id properties in
    let body = payload in
    Lwt.return @@ Yaks_fe_sock_types.make_message header body 



  let make_open ?username ?password () =
    match (username,password) with
    | (None, None) ->  
      let mid = Yaks_fe_sock_codes.OPEN in
      make_msg mid [] Properties.empty Yaks_fe_sock_types.YEmpty
    | _ , _ -> Lwt.fail_with "Not yet..."

  let make_create t p properties =
    let mid = Yaks_fe_sock_codes.CREATE in
    let payload = Yaks_fe_sock_types.YPath p in
    match t with
    | `Access -> 
      let flags = [Yaks_fe_sock_codes.ACCESS] in 
      make_msg mid flags properties payload 
    | `Storage -> 
      let flags = [Yaks_fe_sock_codes.STORAGE] in 
      make_msg mid flags properties payload
    | _ -> Lwt.fail_with "Entity Create Not supported!!"


  let make_delete ?(delete_type=`Resource) ?path id =
    let mid = Yaks_fe_sock_codes.DELETE in
    match delete_type with 
    | `Access -> 
      (match id with 
       | IdAccess aid ->   
         let properties = Properties.add Yaks_properties.Access.Key.id (AccessId.to_string aid) Properties.empty in
         make_msg mid [Yaks_fe_sock_codes.ACCESS; Yaks_fe_sock_codes.PROPERTY] properties YEmpty
       | _ -> Lwt.fail_with "Wrong id"
      )

    | `Storage -> 
      (match id with 
       | IdStorage sid ->   
         let properties = Properties.add Yaks_properties.Storage.Key.id (StorageId.to_string sid) Properties.empty in
         make_msg mid [Yaks_fe_sock_codes.STORAGE; Yaks_fe_sock_codes.PROPERTY] properties YEmpty
       | _ -> Lwt.fail_with "Wrong id"
      )
    | `Resource -> 
      (match id with 
       | IdAccess aid ->   
         let properties = Properties.add Yaks_properties.Access.Key.id (AccessId.to_string aid) Properties.empty in
         let payload = Yaks_fe_sock_types.YPath (Apero.Option.get path) in
         make_msg mid [Yaks_fe_sock_codes.PROPERTY] properties payload
       | _ -> Lwt.fail_with "Wrong id"
      )

  let make_put ?(encoding=Yaks_fe_sock_codes.RAW) id path value = 
    ignore @@ encoding;
    let mid  = Yaks_fe_sock_codes.PUT in
    match id with
    | IdAccess id -> 
      let payload = Yaks_fe_sock_types.YPathValueList [(path, value)] in 
      let properties = Properties.add Yaks_properties.Access.Key.id (AccessId.to_string id) Properties.empty in
      make_msg mid [Yaks_fe_sock_codes.PROPERTY] properties payload
    | _ -> Lwt.fail_with "Wrong id"


  let make_patch ?(encoding=Yaks_fe_sock_codes.RAW) id path value = 
    ignore @@ encoding;
    let mid  = Yaks_fe_sock_codes.PATCH in
    match id with
    | IdAccess id -> 
      let payload = Yaks_fe_sock_types.YPathValueList [(path, value)] in 
      let properties = Properties.add Yaks_properties.Access.Key.id (AccessId.to_string id) Properties.empty in
      make_msg mid [Yaks_fe_sock_codes.PROPERTY] properties payload
    | _ -> Lwt.fail_with "Wrong id"

  let make_get ?(encoding=Yaks_fe_sock_codes.RAW) id selector = 
    ignore @@ encoding;
    let mid  = Yaks_fe_sock_codes.GET in
    match id with
    | IdAccess id -> 
      let payload = Yaks_fe_sock_types.YSelector selector in 
      let properties = Properties.add Yaks_properties.Access.Key.id (AccessId.to_string id) Properties.empty in
      make_msg mid [Yaks_fe_sock_codes.PROPERTY] properties payload
    | _ -> Lwt.fail_with "Wrong id"

  let make_sub ?(encoding=Yaks_fe_sock_codes.RAW) id selector = 
    ignore @@ encoding;
    let mid  = Yaks_fe_sock_codes.SUB in
    match id with
    | IdAccess id -> 
      let payload = Yaks_fe_sock_types.YSelector selector in 
      let properties = Properties.add Yaks_properties.Access.Key.id (AccessId.to_string id) Properties.empty in
      make_msg mid [Yaks_fe_sock_codes.PROPERTY] properties payload
    | _ -> Lwt.fail_with "Wrong id"

  let make_unsub accessid subscriptionid = 
    let mid = Yaks_fe_sock_codes.UNSUB in 
    match accessid with
    | IdAccess aid -> 
      (match subscriptionid with
       | IdSubscription sid -> 
         let properties = Properties.add Yaks_properties.Access.Key.id (AccessId.to_string aid) Properties.empty in
         let payload = Yaks_fe_sock_types.YSubscription (SubscriberId.to_string sid) in 
         make_msg mid [Yaks_fe_sock_codes.PROPERTY] properties payload
       | _ -> Lwt.fail_with "Wrong id"
      )
    | _ -> Lwt.fail_with "Wrong id"

  
  let make_values corrid values = 
    let mid = Yaks_fe_sock_codes.VALUES in
    let payload = Yaks_fe_sock_types.YPathValueList values in 
    make_msg ~corrid mid [] Properties.empty payload

  let make_eval id path =
    let mid = Yaks_fe_sock_codes.EVAL in
    match id with
    | IdAccess id -> 
      let properties = Properties.add Yaks_properties.Access.Key.id (AccessId.to_string id) Properties.empty in
      let payload = Yaks_fe_sock_types.YPath path in 
      make_msg mid [Yaks_fe_sock_codes.PROPERTY] properties payload
    | _ -> Lwt.fail_with "Wrong id"


  let make_ok accessid corrid =
    let mid = Yaks_fe_sock_codes.OK in
    match accessid with
    | IdAccess id ->
      let properties = Properties.add Yaks_properties.Access.Key.id (AccessId.to_string id) Properties.empty in
      make_msg ~corrid mid [Yaks_fe_sock_codes.PROPERTY] properties YEmpty
    | _ -> Lwt.fail_with "Wrong id"


  let make_error accessid corrid errno =
    let mid = Yaks_fe_sock_codes.ERROR in
    match accessid with
    | IdAccess id ->
      let properties = Properties.add Yaks_properties.Access.Key.id (AccessId.to_string id) Properties.empty in
      make_msg ~corrid mid [Yaks_fe_sock_codes.PROPERTY] properties (Yaks_fe_sock_types.YErrorInfo  (Int64.of_int (Yaks_fe_sock_codes.error_code_to_int errno)) )
    | _ -> Lwt.fail_with "Wrong id"
end