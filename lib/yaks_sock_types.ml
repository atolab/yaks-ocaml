open Apero
open Yaks_types

module MVar = Apero.MVar_lwt

type correlationid = int64
type wsid = string
type subid = string

type listener_t = (Path.t * Value.t) list -> unit Lwt.t
type eval_callback_t = Path.t -> properties -> Value.t Lwt.t

let default_write_quorum = 0  (* meaning reply as soon as 1st Yaks acknowledges *)
let default_read_quorum = 0   (* meaning reply as soon as 1st Yaks get 1 answer *)

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

  let make_login props =
    make_msg Yaks_fe_sock_codes.LOGIN [] props Yaks_fe_sock_types.YEmpty

  let make_logout () =
    make_msg Yaks_fe_sock_codes.LOGOUT [] Properties.empty Yaks_fe_sock_types.YEmpty

  let make_workspace path =
    let payload = Yaks_fe_sock_types.YPath path in
    make_msg Yaks_fe_sock_codes.WORKSPACE [] Properties.empty payload

  let make_put ?(quorum=default_write_quorum) ?workspace path value = 
    ignore quorum;
    let properties = match workspace with
      | Some wsid -> Properties.singleton Yaks_properties.Admin.workspaceid wsid
      | None -> Properties.empty 
    in
    let payload = Yaks_fe_sock_types.YPathValueList [(path, value)] in 
    make_msg Yaks_fe_sock_codes.PUT [] properties payload

  let make_update  ?(quorum=default_write_quorum) ?workspace path value = 
    ignore quorum;
    let properties = match workspace with
      | Some wsid -> Properties.singleton Yaks_properties.Admin.workspaceid wsid
      | None -> Properties.empty 
    in
    let payload = Yaks_fe_sock_types.YPathValueList [(path, value)] in 
    make_msg Yaks_fe_sock_codes.UPDATE [] properties payload

  let make_get ?(quorum=default_read_quorum) ?workspace selector = 
    ignore quorum;
    let properties = match workspace with
      | Some wsid -> Properties.singleton Yaks_properties.Admin.workspaceid wsid
      | None -> Properties.empty 
    in
    let payload = Yaks_fe_sock_types.YSelector selector in 
    make_msg Yaks_fe_sock_codes.GET [] properties payload

  let make_remove ?(quorum=default_write_quorum) ?workspace path = 
    ignore quorum;
    let properties = match workspace with
      | Some wsid -> Properties.singleton Yaks_properties.Admin.workspaceid wsid
      | None -> Properties.empty 
    in
    let payload = Yaks_fe_sock_types.YPath path in 
    make_msg Yaks_fe_sock_codes.DELETE [] properties payload

  let make_sub ?workspace selector = 
    let properties = match workspace with
      | Some wsid -> Properties.singleton Yaks_properties.Admin.workspaceid wsid
      | None -> Properties.empty 
    in
    let payload = Yaks_fe_sock_types.YSelector selector in 
    make_msg Yaks_fe_sock_codes.SUB [] properties payload

  let make_unsub subscriptionid =
    let payload = Yaks_fe_sock_types.YSubscription subscriptionid in
    make_msg Yaks_fe_sock_codes.UNSUB [] Properties.empty payload
  
  let make_values corrid values = 
    let payload = Yaks_fe_sock_types.YPathValueList values in 
    make_msg ~corrid Yaks_fe_sock_codes.VALUES [] Properties.empty payload

  let make_eval ?workspace path =
    let properties = match workspace with
      | Some wsid -> Properties.singleton Yaks_properties.Admin.workspaceid wsid
      | None -> Properties.empty 
    in
    let payload = Yaks_fe_sock_types.YPath path in 
    make_msg Yaks_fe_sock_codes.EVAL [] properties payload


  let make_ok corrid =
    make_msg ~corrid Yaks_fe_sock_codes.OK [] Properties.empty YEmpty

  let make_error corrid errno = 
    let payload = Yaks_fe_sock_types.YErrorInfo  (Int64.of_int (Yaks_fe_sock_codes.error_code_to_int errno)) in
    make_msg ~corrid Yaks_fe_sock_codes.ERROR [] Properties.empty payload

end