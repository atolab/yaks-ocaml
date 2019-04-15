open Apero
open Yaks_types

module MVar = Apero.MVar_lwt

type correlationid = int64
type wsid = string
type subid = string

type listener_t = (Path.t * change) list -> unit Lwt.t
type eval_callback_t = Path.t -> properties -> Value.t Lwt.t


module Message = struct

  type t = Yaks_fe_sock_types.message

  let () = Random.self_init ()

  let make_msg ?corrid mid flags properties payload = 
    (* NOTE: no need to set PROPERTY in flags. If properties is not empty, make_header below add the flag *)
    let corr_id = 
      (match corrid with
       | Some i -> i
       | None -> Random.int64 Int64.max_int
      ) in
    let header = Yaks_fe_sock_types.make_header mid flags corr_id properties in
    let body = payload in
    Yaks_fe_sock_types.make_message header body 

  let make_login props =
    make_msg Yaks_fe_sock_codes.LOGIN [] props Yaks_fe_sock_types.YEmpty

  let make_logout () =
    make_msg Yaks_fe_sock_codes.LOGOUT [] Properties.empty Yaks_fe_sock_types.YEmpty

  let make_workspace path =
    let payload = Yaks_fe_sock_types.YPath path in
    make_msg Yaks_fe_sock_codes.WORKSPACE [] Properties.empty payload

  let make_put ?quorum ws_props path value = 
    ignore quorum;
    let payload = Yaks_fe_sock_types.YPathValueList [(path, value)] in 
    make_msg Yaks_fe_sock_codes.PUT [] ws_props payload

  let make_update  ?quorum ws_props path value = 
    ignore quorum;
    let payload = Yaks_fe_sock_types.YPathValueList [(path, value)] in 
    make_msg Yaks_fe_sock_codes.UPDATE [] ws_props payload

  let make_get ?quorum ws_props selector = 
    ignore quorum;
    let payload = Yaks_fe_sock_types.YSelector selector in 
    make_msg Yaks_fe_sock_codes.GET [] ws_props payload

  let make_remove ?quorum ws_props path = 
    ignore quorum;
    let payload = Yaks_fe_sock_types.YPath path in 
    make_msg Yaks_fe_sock_codes.DELETE [] ws_props payload

  let make_sub ws_props selector = 
    let payload = Yaks_fe_sock_types.YSelector selector in 
    make_msg Yaks_fe_sock_codes.SUB [] ws_props payload

  let make_unsub subscriptionid =
    let payload = Yaks_fe_sock_types.YSubscription subscriptionid in
    make_msg Yaks_fe_sock_codes.UNSUB [] Properties.empty payload
  
  let make_values corrid values = 
    let payload = Yaks_fe_sock_types.YPathValueList values in 
    make_msg ~corrid Yaks_fe_sock_codes.VALUES [] Properties.empty payload

  let make_reg_eval ws_props path =
    let payload = Yaks_fe_sock_types.YPath path in 
    make_msg Yaks_fe_sock_codes.REG_EVAL [] ws_props payload

  let make_unreg_eval ws_props path =
    let payload = Yaks_fe_sock_types.YPath path in 
    make_msg Yaks_fe_sock_codes.UNREG_EVAL [] ws_props payload

  let make_eval ?multiplicity ws_props selector = 
    ignore multiplicity;
    let payload = Yaks_fe_sock_types.YSelector selector in 
    make_msg Yaks_fe_sock_codes.EVAL [] ws_props payload


  let make_ok corrid =
    make_msg ~corrid Yaks_fe_sock_codes.OK [] Properties.empty YEmpty

  let make_error corrid errno = 
    let payload = Yaks_fe_sock_types.YErrorInfo  (Int64.of_int (Yaks_fe_sock_codes.error_code_to_int errno)) in
    make_msg ~corrid Yaks_fe_sock_codes.ERROR [] Properties.empty payload

end