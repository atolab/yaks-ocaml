open Apero
open Yaks_types

module MVar : Apero.MVar

type correlationid = int64
type wsid = string
type subid = string

type listener_t = (Path.t * change) list -> unit Lwt.t
type eval_callback_t = Path.t -> properties -> Value.t Lwt.t

module Message : sig 
  type t

  val make_msg : ?corrid:int64 -> Yaks_fe_sock_codes.message_id -> Yaks_fe_sock_codes.message_flags list -> Apero.properties -> Yaks_fe_sock_types.payload -> Yaks_fe_sock_types.message

  val make_login : properties -> Yaks_fe_sock_types.message
  val make_logout : unit -> Yaks_fe_sock_types.message
  val make_workspace : Path.t -> Yaks_fe_sock_types.message
  val make_put : ?quorum:int -> properties -> Path.t -> Value.t -> Yaks_fe_sock_types.message
  val make_update : ?quorum:int -> properties -> Path.t -> Value.t -> Yaks_fe_sock_types.message
  val make_get : ?quorum:int -> properties -> Selector.t -> Yaks_fe_sock_types.message
  val make_remove : ?quorum:int -> properties -> Path.t -> Yaks_fe_sock_types.message
  val make_sub : properties -> Selector.t -> Yaks_fe_sock_types.message
  val make_unsub : subid -> Yaks_fe_sock_types.message
  val make_reg_eval : properties -> Path.t -> Yaks_fe_sock_types.message
  val make_unreg_eval : properties -> Path.t -> Yaks_fe_sock_types.message
  val make_eval : ?multiplicity:int -> properties -> Selector.t -> Yaks_fe_sock_types.message
  val make_values : correlationid -> (Path.t * Value.t) list -> Yaks_fe_sock_types.message
  val make_ok : correlationid -> Yaks_fe_sock_types.message
  val make_error : correlationid -> Yaks_fe_sock_codes.error_code -> Yaks_fe_sock_types.message
end
