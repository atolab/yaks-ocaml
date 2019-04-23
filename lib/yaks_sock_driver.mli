open Apero
open Yaks_types
open Yaks_sock_types

type t

val create : Apero_net.Locator.t -> t Lwt.t
val destroy : t ->  unit Lwt.t

val process_login: properties -> t -> unit Lwt.t
val process_logout: t -> unit Lwt.t
val process_workspace : Path.t -> t -> wsid Lwt.t 
val process_get : ?quorum:int -> properties -> Selector.t -> t -> ((Path.t * Value.t) list) Lwt.t 
val process_put : ?quorum:int -> properties -> Path.t -> Value.t -> t -> unit Lwt.t 
val process_update : ?quorum:int -> properties -> Path.t ->Value.t -> t -> unit Lwt.t 
val process_remove : ?quorum:int -> properties -> Path.t -> t  -> unit Lwt.t 
val process_subscribe : properties -> ?listener:listener_t -> Selector.t -> t -> subid Lwt.t
val process_unsubscribe :  subid -> t -> unit Lwt.t
val process_register_eval : properties -> ?workpath:Path.t -> Path.t -> eval_callback_t -> t -> unit Lwt.t
val process_unregister_eval : properties -> ?workpath:Path.t -> Path.t -> t -> unit Lwt.t
val process_eval : ?multiplicity:int -> properties -> Selector.t -> t -> ((Path.t * Value.t) list) Lwt.t 

