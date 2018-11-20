open Yaks_common_types
open Yaks_sock_types

type t

val create : Apero_net.Locator.t -> t Lwt.t
val destroy : t ->  unit Lwt.t

val process_get : Selector.t -> AccessId.t -> t -> ((Path.t * Value.t) list) Lwt.t 
val process_put : Path.t -> AccessId.t -> Value.t -> t -> unit Lwt.t 
val process_patch : Path.t -> AccessId.t ->Value.t -> t -> unit Lwt.t 
val process_remove : ?delete_type:entity_type -> ?selector:Selector.t -> id -> t  -> unit Lwt.t 
val process_subscribe : ?listener:listener_t -> Selector.t -> AccessId.t -> t -> SubscriberId.t Lwt.t
val process_unsubscribe :  SubscriberId.t -> AccessId.t -> t -> unit Lwt.t
val process_eval : Path.t -> eval_callback_t -> AccessId.t -> t -> unit Lwt.t

val process : Yaks_fe_sock_types.message -> t -> Yaks_fe_sock_types.message Lwt.t

