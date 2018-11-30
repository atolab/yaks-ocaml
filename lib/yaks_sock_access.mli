open Yaks_types
open Yaks_sock_types


module Access : sig 
  type t

  val create : AccessId.t -> Path.t -> Apero.properties -> Yaks_sock_driver.t -> t
  val get : Selector.t -> t -> ((Path.t * Value.t) list) Lwt.t 
  val put : Path.t -> Value.t -> t -> unit Lwt.t 
  val delta_put: Path.t -> Value.t -> t -> unit Lwt.t
  val remove: Path.t -> t  -> unit Lwt.t 
  val subscribe: ?listener:listener_t -> Selector.t -> t -> SubscriberId.t Lwt.t
  val unsubscribe: SubscriberId.t -> t -> unit Lwt.t
  val get_subscriptions : t -> (SubscriberId.t list) Lwt.t
  val eval : Path.t -> eval_callback_t -> t -> unit Lwt.t
  val get_id : t -> AccessId.t Lwt.t
end