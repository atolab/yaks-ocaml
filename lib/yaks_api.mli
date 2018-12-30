open Apero
open Yaks_types

type yid = string
(** Yaks service identifier (unique in the system) *)
type feid = string
(** Frontend identifier (must be unique per Yaks service) *)
type beid = string
(** Backend identifier (must be unique per Yaks service) *)
type stid = string
(** Storage identifier (must be unique per Yaks service) *)
type sid = string
(** Session identifier (unique per Yaks service) *)
type subid = string
(** Subscriber identifier (unique per Yaks service) *)


type listener_t = (Path.t * Value.t) list -> unit Lwt.t

type eval_callback_t = Path.t -> properties -> Value.t Lwt.t

module Workspace : sig
  type t    

  val get : ?quorum:int -> Selector.t -> t -> (Path.t * Value.t) list Lwt.t
  val put : ?quorum:int -> Path.t -> Value.t -> t -> unit Lwt.t 
  val update: ?quorum:int ->Path.t -> Value.t -> t -> unit Lwt.t
  val remove: ?quorum:int -> Path.t -> t  -> unit Lwt.t 

  val subscribe: ?listener:listener_t -> Selector.t -> t -> subid Lwt.t
  val unsubscribe: subid -> t -> unit Lwt.t

  val eval : Path.t -> eval_callback_t -> t -> unit Lwt.t
end

module Admin : sig
  type t

  val add_frontend : ?yaks:yid -> feid -> properties -> t -> unit Lwt.t
  val get_frontends : ?yaks:yid -> t -> (feid * properties) list Lwt.t
  val get_frontend : ?yaks:yid -> feid -> t -> properties option Lwt.t
  val remove_frontend : ?yaks:yid -> feid -> t -> unit Lwt.t

  val add_backend : ?yaks:yid -> beid -> properties -> t -> unit Lwt.t
  val get_backends : ?yaks:yid -> t -> (beid * properties) list Lwt.t
  val get_backend : ?yaks:yid -> beid -> t -> properties option Lwt.t
  val remove_backend : ?yaks:yid -> beid -> t -> unit Lwt.t

  val add_storage : ?yaks:yid -> stid -> ?backend:beid -> properties -> t -> unit Lwt.t
  val get_storages : ?yaks:yid -> ?backend:beid -> t -> (stid * properties) list Lwt.t
  val get_storage : ?yaks:yid -> stid -> t -> properties option Lwt.t
  val remove_storage : ?yaks:yid -> stid -> t -> unit Lwt.t

  val get_sessions : ?yaks:yid -> ?frontend:feid -> t -> (sid * properties) list Lwt.t
  val close_session : ?yaks:yid -> sid -> t -> unit Lwt.t

  val get_subscriptions : ?yaks:yid -> sid -> t -> Selector.t list Lwt.t
end

(* module Yaks : sig *)
  type t

  val login : Apero_net.Locator.t -> properties -> t Lwt.t
  val logout : t -> unit Lwt.t

  val get_id : t -> yid
  val workspace : Path.t -> t -> Workspace.t Lwt.t
  val admin : t -> Admin.t Lwt.t
(* end *)

