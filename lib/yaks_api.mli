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
(** Callback function implemented by the listener *)

type eval_callback_t = Path.t -> properties -> Value.t Lwt.t
(** Callback function registered as an eval *)

type transcoding_fallback = Fail | Drop | Keep
(** Action to perform in [get] operation when the transcoding of a received [Value] into the requested encoding fails.
    [Fail] means the [get] operation will return an error.
    [Drop] means the [get] operation will drop from the returned result the values which can't be transcoded.
    [Keep] means the [get] operation will return the values which can't be transcoded with their original encoding. *)


module Workspace : sig
  type t    

  val get : ?quorum:int -> ?encoding:Value.encoding -> ?fallback:transcoding_fallback -> Selector.t -> t -> (Path.t * Value.t) list Lwt.t
  (** [get quorum encoding fallback s w] requests Yaks to return a list of the stored paths/values where all the paths match the selector [s].
      [s] can be absolute or relative to the workspace [w].
      The [quorum] (default value is 1) is used by Yaks to decide for each matching path the number of answer from storages to wait before returning the associated value.
      The [encoding] indicates the expected encoding of the resulting values. If the original values have a different encoding, Yaks will try to transcode them into the expected encoding.
      By default, if no encoding is specified, the vaules are returned with their original encoding.
      The [fallback] indicates the action that Yakss will perform if the transcoding of a value fails. *)

  val put : ?quorum:int -> Path.t -> Value.t -> t -> unit Lwt.t
  (** [put quorum p v w] publishes a path [p] with an associated value [v] on Yaks.
      [p] can be absolute or relative to the workspace [w].
      The [quorum] (default value is 1) is used by Yaks to wait for a certain number of acknowledgments from Yaks' storages that should store the path/value. *)

  val update: ?quorum:int -> Path.t -> Value.t -> t -> unit Lwt.t
  (** [update quorum p v w] publishes a path [p] with an associated delta-value [v] on Yaks.
      [p] can be absolute or relative to the workspace [w].
      The [quorum] (default value is 1) is used by Yaks to wait for a certain number of acknowledgments from Yaks' storages that should store the path/delta-value. *)

  val remove: ?quorum:int -> Path.t -> t  -> unit Lwt.t
  (** [remove quorum p w] removes from Yaks' storages the path [p] and the associated value. 
      [p] can be absolute or relative to the workspace [w].
      The [quorum] (default value is 1) is used by Yaks to wait for a certain number of acknowledgments from Yaks' storages that should remove the path/value. *)

  val subscribe: ?listener:listener_t -> Selector.t -> t -> subid Lwt.t
  (** [subscribe listener s w] register a subscription to all publications matching the selector [s]. A subscription identifier is returned.
      [s] can be absolute or relative to the workspace [w].
      If specified, the [listener] callback will be called with the published path/value *)

  val unsubscribe: subid -> t -> unit Lwt.t
  (** [unsubscribe subid w] unregisters a previous subscription with the identifier [subid] *)

  val register_eval : Path.t -> eval_callback_t -> t -> unit Lwt.t
  (** [register_eval p e w] registers an evaluation function [e] under the path [p].
      [p] can be absolute or relative to the workspace [w]. *)

  val unregister_eval : Path.t -> t  -> unit Lwt.t
  (** [register_eval p w] unregisters an previously registered evaluation function under the path [p].
      [p] can be absolute or relative to the workspace [w]. *)

  val eval : ?multiplicity:int -> ?encoding:Value.encoding -> ?fallback:transcoding_fallback -> Selector.t -> t -> (Path.t * Value.t) list Lwt.t
  (** [eval multiplicity encoding fallback s w] calls all the evaluation functions registered with a path matching the selector [s].
      If several evaluation function are registerd with the same path (by different Yaks clients), Yaks will call N functions where N=[multiplicity] (default value is 1).
      Note that in such case, the returned path/value list will contain N time each path with the different values returned by the different dunction calls.
      The [encoding] indicates the expected encoding of the resulting values. If the original values have a different encoding, Yaks will try to transcode them into the expected encoding.
      By default, if no encoding is specified, the vaules are returned with their original encoding.
      The [fallback] indicates the action that Yakss will perform if the transcoding of a value fails. *)

end

module Admin : sig
  type t

  val add_frontend : ?yaks:yid -> feid -> properties -> t -> unit Lwt.t
  (** [add_frontend yaks feid p a] adds in the Yaks service with identifier [yaks] (by default it will be the service this API is connected to)
      a frontend with [feid] as identifier and [p] as properties. *)
  
  val get_frontends : ?yaks:yid -> t -> (feid * properties) list Lwt.t
  (** [get_frontends yaks a] returns the caracteristics (identifier and properties) of all the frontends of the Yaks service with identifier [yaks]
      (by default it will be the service this API is connected to). *)

  val get_frontend : ?yaks:yid -> feid -> t -> properties option Lwt.t
  (** [get_frontend yaks feid a] returns the properties of the frontend with identifier [feid] of the Yaks service with identifier [yaks]
      (by default it will be the service this API is connected to). *)

  val remove_frontend : ?yaks:yid -> feid -> t -> unit Lwt.t
  (** [remove_frontend yaks feid a] removes the frontend with identifier [feid] from the Yaks service with identifier [yaks]
      (by default it will be the service this API is connected to). *)

  val add_backend : ?yaks:yid -> beid -> properties -> t -> unit Lwt.t
  (** [add_backend yaks beid p a] adds in the Yaks service with identifier [yaks] (by default it will be the service this API is connected to)
      a backend with [beid] as identifier and [p] as properties. *)

  val get_backends : ?yaks:yid -> t -> (beid * properties) list Lwt.t
  (** [get_backends yaks a] returns the caracteristics (identifier and properties) of all the backends of the Yaks service with identifier [yaks]
      (by default it will be the service this API is connected to). *)

  val get_backend : ?yaks:yid -> beid -> t -> properties option Lwt.t
  (** [get_backend yaks beid a] returns the properties of the backend with identifier [beid] of the Yaks service with identifier [yaks]
      (by default it will be the service this API is connected to). *)

 val remove_backend : ?yaks:yid -> beid -> t -> unit Lwt.t
 (** [remove_backend yaks beid a] removes the backend with identifier [beid] from the Yaks service with identifier [yaks]
      (by default it will be the service this API is connected to). *)

  val add_storage : ?yaks:yid -> stid -> ?backend:beid -> properties -> t -> unit Lwt.t
  (** [add_storage yaks stid beid p a] adds in the Yaks service with identifier [yaks] (by default it will be the service this API is connected to)
      and using the backend with identifier [beid] a storage with [stid] as identifier and [p] as properties.
      If [beid] is not specified, Yaks will automatically find a backend that can handle the specified properties. *)

  val get_storages : ?yaks:yid -> ?backend:beid -> t -> (stid * properties) list Lwt.t
  (** [get_storages yaks beid a] returns the caracteristics (identifier and properties) of all the storages of the Yaks service with identifier [yaks]
      (by default it will be the service this API is connected to) and that are managed by the backend with identifier [beid].
      If [beid] is not specified, all the storages caracterisitics are returned, whatever their backend. *)

  val get_storage : ?yaks:yid -> stid -> t -> properties option Lwt.t
  (** [get_storage yaks stid a] returns the properties of the storage with identifier [stid] of the Yaks service with identifier [yaks]
      (by default it will be the service this API is connected to). *)

  val remove_storage : ?yaks:yid -> stid -> t -> unit Lwt.t
  (** [remove_storage yaks stid a] removes the storage with identifier [stid] from the Yaks service with identifier [yaks]
      (by default it will be the service this API is connected to). *)

  val get_sessions : ?yaks:yid -> ?frontend:feid -> t -> (sid * properties) list Lwt.t
  (** [get_sessions yaks feid a] returns the caracteristics (identifier and properties) of all the sessions that are open on the Yaks service with identifier [yaks]
      (by default it will be the service this API is connected to) and that are managed by the frontend with identifier [feid].
      If [feid] is not specified, all the sessions caracterisitics are returned, whatever their frontend. *)

  val close_session : ?yaks:yid -> sid -> t -> unit Lwt.t
  (** [close_sessions yaks sid a] force-closes the session with identifier [sid] from the Yaks service with identifier [yaks]
      (by default it will be the service this API is connected to). *)

  val get_subscriptions : ?yaks:yid -> sid -> t -> Selector.t list Lwt.t
  (** [get_subscriptions yaks a] returns the selectors of all the subscriptions made by a session with identifier [sid] on the Yaks service with identifier [yaks]
      (by default it will be the service this API is connected to). *)

end


type t

val login : Apero_net.Locator.t -> properties -> t Lwt.t
(** [login l p] connects this API to the Yaks service using the locator [l] and login with the properties [p]. *)

val logout : t -> unit Lwt.t
(** [logout y] closes the connection of this API to the connected Yaks service *)

val get_id : t -> yid
(** [get_id y] returns the identifier of the Yaks service this API is connected to. *)

val workspace : Path.t -> t -> Workspace.t Lwt.t
(** [workspace p y] creates a new workspace with the working path [p].
    All non-absolute paths or selectors used with this workspace will be treated as relative to this working path *)

val admin : t -> Admin.t Lwt.t
(** [admin y] returns the Admin interface *)
