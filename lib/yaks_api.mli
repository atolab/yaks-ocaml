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
type subid = Zenoh.sub
(** Subscriber identifier (unique per Yaks service) *)


type listener_t = (Path.t * change) list -> unit Lwt.t
(** Callback function implemented by the listener *)

type eval_callback_t = Path.t -> properties -> Value.t Lwt.t
(** Callback function registered as an eval *)

type transcoding_fallback = Fail | Drop | Keep
(** Action to perform in [get] operation when the transcoding of a received [Value] 
    into the requested encoding fails.
    [Fail] means the [get] operation will return an error.
    [Drop] means the [get] operation will drop from the returned result the values which can't be transcoded.
    [Keep] means the [get] operation will return the values which can't be transcoded with their original encoding. *)


module RegisteredPath : sig
  type t

  val put : ?quorum:int -> Value.t -> t -> unit Lwt.t
  (** Similar to [Workspace.put] for a registered path. *)

  val update: ?quorum:int -> Value.t -> t -> unit Lwt.t
  (** Similar to [Workspace.update] for a registered path. *)

  val remove: ?quorum:int -> t  -> unit Lwt.t
  (** Similar to [Workspace.remove] for a registered path. *)
end

module Workspace : sig
  type t    

  val register_path : Path.t -> t -> RegisteredPath.t Lwt.t
  (** [register_path p] registers the Path [p] as a Publisher in Zenoh and returns a [RegisteredPath.t].
      A [RegisterdPath.t] can be used in case of successive put/update/remove on a same Path, to save bandwidth and get a better throughput.
      The [RegisterdPath] operations use the Zenoh.stream and Zenoh.lstream operations that avoid to send the Path as a string for each publication.
  *)

  val get : ?quorum:int -> ?encoding:Value.encoding -> ?fallback:transcoding_fallback -> Selector.t -> t -> (Path.t * Value.t) list Lwt.t
  (** [get quorum encoding fallback s ws] gets the set of tuples {e \{ <path,value> \} } available in YAKS for which {e path} 
    matches the {b selector} [s], where the selector [s] can be absolute or relative to the {e workspace} [ws]. 
      
    If a [quorum] is provided, then [get] will complete succesfully if and only if a number [quorum] of independent and complete storage set 
    exist. Complete storage means a storage that fully covers the selector (i.e. any path matching the selector is covered by the storage).
    This ensures that if there is a  {e \{ <path,value> \} } stored in YAKS for which the {e path} matches the selector [s], then
    there are at least [quorum] idependent copies of this element stored in YAKS. Of these [quorum] idependent copies, the one returned to the
    application is the most recent version.

    If no quorum is provided (notice this is the default behaviour) then the [get] will succeed even if there isn't a set
    of storages that fully covers the selector. I.e. storages that partially cover the selector will also reply.

    The [encoding]  allows an application to request values to be encoded in a specific format.
    See {! Yaks_types.Value.encoding} for the available encodings.

    If no encoding is provided (this is the default behaviour) then YAKS will not try to perform any transcoding and will
    return matching values in the encoding in which they are stored.
    
    The [fallback] controls what happens for those values that cannot be transcoded into the desired encoding, the 
    available options are:
    - Fail: the [get] fails if some value cannot be transcoded.
    - Drop: values that cannot be transcoded are dropped.
    - Keep: values that cannot be transcoded are kept with their original encoding and left for the application to deal with.  *)

  val sget : ?quorum:int -> ?encoding:Value.encoding -> ?fallback:transcoding_fallback -> Selector.t -> t -> (Path.t * Value.t) Lwt_stream.t
  (** Similar to [get], but returning the set of tuples {e \{ <path,value> \} } as a Lwt_stream.t *)

  val put : ?quorum:int -> Path.t -> Value.t -> t -> unit Lwt.t
  (** [put quorum path value ws]  
    - causes the notification of all {e subscriptions} whose selector matches [path], and

    - stores the tuple {e <path,value> } on all {e storages} in YAKS whose {e selector} 
    matches [path]

    Notice that the [path] can be absolute or erelative to the workspace [ws].

    If a [quorum] is provided then the [put] will success only if and only if a number 
    [quorum] of independent storages exist that match [path]. If such a set exist, the 
    put operation will complete only ater the tuple {e <path,value> } 
    has been written on all these storages.

    If no quorum is provided, then no assumptions are made and the [put] always succeeds,
    even if there are currently no matching storage. In this case the only effect of this operation
    will be that of triggering matching subscriber, if any exist. *)

  val update: ?quorum:int -> Path.t -> Value.t -> t -> unit Lwt.t
  (** [update quorum path value ws] allows to {! put} a delta, thus avoiding to distribute the entire value. *) 

  val remove: ?quorum:int -> Path.t -> t  -> unit Lwt.t
  (** [remove quorum path ws] removes from all  Yaks's storages the tuple having the given [path].
    [path] can be absolute or relative to the workspace [ws].
    If a [quorum] is provided, then the [remove] will complete only after having successfully removed the tuple 
    from [quorum] storages. *)

  val subscribe: ?listener:listener_t -> Selector.t -> t -> subid Lwt.t
  (** [subscribe listener selector ws] registers a subscription to tuples whose path matches the selector [s]. 
  
    A subscription identifier is returned.
    The [selector] can be absolute or relative to the workspace [ws]. If specified, 
    the [listener] callback will be called for each {! put} and {! update} on tuples whose
    path matches the subscription [selector] *)

  val unsubscribe: subid -> t -> unit Lwt.t
  (** [unsubscribe subid w] unregisters a previous subscription with the identifier [subid] *)

  val register_eval : Path.t -> eval_callback_t -> t -> unit Lwt.t
  (** [register_eval path eval ws] registers an evaluation function [eval] under the provided [path].
    The [path] can be absolute or relative to the workspace [ws]. *)

  val unregister_eval : Path.t -> t  -> unit Lwt.t
  (** [register_eval path ws] unregisters an previously registered evaluation function under the give [path].
    The [path] can be absolute or relative to the workspace [ws]. *)

end

module Admin : sig
  type t

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
      Note that "selector" is a mandatory property.
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

end

module Infix : sig
  
  val (~//) : string -> Path.t
  (** [~// s] returns [s] as a Path if it's valid. Otherwise it raises an [Exception].
      Note that the Path's string is sanitized (i.e. it's trimmed meaningless '/' are removed).
      This operator is equal to { Path.of_string }. *)

  val (~/*) : string -> Selector.t
  (** [~/* s] returns [s] as a Selector if it's valid. Otherwise it raises an [Exception].
      Note that the expression is sanitized (i.e. it's trimmed meaningless '/' are removed)
      This operator is equal to { Selector.of_string }. *)

  val (~$) : string -> Value.t
  (** [~$ s] returns [s] as a Value with a { Value.String_Encoding } as encoding.
      This operator is equal to { Value.of_string [s] Value.String_Encoding }. *)

end


type t

val login : string -> properties -> t Lwt.t
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
