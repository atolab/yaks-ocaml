open Apero
open Yaks_types

module MVar : Apero.MVar

type listener_t = (Path.t * Value.t) list -> unit Lwt.t
type eval_callback_t = Path.t -> properties -> Value.t Lwt.t

type id =
  | IdAccess of AccessId.t
  | IdStorage of StorageId.t
  | IdSubscription of SubscriberId.t

type entity_type = [
  | `Access
  | `Storage
  | `Resource
]

module Message : sig 
  type t

  val make_msg : ?corrid:int64 -> Yaks_fe_sock_codes.message_id -> Yaks_fe_sock_codes.message_flags list -> Apero.properties -> Yaks_fe_sock_types.payload -> Yaks_fe_sock_types.message Lwt.t

  val make_open : ?username:String.t -> ?password:String.t -> unit -> Yaks_fe_sock_types.message Lwt.t
  val make_create : ?alias:String.t -> ?config:String.t -> ?complete:bool -> entity_type -> Path.t -> int -> Yaks_fe_sock_types.message Lwt.t
  val make_delete : ?delete_type:entity_type -> ?path:Path.t -> id -> Yaks_fe_sock_types.message Lwt.t
  val make_put : ?encoding: Yaks_fe_sock_codes.value_encoding -> id ->  Path.t -> Value.t -> Yaks_fe_sock_types.message Lwt.t 
  val make_patch : ?encoding: Yaks_fe_sock_codes.value_encoding -> id -> Path.t -> Value.t -> Yaks_fe_sock_types.message Lwt.t 
  val make_get : ?encoding: Yaks_fe_sock_codes.value_encoding -> id -> Selector.t -> Yaks_fe_sock_types.message Lwt.t
  val make_sub : ?encoding: Yaks_fe_sock_codes.value_encoding -> id -> Selector.t -> Yaks_fe_sock_types.message Lwt.t
  val make_unsub : id ->  id -> Yaks_fe_sock_types.message Lwt.t
  val make_eval : id -> Path.t -> Yaks_fe_sock_types.message Lwt.t
  val make_values : int64 -> (Path.t * Value.t) list -> Yaks_fe_sock_types.message Lwt.t
  val make_ok : id -> int64 -> Yaks_fe_sock_types.message Lwt.t
  val make_error : id -> int64 -> Yaks_fe_sock_codes.error_code -> Yaks_fe_sock_types.message Lwt.t
end
