open Yaks_types
open Yaks_sock_access
open Yaks_sock_storage

module Api : sig 
  type t

  (* API mgmt *)
  val connect: Apero_net.Locator.t -> t Lwt.t
  val close: t -> unit Lwt.t

  (* Access mgmt *)

  val create_access : ?properties:Apero.properties -> ?alias:string -> Path.t -> t -> Access.t Lwt.t
  val dispose_access : Access.t -> t -> unit Lwt.t
  val get_accesses : t -> (Access.t list) Lwt.t
  val get_access : AccessId.t -> t -> Access.t Lwt.t

  (* Storage mgmt *)

  val create_storage: ?properties:Apero.properties -> ?alias:string -> Path.t -> t -> Storage.t Lwt.t
  val dispose_storage: Storage.t -> t -> unit Lwt.t
  val get_storages: t -> (Storage.t list) Lwt.t
  val get_storage: StorageId.t -> t -> Storage.t Lwt.t

end 