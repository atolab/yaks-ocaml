open Yaks_types

module Storage : sig 

  type t

  val get_id : t -> StorageId.t Lwt.t
  val create: StorageId.t -> Apero.properties -> Path.t -> Yaks_sock_driver.t -> t
end