open Yaks_common_types

module Storage : sig 

  type t

  val get_id : t -> StorageId.t Lwt.t
  val create: Apero.properties -> Path.t -> StorageId.t -> Yaks_sock_driver.t -> t
end