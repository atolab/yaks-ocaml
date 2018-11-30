open Lwt.Infix
open Yaks_types
open Yaks_sock_types

module Storage = struct


  type state = {
    path : Path.t
  ; properties : Apero.properties
  ; sid : StorageId.t
  ; driver : Yaks_sock_driver.t
  }

  type t = state MVar.t

  let create id properties path driver = 
    MVar.create {sid=id; path; properties; driver}

  let get_id storage = 
    MVar.read storage >>= fun s -> Lwt.return s.sid

end