open Apero
open Lwt.Infix
open Atypes
open Yaks_types
open Yaks_sock_types
open Yaks_sock_access
open Yaks_sock_storage


module Api = struct 
  module AccessMap = Map.Make(AccessId)
  module StorageMap = Map.Make(StorageId)

  type state = { accesses : (Access.t) AccessMap.t
               ; storages : (Storage.t) StorageMap.t
               ; endpoint : Apero_net.Locator.t
               ; driver : Yaks_sock_driver.t }

  type t = state MVar.t

  let connect endpoint = 
    let open Apero_net.Locator in
    let open Yaks_sock_driver in
    match endpoint with
    | TcpLocator _ as ep -> 
      let _ = ignore @@ Logs_lwt.info (fun m -> m "[YAS]: Connecting to: %s" (Apero_net.Locator.to_string ep)) in
      create (endpoint) >>= fun driver ->
      let api = {accesses = AccessMap.empty
                ; storages = StorageMap.empty
                ; endpoint = ep
                ; driver} in
      Message.make_open () 
      >>= fun m -> process m driver 
      >>= fun m ->
      (match m.header.mid with
       | OK -> Lwt.return @@ MVar.create api
       | _ -> Lwt.fail_with "Server closed connection")
    | UdpLocator _ as ep -> 
      let _ = ignore @@ Logs_lwt.err (fun m -> m "[YAS]: Locator %s is unsupported" (Apero_net.Locator.to_string ep)) in
      let e = `ValidationError (`Msg ("Invalid Locator, only TCP supported")) in
      Lwt.fail @@ Exception e

  let close api = 
    MVar.take api >>= fun api ->
    let _ = ignore @@ Logs_lwt.info (fun m -> m "[YAS]: Closing connection to: %s " (Apero_net.Locator.to_string api.endpoint)) in
    Yaks_sock_driver.destroy api.driver

  let create_access ?(properties=Properties.empty) ?alias path api = 
    MVar.guarded api @@ fun self -> 
    let open Message in
    let _ = ignore @@ Logs_lwt.info (fun m -> m "[YAS]: Creating access on endpoint %s " (Apero_net.Locator.to_string self.endpoint)) in 
    let properties = match alias with
      | Some alias -> Properties.add Yaks_properties.Access.Key.alias alias properties
      | None -> properties
    in
    make_create `Access path properties 
    >>= fun msg -> Yaks_sock_driver.process msg self.driver 
    >>= fun rmsg ->
    if msg.header.corr_id <> rmsg.header.corr_id then
      Lwt.fail_with "Correlation Incorrect"
    else
      match rmsg.header.mid, rmsg.body with
      | OK, _ ->
        let aid = Properties.find Yaks_properties.Access.Key.id rmsg.header.properties in
        let access_id = Apero.Option.get @@ AccessId.of_string aid in
        let access = Access.create access_id path properties self.driver in
        let _ =  Logs_lwt.info (fun m -> m "[YAS]: Created access %s " (AccessId.to_string access_id)) in
        MVar.return access { self with accesses = AccessMap.add access_id access self.accesses }
      | ERROR, YErrorInfo vle ->
        Lwt.fail_with @@ "Yaks reported error: "^(Yaks_fe_sock_types.error_info_to_string vle)
      | _, _ ->
        Lwt.fail_with @@ "Unexpected reply message: "^(Yaks_fe_sock_codes.message_id_to_string rmsg.header.mid)


  let dispose_access access api =
    let open Message in
    MVar.guarded api @@ fun api ->
    Access.get_id access >>= fun aid ->
    let _ = ignore @@ Logs_lwt.info (fun m -> m "[YAS]: Dispose access %s on endpoint %s" (AccessId.to_string aid )(Apero_net.Locator.to_string api.endpoint)) in 
    make_delete ~delete_type:`Access (IdAccess aid) 
    >>= fun msg -> Yaks_sock_driver.process msg api.driver 
    >>= fun rmsg -> 
    if msg.header.corr_id <> rmsg.header.corr_id then
      Lwt.fail_with "Correlation Incorrect"
    else
      let _ = Logs_lwt.info (fun m -> m "[YAS]: Removed access %s " (AccessId.to_string aid)) in 
      MVar.return () {api with accesses = AccessMap.remove aid api.accesses}


  let get_accesses api = 
    MVar.read api >>= fun api ->
    Lwt.return @@ List.map (fun (_,a) -> a) (AccessMap.bindings api.accesses)

  let get_access access_id api = 
    MVar.read api >>= fun api ->
    Lwt.return @@ AccessMap.find access_id api.accesses

  let create_storage ?(properties=Properties.empty) ?alias path api =
    let open Message in
    MVar.guarded api @@ fun api ->
    let _ = ignore @@ Logs_lwt.info (fun m -> m "[YAS]: Creating Storage on endpoint %s " (Apero_net.Locator.to_string api.endpoint)) in
    let properties = match alias with
      | Some alias -> Properties.add Yaks_properties.Storage.Key.alias alias properties
      | None -> properties
    in
    make_create `Storage path properties 
    >>= fun msg -> Yaks_sock_driver.process msg api.driver
    >>= fun rmsg -> 
    if msg.header.corr_id <> rmsg.header.corr_id then
      Lwt.fail_with "Correlation Incorrect"
    else
      match rmsg.header.mid, rmsg.body with
      | OK, _ ->
        let sid = Properties.find Yaks_properties.Access.Key.id rmsg.header.properties in
        let storageid = Apero.Option.get @@ StorageId.of_string sid in
        let storage = Storage.create storageid properties path api.driver in
        let _ =  Logs_lwt.info (fun m -> m "[YAS]: Created storage %s " (StorageId.to_string storageid)) in
        MVar.return storage {api with storages = StorageMap.add storageid storage api.storages}
      | ERROR, YErrorInfo vle ->
        Lwt.fail_with @@ "Yaks reported error: "^(Yaks_fe_sock_types.error_info_to_string vle)
      | _, _ ->
        Lwt.fail_with @@ "Unexpected reply message: "^(Yaks_fe_sock_codes.message_id_to_string rmsg.header.mid)

  let dispose_storage storage api = 
    let open Message in
    MVar.guarded api @@ fun api ->
    Storage.get_id storage >>= fun sid ->
    let _ = ignore @@ Logs_lwt.info (fun m -> m "[YAS]: Disposing storage %s on endpoint %s" (StorageId.to_string sid) (Apero_net.Locator.to_string api.endpoint)) in 
    make_delete ~delete_type:`Storage (IdStorage sid)
    >>= fun msg -> Yaks_sock_driver.process msg api.driver
    >>= fun rmsg -> 
    if msg.header.corr_id <> rmsg.header.corr_id then
      Lwt.fail_with "Correlation Incorrect"
    else
      let _ = Logs_lwt.info (fun m -> m "[YAS]: Removed Storage %s " (StorageId.to_string sid)) in 
      MVar.return () {api with storages = StorageMap.remove sid api.storages}

  let get_storages api =
    MVar.read api >>= fun api ->
    Lwt.return @@ List.map (fun (_,s) -> s) (StorageMap.bindings api.storages)

  let get_storage storage_id api = 
    MVar.read api >>= fun api ->
    Lwt.return @@ StorageMap.find storage_id api.storages
end
