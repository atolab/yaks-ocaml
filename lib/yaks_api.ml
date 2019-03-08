open Apero
open Lwt.Infix
open Yaks_types
open Yaks_sock_types

type yid = string
type feid = string
type beid = string
type stid = string
type sid = string
type subid = string


type on_put_t = Path.t -> Value.t -> unit Lwt.t
type on_update_t = Path.t -> Value.t -> unit Lwt.t
type on_remove_t = Path.t -> unit Lwt.t

type eval_callback_t = Path.t -> properties -> Value.t Lwt.t

type transcoding_fallback = Fail | Drop | Keep

module Workspace = struct
    type t = 
      { driver : Yaks_sock_driver.t
      ; wsid : wsid option 
      ; path: Path.t option }

    let get ?quorum ?encoding ?fallback selector t =
      let _ = ignore encoding and _ = ignore fallback in
      let _ = Logs_lwt.debug (fun m -> m "[YA]: GET on %s" (Selector.to_string selector)) in
      Yaks_sock_driver.process_get ?quorum ?workspace:t.wsid selector t.driver

    let put ?quorum path value t =
      let _ = Logs_lwt.debug (fun m -> m "[YA]: PUT on %s" (Path.to_string path)) in
      Yaks_sock_driver.process_put ?quorum ?workspace:t.wsid path value t.driver

    let update ?quorum path value t =
      let _ = Logs_lwt.debug (fun m -> m "[YA]: UPDATE on %s" (Path.to_string path)) in
      Yaks_sock_driver.process_put ?quorum ?workspace:t.wsid path value t.driver

    let remove ?quorum path t =
      let _ = Logs_lwt.debug (fun m -> m "[YA]: REMOVE on %s" (Path.to_string path)) in
      Yaks_sock_driver.process_remove ?quorum ?workspace:t.wsid path t.driver

    let subscribe ?(on_put=fun _ _ -> Lwt.return_unit) ?(on_update=fun _ _ -> Lwt.return_unit) ?(on_remove=fun _ -> Lwt.return_unit) selector t =
      let _ = Logs_lwt.debug (fun m -> m "[YA]: SUB on %s" (Selector.to_string selector)) in
      Yaks_sock_driver.process_subscribe ?workspace:t.wsid on_put on_update on_remove selector t.driver

    let unsubscribe subid t =
      let _ = Logs_lwt.debug (fun m -> m "[YA]: UNSUB %s" subid) in
      Yaks_sock_driver.process_unsubscribe subid t.driver

    let register_eval path eval_callback t =
      let _ = Logs_lwt.debug (fun m -> m "[YA]: REG_EVAL %s" (Path.to_string path)) in
      Yaks_sock_driver.process_register_eval ?workspace:t.wsid ?workpath:t.path path eval_callback t.driver

    let unregister_eval path t =
      let _ = Logs_lwt.debug (fun m -> m "[YA]: UNREG_EVAL %s" (Path.to_string path)) in
      Yaks_sock_driver.process_unregister_eval ?workspace:t.wsid ?workpath:t.path path t.driver

    let eval ?multiplicity ?encoding ?fallback selector t =
      let _ = ignore encoding and _ = ignore fallback in
      let _ = Logs_lwt.debug (fun m -> m "[YA]: EVAL on %s" (Selector.to_string selector)) in
      Yaks_sock_driver.process_eval ?multiplicity ?workspace:t.wsid selector t.driver

end

module Admin = struct
  type t = { admin : Workspace.t }

  let prefix = "@"
  let my_yaks = "local"

  let properties_of_value v = match v with
    | Value.PropertiesValue p -> p
    | _ -> Properties.singleton "value" (Value.to_string v)

  let add_frontend ?(yaks=my_yaks) feid props t =
    let path = Printf.sprintf "/%s/%s/frontend/%s" prefix yaks feid in
    Workspace.put ~quorum:1 (Path.of_string path) (Value.PropertiesValue props) t.admin

  let get_frontends ?(yaks=my_yaks) t =
    let sel = Printf.sprintf "/%s/%s/frontend/*" prefix yaks in
    Workspace.get ~quorum:1 (Selector.of_string sel) t.admin
    >|= List.map (fun (p, v) ->
      let feid = Astring.with_range ~first:(String.length sel-2) (Path.to_string p) in
      let prop = properties_of_value v in
      (feid, prop))

  let get_frontend ?(yaks=my_yaks) feid t =
    let sel = Printf.sprintf "/%s/%s/frontend/%s" prefix yaks feid in
    Workspace.get ~quorum:1 (Selector.of_string sel) t.admin
    >|= (fun l -> Option.map (List.nth_opt l 0) (fun (_,v) -> properties_of_value v))

  let remove_frontend ?(yaks=my_yaks) feid t =
    let path = Printf.sprintf "/%s/%s/frontend/%s" prefix yaks feid in
    Workspace.remove ~quorum:1 (Path.of_string path) t.admin


  let add_backend ?(yaks=my_yaks) beid props t =
    let path = Printf.sprintf "/%s/%s/backend/%s" prefix yaks beid in
    Workspace.put ~quorum:1 (Path.of_string path) (Value.PropertiesValue props) t.admin

  let get_backends ?(yaks=my_yaks) t =
    let sel = Printf.sprintf "/%s/%s/backend/*" prefix yaks in
    Workspace.get ~quorum:1 (Selector.of_string sel) t.admin
    >|= List.map (fun (p, v) ->
      let feid = Astring.with_range ~first:(String.length sel-2) (Path.to_string p) in
      let prop = properties_of_value v in
      (feid, prop))

  let get_backend ?(yaks=my_yaks) beid t =
    let sel = Printf.sprintf "/%s/%s/backend/%s" prefix yaks beid in
    Workspace.get ~quorum:1 (Selector.of_string sel) t.admin
    >|= (fun l -> Option.map (List.nth_opt l 0) (fun (_,v) -> properties_of_value v))

  let remove_backend ?(yaks=my_yaks) beid t =
    let path = Printf.sprintf "/%s/%s/backend/%s" prefix yaks beid in
    Workspace.remove ~quorum:1 (Path.of_string path) t.admin


  let add_storage ?(yaks=my_yaks) stid ?backend props t =
    let beid = Option.get_or_default backend "auto" in
    let path = Printf.sprintf "/%s/%s/backend/%s/storage/%s" prefix yaks beid stid in
    Workspace.put ~quorum:1 (Path.of_string path) (Value.PropertiesValue props) t.admin

  let get_storages ?(yaks=my_yaks) ?backend t =
    let beid = Option.get_or_default backend "*" in
    let sel = Printf.sprintf "/%s/%s/backend/%s/storage/*" prefix yaks beid in
    Workspace.get ~quorum:1 (Selector.of_string sel) t.admin
    >|= List.map (fun (p, v) ->
      let path = Path.to_string p in
      let last_slash_idx = Astring.find ~rev:true (fun c -> c = '/') path |> Option.get in
      let beid = Astring.with_range ~first:(last_slash_idx+1) path in
      let prop = properties_of_value v in
      (beid, prop))

  let get_storage ?(yaks=my_yaks) stid t =
    let sel = Printf.sprintf "/%s/%s/backend/*/storage/%s" prefix yaks stid in
    Workspace.get ~quorum:1 (Selector.of_string sel) t.admin
    >|= (fun l -> Option.map (List.nth_opt l 0) (fun (_,v) -> properties_of_value v))

  let remove_storage ?(yaks=my_yaks) stid t =
    let sel = Printf.sprintf "/%s/%s/backend/*/storage/%s" prefix yaks stid in
    let path = 
      Workspace.get ~quorum:1 (Selector.of_string sel) t.admin
      >|= (fun l -> Option.map (List.nth_opt l 0) (fun (p,_) -> p))
    in
    match%lwt path with
    | Some p -> Workspace.remove ~quorum:1 p t.admin
    | None -> Lwt.return_unit

  
  let get_sessions ?(yaks=my_yaks) ?frontend t =
    let feid = Option.get_or_default frontend "*" in
    let sel = Printf.sprintf "/%s/%s/frontend/%s/session/*" prefix yaks feid in
    Workspace.get ~quorum:1 (Selector.of_string sel) t.admin
    >|= List.map (fun (p, v) ->
      let path = Path.to_string p in
      let last_slash_idx = Astring.find ~rev:true (fun c -> c = '/') path |> Option.get in
      let beid = Astring.with_range ~first:(last_slash_idx+1) path in
      let prop = properties_of_value v in
      (beid, prop))

  let close_session ?(yaks=my_yaks) sid t =
    let sel = Printf.sprintf "/%s/%s/frontend/*/session/%s" prefix yaks sid in
    let path = 
      Workspace.get ~quorum:1 (Selector.of_string sel) t.admin
      >|= (fun l -> Option.map (List.nth_opt l 0) (fun (p,_) -> p))
    in
    match%lwt path with
    | Some p -> Workspace.remove ~quorum:1 p t.admin
    | None -> Lwt.return_unit


  let get_subscriptions ?(yaks=my_yaks) sid t =
    let sel = Printf.sprintf "/%s/%s/frontend/*/session/%s" prefix yaks sid in
    Workspace.get ~quorum:1 (Selector.of_string sel) t.admin
    >|= List.map (fun (_, v) -> Selector.of_string @@ Value.to_string v)

end

module Infix = struct

  let (~//) = Path.of_string
  let (~/*) = Selector.of_string
  let (~$) s = Value.of_string s Value.String_Encoding |> Result.get

end


type t =
  { endpoint : Apero_net.Locator.t
  ; driver : Yaks_sock_driver.t }

let login endpoint props = 
  let open Apero_net.Locator in
  match endpoint with
  | TcpLocator _ as ep -> 
    let _ = ignore @@ Logs_lwt.info (fun m -> m "[YAS]: Connecting to: %s" (Apero_net.Locator.to_string ep)) in
    Yaks_sock_driver.create (endpoint) >>= fun driver ->
    let t = {endpoint = ep; driver} in
    Lwt.try_bind
      (fun () -> Yaks_sock_driver.process_login props driver)
      (fun () -> Lwt.return t)
      (fun ex -> let%lwt _ = Yaks_sock_driver.destroy driver in
        Lwt.fail_with @@ "Login failed: "^(Printexc.to_string ex))
  | UdpLocator _ as ep -> 
    let _ = ignore @@ Logs_lwt.err (fun m -> m "[YAS]: Locator %s is unsupported" (Apero_net.Locator.to_string ep)) in
    let e = `ValidationError (`Msg ("Invalid Locator, only TCP supported")) in
    Lwt.fail @@ Exception e


let logout t =
  Yaks_sock_driver.process_logout t.driver >>= fun () ->
  Yaks_sock_driver.destroy t.driver

let get_id _ = "local"

let workspace path t =
  Yaks_sock_driver.process_workspace path t.driver >|=
  fun wsid : Workspace.t -> { driver=t.driver; wsid=Some wsid; path=Some path }

let admin t : Admin.t Lwt.t = 
  let w : Workspace.t = { driver=t.driver; wsid=None; path=None } in
  let a : Admin.t = { admin=w } in
  Lwt.return a


