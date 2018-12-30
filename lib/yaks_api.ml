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


type listener_t = (Path.t * Value.t) list -> unit Lwt.t

type eval_callback_t = Path.t -> properties -> Value.t Lwt.t

module Workspace = struct
    type t = 
      { driver : Yaks_sock_driver.t
      ; wsid : wsid option 
      ; path: Path.t option }

    let get ?quorum selector t =
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

    let subscribe ?(listener=(fun _ -> Lwt.return_unit)) selector t =
      let _ = Logs_lwt.debug (fun m -> m "[YA]: SUB on %s" (Selector.to_string selector)) in
      Yaks_sock_driver.process_subscribe ?workspace:t.wsid ~listener selector t.driver

    let unsubscribe subid t =
      let _ = Logs_lwt.debug (fun m -> m "[YA]: UNSUB %s" subid) in
      Yaks_sock_driver.process_unsubscribe subid t.driver

    let eval path eval_callback t =
      let _ = Logs_lwt.debug (fun m -> m "[YA]: EVAL %s" (Path.to_string path)) in
      (* TODO: currently eval is registered  with absolute path, because the received selectors in GETs
         will be absolutes and need to match the registerd evals.
         Evolution: manage EvalsMap (and SubscriberMap) here (in API) rather than in driver.
         Thus, the EVAL msg can be send with relative path, while registered with abs path locally here.
      *)
      let abspath = if Path.is_relative path then Path.add_prefix ~prefix:(Option.get t.path) path else path in
      Yaks_sock_driver.process_eval ?workspace:t.wsid abspath eval_callback t.driver

end

module Admin = struct
  type t = { admin : Workspace.t }

  let prefix = "_admin_"
  let my_yaks = "local"

  let properties_of_value v = match v with
    | Value.PropertiesValue p -> p
    | _ -> Properties.singleton "value" (Value.to_string v)

  let add_frontend ?yaks feid props t =
    let yid = Option.get_or_default yaks my_yaks in
    let path = Printf.sprintf "/%s/%s/frontend/%s" prefix yid feid in
    Workspace.put ~quorum:1 (Path.of_string path) (Value.PropertiesValue props) t.admin

  let get_frontends ?yaks t =
    let yid = Option.get_or_default yaks my_yaks in
    let sel = Printf.sprintf "/%s/%s/frontend/*" prefix yid in
    Workspace.get ~quorum:1 (Selector.of_string sel) t.admin
    >|= List.map (fun (p, v) ->
      let feid = Astring.with_range ~first:(String.length sel-2) (Path.to_string p) in
      let prop = properties_of_value v in
      (feid, prop))

  let get_frontend ?yaks feid t =
    let yid = Option.get_or_default yaks my_yaks in
    let sel = Printf.sprintf "/%s/%s/frontend/%s" prefix yid feid in
    Workspace.get ~quorum:1 (Selector.of_string sel) t.admin
    >|= (fun l -> Option.map (List.nth_opt l 0) (fun (_,v) -> properties_of_value v))

  let remove_frontend ?yaks feid t =
    let yid = Option.get_or_default yaks my_yaks in
    let path = Printf.sprintf "/%s/%s/frontend/%s" prefix yid feid in
    Workspace.remove ~quorum:1 (Path.of_string path) t.admin


  let add_backend ?yaks beid props t =
    let yid = Option.get_or_default yaks my_yaks in
    let path = Printf.sprintf "/%s/%s/backend/%s" prefix yid beid in
    Workspace.put ~quorum:1 (Path.of_string path) (Value.PropertiesValue props) t.admin

  let get_backends ?yaks t =
    let yid = Option.get_or_default yaks my_yaks in
    let sel = Printf.sprintf "/%s/%s/backend/*" prefix yid in
    Workspace.get ~quorum:1 (Selector.of_string sel) t.admin
    >|= List.map (fun (p, v) ->
      let feid = Astring.with_range ~first:(String.length sel-2) (Path.to_string p) in
      let prop = properties_of_value v in
      (feid, prop))

  let get_backend ?yaks beid t =
    let yid = Option.get_or_default yaks my_yaks in
    let sel = Printf.sprintf "/%s/%s/backend/%s" prefix yid beid in
    Workspace.get ~quorum:1 (Selector.of_string sel) t.admin
    >|= (fun l -> Option.map (List.nth_opt l 0) (fun (_,v) -> properties_of_value v))

  let remove_backend ?yaks beid t =
    let yid = Option.get_or_default yaks my_yaks in
    let path = Printf.sprintf "/%s/%s/backend/%s" prefix yid beid in
    Workspace.remove ~quorum:1 (Path.of_string path) t.admin


  let add_storage ?yaks stid ?backend props t =
    let yid = Option.get_or_default yaks my_yaks in
    let beid = Option.get_or_default backend "auto" in
    let path = Printf.sprintf "/%s/%s/backend/%s/storage/%s" prefix yid beid stid in
    Workspace.put ~quorum:1 (Path.of_string path) (Value.PropertiesValue props) t.admin

  let get_storages ?yaks ?backend t =
    let yid = Option.get_or_default yaks my_yaks in
    let beid = Option.get_or_default backend "*" in
    let sel = Printf.sprintf "/%s/%s/backend/%s/storage/*" prefix yid beid in
    Workspace.get ~quorum:1 (Selector.of_string sel) t.admin
    >|= List.map (fun (p, v) ->
      let path = Path.to_string p in
      let last_slash_idx = Astring.find ~rev:true (fun c -> c = '/') path |> Option.get in
      let beid = Astring.with_range ~first:(last_slash_idx+1) path in
      let prop = properties_of_value v in
      (beid, prop))

  let get_storage ?yaks stid t =
    let yid = Option.get_or_default yaks my_yaks in
    let sel = Printf.sprintf "/%s/%s/backend/*/storage/%s" prefix yid stid in
    Workspace.get ~quorum:1 (Selector.of_string sel) t.admin
    >|= (fun l -> Option.map (List.nth_opt l 0) (fun (_,v) -> properties_of_value v))

  let remove_storage ?yaks stid t =
    let yid = Option.get_or_default yaks my_yaks in
    let sel = Printf.sprintf "/%s/%s/backend/*/storage/%s" prefix yid stid in
    let path = 
      Workspace.get ~quorum:1 (Selector.of_string sel) t.admin
      >|= (fun l -> Option.map (List.nth_opt l 0) (fun (p,_) -> p))
    in
    match%lwt path with
    | Some p -> Workspace.remove ~quorum:1 p t.admin
    | None -> Lwt.return_unit

  
  let get_sessions ?yaks ?frontend t =
    let yid = Option.get_or_default yaks my_yaks in
    let feid = Option.get_or_default frontend "*" in
    let sel = Printf.sprintf "/%s/%s/frontend/%s/session/*" prefix yid feid in
    Workspace.get ~quorum:1 (Selector.of_string sel) t.admin
    >|= List.map (fun (p, v) ->
      let path = Path.to_string p in
      let last_slash_idx = Astring.find ~rev:true (fun c -> c = '/') path |> Option.get in
      let beid = Astring.with_range ~first:(last_slash_idx+1) path in
      let prop = properties_of_value v in
      (beid, prop))

  let close_session ?yaks sid t =
    let yid = Option.get_or_default yaks my_yaks in
    let sel = Printf.sprintf "/%s/%s/frontend/*/session/%s" prefix yid sid in
    let path = 
      Workspace.get ~quorum:1 (Selector.of_string sel) t.admin
      >|= (fun l -> Option.map (List.nth_opt l 0) (fun (p,_) -> p))
    in
    match%lwt path with
    | Some p -> Workspace.remove ~quorum:1 p t.admin
    | None -> Lwt.return_unit


  let get_subscriptions ?yaks sid t =
    let yid = Option.get_or_default yaks my_yaks in
    let sel = Printf.sprintf "/%s/%s/frontend/*/session/%s" prefix yid sid in
    Workspace.get ~quorum:1 (Selector.of_string sel) t.admin
    >|= List.map (fun (_, v) -> Selector.of_string @@ Value.to_string v)

end

(* module Yaks = struct *)
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

(* end *)

