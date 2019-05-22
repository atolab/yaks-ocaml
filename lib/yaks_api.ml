open Apero
open Lwt.Infix
open Yaks_types

type yid = string
type feid = string
type beid = string
type stid = string
type sid = string
type subid = Zenoh.sub


type listener_t = (Path.t * change) list -> unit Lwt.t

type eval_callback_t = Path.t -> properties -> Value.t Lwt.t

type transcoding_fallback = Fail | Drop | Keep

module EvalMap = Map.Make (Path)

module Workspace = struct
    type t =
      { zenoh : Zenoh.t
      ; path: Path.t
      ; evals: Zenoh.storage EvalMap.t Guard.t}

    let absolute_path path t = 
      if Path.is_relative path then Path.add_prefix ~prefix:t.path path else path

    let absolute_selector selector t =
      if Selector.is_relative selector then Selector.add_prefix ~prefix:t.path selector else selector
      

    let get ?quorum ?encoding ?fallback selector t =
      ignore quorum; ignore encoding; ignore fallback;
      let selector = absolute_selector selector t in
      Logs.debug (fun m -> m "[Yapi]: GET on %s" (Selector.to_string selector));
      Yaks_zutils.query_values t.zenoh selector

    let sget ?quorum ?encoding ?fallback selector t =
      ignore quorum; ignore encoding; ignore fallback;
      let selector = absolute_selector selector t in
      Logs.debug (fun m -> m "[Yapi]: GET on %s" (Selector.to_string selector));
      Yaks_zutils.squery_values t.zenoh selector

    let put ?quorum path value t =
      ignore quorum;
      let path = absolute_path path t in
      Logs.debug (fun m -> m "[Yapi]: PUT on %s" (Path.to_string path));
      Yaks_zutils.write_put t.zenoh path value

    let update ?quorum path value t =
      ignore quorum;
      let path = absolute_path path t in
      Logs.debug (fun m -> m "[Yapi]: UPDATE on %s" (Path.to_string path));
      Yaks_zutils.write_update t.zenoh path value

    let remove ?quorum path t =
      ignore quorum;
      let path = absolute_path path t in
      Logs.debug (fun m -> m "[Yapi]: REMOVE on %s" (Path.to_string path));
      Yaks_zutils.write_remove t.zenoh path

    let subscribe ?listener selector t =
      let selector = absolute_selector selector t in
      Logs.debug (fun m -> m "[Yapi]: SUB on %s" (Selector.to_string selector));
      let listener = match listener with
        | Some l -> Some (fun path changes -> List.map (fun change -> (path, change)) changes |> l)
        | None -> None
      in
      Yaks_zutils.subscribe t.zenoh ?listener selector

    let unsubscribe subid t =
      Logs.debug (fun m -> m "[Yapi]: UNSUB");
      Yaks_zutils.unsubscribe t.zenoh subid

    let zenoh_eval_prefix = "+"
    let zenoh_eval_prefix_path = Path.of_string "+"

    let register_eval path (eval_callback:eval_callback_t) t =
      let path = absolute_path path t in
      Logs.debug (fun m -> m "[Yapi]: REG_EVAL %s" (Path.to_string path));
      (* NB:
        - Currently an eval is represented with a storage, once zenoh will support something like evals, we'll
          transition to that abstraction to avoid bu construction the progagation of spurious values.
        - The Zenoh storage selector for eval is the eval's path prefixed with '+'
      *)
      let zenoh_eval_path = zenoh_eval_prefix ^ (Path.to_string path) in
      let on_query resname predicate =
        Logs.debug (fun m -> m "[Yapi]: Handling remote Zenoh query on eval '%s' for '%s?%s'" (Path.to_string path) resname predicate);
        eval_callback path (Properties.of_string predicate)
        >|= fun value ->
          let encoding = Some(Yaks_zutils.encoding_to_flag value) in
          let data_info = { Ztypes.empty_data_info with encoding; ts=None } in
          let buf = Abuf.create ~grow:8192 8192 in
          let () = Yaks_zutils.encode_value value buf in
          [(zenoh_eval_path, buf, data_info)]
      in
      Guard.guarded t.evals
        @@ fun evals ->
        let%lwt () = match EvalMap.find_opt path evals with
          | Some storage -> Zenoh.unstore t.zenoh storage
          | None -> Lwt.return_unit
        in
        let%lwt storage = Zenoh.store t.zenoh zenoh_eval_path (fun _ _ -> Lwt.return_unit) on_query in
      Guard.return () (EvalMap.add path storage evals)

    let unregister_eval path t =
      let path = absolute_path path t in
      Logs.debug (fun m -> m "[Yapi]: UNREG_EVAL %s" (Path.to_string path));
      Guard.guarded t.evals
        @@ fun evals ->
        let%lwt () = match EvalMap.find_opt path evals with
          | Some storage -> Zenoh.unstore t.zenoh storage
          | None -> Lwt.return_unit
        in
      Guard.return () (EvalMap.remove path evals)

    let eval ?multiplicity ?encoding ?fallback selector t =
      ignore multiplicity; ignore encoding; ignore fallback;
      let selector = absolute_selector selector t in
      let selector = Selector.add_prefix ~prefix: zenoh_eval_prefix_path selector in
      Logs.debug (fun m -> m "[Yapi]: EVAL on %s" (Selector.to_string selector));
      Yaks_zutils.query_values t.zenoh selector

end

module Admin = struct
  type t =
   { admin : Workspace.t
   ; yaksid : yid }

  let properties_of_value v = match v with
    | Value.PropertiesValue p -> p
    | _ -> Properties.singleton "value" (Value.to_string v)

  let add_backend ?yaks beid props t =
    let yaks = match yaks with | Some id -> id | None -> t.yaksid in
    let path = Printf.sprintf "/@/%s/backend/%s" yaks beid in
    Workspace.put ~quorum:1 (Path.of_string path) (Value.PropertiesValue props) t.admin

  let get_backends ?yaks t =
    let yaks = match yaks with | Some id -> id | None -> t.yaksid in
    let sel = Printf.sprintf "/@/%s/backend/*" yaks in
    Workspace.get ~quorum:1 (Selector.of_string sel) t.admin
    >|= List.map (fun (p, v) ->
      let feid = Astring.with_range ~first:(String.length sel-2) (Path.to_string p) in
      let prop = properties_of_value v in
      (feid, prop))

  let get_backend ?yaks beid t =
    let yaks = match yaks with | Some id -> id | None -> t.yaksid in
    let sel = Printf.sprintf "/@/%s/backend/%s" yaks beid in
    Workspace.get ~quorum:1 (Selector.of_string sel) t.admin
    >|= (fun l -> Option.map (List.nth_opt l 0) (fun (_,v) -> properties_of_value v))

  let remove_backend ?yaks beid t =
    let yaks = match yaks with | Some id -> id | None -> t.yaksid in
    let path = Printf.sprintf "/@/%s/backend/%s" yaks beid in
    Workspace.remove ~quorum:1 (Path.of_string path) t.admin


  let add_storage ?yaks stid ?backend props t =
    let yaks = match yaks with | Some id -> id | None -> t.yaksid in
    let beid = Option.get_or_default backend "auto" in
    let path = Printf.sprintf "/@/%s/backend/%s/storage/%s" yaks beid stid in
    Workspace.put ~quorum:1 (Path.of_string path) (Value.PropertiesValue props) t.admin

  let get_storages ?yaks ?backend t =
    let yaks = match yaks with | Some id -> id | None -> t.yaksid in
    let beid = Option.get_or_default backend "*" in
    let sel = Printf.sprintf "/@/%s/backend/%s/storage/*" yaks beid in
    Workspace.get ~quorum:1 (Selector.of_string sel) t.admin
    >|= List.map (fun (p, v) ->
      let path = Path.to_string p in
      let last_slash_idx = Astring.find ~rev:true (fun c -> c = '/') path |> Option.get in
      let beid = Astring.with_range ~first:(last_slash_idx+1) path in
      let prop = properties_of_value v in
      (beid, prop))

  let get_storage ?yaks stid t =
    let yaks = match yaks with | Some id -> id | None -> t.yaksid in
    let sel = Printf.sprintf "/@/%s/backend/*/storage/%s" yaks stid in
    Workspace.get ~quorum:1 (Selector.of_string sel) t.admin
    >|= (fun l -> Option.map (List.nth_opt l 0) (fun (_,v) -> properties_of_value v))

  let remove_storage ?yaks stid t =
    let yaks = match yaks with | Some id -> id | None -> t.yaksid in
    let sel = Printf.sprintf "/@/%s/backend/*/storage/%s" yaks stid in
    let path = 
      Workspace.get ~quorum:1 (Selector.of_string sel) t.admin
      >|= (fun l -> Option.map (List.nth_opt l 0) (fun (p,_) -> p))
    in
    match%lwt path with
    | Some p -> Workspace.remove ~quorum:1 p t.admin
    | None -> Lwt.return_unit

end

module Infix = struct

  let (~//) = Path.of_string
  let (~/*) = Selector.of_string
  let (~$) s = Value.of_string s Value.STRING |> Result.get

end


type t =
  { zenoh : Zenoh.t
  ; yaksid : yid
  ; properties : properties }

let login endpoint properties =
  let%lwt zenoh = Zenoh.zopen endpoint in
  let zinfo = Zenoh.info zenoh in
  match Properties.get "peer_pid" zinfo with
  | Some zid -> let yaksid = Yaks_zutils.zenohid_to_yaksid zid in  Lwt.return { zenoh; yaksid; properties; }
  | None -> raise @@ Yaks_common_errors.YException (`InternalError (`Msg ("Connected Zenohd doesn't provide the property 'peer_pid'")))

let logout t =
  ignore t;
  (* TODO: explicit close session...   unsubscribe? unstore? *)
  Lwt.return_unit

let get_id t = t.yaksid

let workspace path t : Workspace.t Lwt.t =
  (* TODO in sync with Zenoh: register path as a resource and use resource_id + relative_path in workspace *)
  let w : Workspace.t = 
    { zenoh = t.zenoh 
    ; path
    ; evals = Guard.create @@ EvalMap.empty }
  in
  Lwt.return w

let admin t : Admin.t Lwt.t = 
  let%lwt w = workspace (Path.of_string "/@/") t in
  let a : Admin.t = { admin=w; yaksid=t.yaksid } in
  Lwt.return a
