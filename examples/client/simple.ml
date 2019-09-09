open Apero
open Yaks_ocaml
open Yaks
open Yaks.Infix
open Lwt.Infix

let usage () = ignore( print_endline "USAGE:\n\t simple <yaks_address> <yaks_port>\n" )


let observer data = 
  Lwt_io.printf ">>>> [APP] OBSERVER\n"
  >>= fun _ -> Lwt_list.iter_p (fun (k,c) ->
      match c with
      | Put tv ->
        Lwt_io.printf ">>>> [APP] [OBS] received  Put of K %s - V: %s\n"  (Yaks.Path.to_string k) (Yaks.Value.to_string tv.value)
      | Update tv ->
        Lwt_io.printf ">>>> [APP] [OBS] received  Update of K %s - V: %s\n"  (Yaks.Path.to_string k) (Yaks.Value.to_string tv.value)
      | Remove t ->
        Lwt_io.printf ">>>> [APP] [OBS] received  Remove of K %s\n"  (Yaks.Path.to_string k)
    ) data

let eval_callback path props =
  let name = Properties.get_or_default "name" ~default:"World"  props in
  let%lwt _ = Lwt_io.printf ">>>> [APP] eval_callback called for %s with %s\n"  (Yaks.Path.to_string path) (Properties.to_string props) in
  Lwt.return @@ Yaks.Value.StringValue ("Hello "^name^" !!")

let eval_callback2 path props =
  let name = Properties.get_or_default "name" ~default:"World"  props in
  let%lwt _ = Lwt_io.printf ">>>> [APP] eval_callback2 called for %s with %s\n"  (Yaks.Path.to_string path) (Properties.to_string props) in
  Lwt.return @@ Yaks.Value.StringValue ("Bonjour "^name^" !!")


let print_admin_space workspace yaksid =
  let sel = ~/*("/@/"^yaksid^"/plugins/yaks/**") in
  Yaks.Workspace.get sel workspace
  >|= List.sort (fun (p,_) (p',_) -> Yaks.Path.compare p p')
  >|= fun pvs ->
    let%lwt _ = Lwt_io.printf "---- ADMIN SPACE ----\n" in
    Lwt.return @@ List.iter 
      (fun (p,v) -> ignore @@ Lwt_io.printf "  %s :\n    %s\n" (Yaks.Path.to_string p) (Yaks.Value.to_string v))
      pvs
  >>= fun _ -> Lwt_io.printf "---------------------\n"


let main argv = 
  let%lwt _ = Lwt_io.printf "[APP] Press enter at each step!!\n" in
  let addr = Array.get argv 1 in
  let port = Array.get argv 2 in 
  let locator = Printf.sprintf "tcp/%s:%s" addr port in
  let%lwt api = Yaks.login locator Properties.empty in
  let yaksid = Yaks.get_id api in

  let%lwt _ = Lwt_io.printf "\n<<<< [APP] Creating workspace on %s\n"  "/afos/0" in
  let%lwt _ = Lwt_io.read_line Lwt_io.stdin in
  let%lwt workspace = Yaks.workspace ~//"/afos/0" api in

  let%lwt _ = print_admin_space workspace yaksid in

  let%lwt _ = Lwt_io.printf "\n<<<< [APP] Creating storage on %s\n"  "/afos/0/**" in
  let%lwt admin = Yaks.admin api in
  let%lwt _ = Lwt_io.read_line Lwt_io.stdin in
  let%lwt _ = Yaks.Admin.add_storage "AFOS-0" (Properties.singleton "selector" "/afos/0/**") admin in

  let%lwt _ = print_admin_space workspace yaksid in

  let%lwt _ = Lwt_io.printf "\n<<<< [APP] Register eval for to %s\n"  "/afos/0/test_eval" in
  let%lwt _ = Lwt_io.read_line Lwt_io.stdin in
  let%lwt _ = Yaks.Workspace.register_eval ~//"test_eval" eval_callback workspace in

  let%lwt _ = Lwt_io.printf "\n<<<< [APP] Register eval2 for to %s\n"  "/afos/0/test_eval2" in
  let%lwt _ = Lwt_io.read_line Lwt_io.stdin in
  let%lwt _ = Yaks.Workspace.register_eval ~//"test_eval2" eval_callback2 workspace in

  let%lwt _ = print_admin_space workspace yaksid in

  let%lwt _ = Lwt_io.printf "\n<<<< [APP] Subscribing to %s\n"  "/afos/0/**" in
  let%lwt _ = Lwt_io.read_line Lwt_io.stdin in
  let%lwt subid = Yaks.Workspace.subscribe ~listener:observer ~/*"**" workspace in

  let%lwt _ = print_admin_space workspace yaksid in

  let%lwt _ = Lwt_io.printf "\n<<<< [APP] Put %s -> %s\n" "/afos/0/1" "hello!" in
  let%lwt _ = Lwt_io.read_line Lwt_io.stdin in
  let t0 = Unix.gettimeofday () in 
  let%lwt _ = Yaks.Workspace.put ~//"/afos/0/1" ~$"hello!" workspace in
  let t1 = Float.sub (Unix.gettimeofday ()) t0 in
  let%lwt _ = Lwt_io.printf "\n<<<< [APP] Put took %f\n" t1 in

  let%lwt _ = Lwt_io.printf "\n<<<< [APP] Getting %s \n" "/afos/0/*" in
  let%lwt _ = Lwt_io.read_line Lwt_io.stdin in
  Yaks.Workspace.get ~/*"*" workspace
  >>= fun data -> List.iter (
    fun (k,v) -> 
      ignore @@ Lwt_io.printf ">>>> [APP] K %s - V: %s\n"  (Yaks.Path.to_string k) (Yaks.Value.to_string v);
  ) data; Lwt.return_unit
  >>= fun _ -> 

  let%lwt _ = Lwt_io.printf "\n<<<< [APP] Calling eval %s \n" "/afos/0/test_eval" in
  let%lwt _ = Lwt_io.read_line Lwt_io.stdin in
  Yaks.Workspace.get ~/*"test_eval" workspace
  >>= fun data -> List.iter (
    fun (k,v) -> 
      ignore @@ Lwt_io.printf ">>>> [APP] K %s - V: %s\n"  (Yaks.Path.to_string k) (Yaks.Value.to_string v);
  ) data; Lwt.return_unit
  >>= fun _ -> 
  
  let%lwt _ = Lwt_io.printf "\n<<<< [APP] Calling eval %s with name=Bob\n" "/afos/0/test_eval2" in
  let%lwt _ = Lwt_io.read_line Lwt_io.stdin in
  Yaks.Workspace.get ~/*"test_eval2?(name=Bob)" workspace
  >>= fun data -> List.iter (
    fun (k,v) -> 
      ignore @@ Lwt_io.printf ">>>> [APP] K %s - V: %s\n"  (Yaks.Path.to_string k) (Yaks.Value.to_string v);
  ) data; Lwt.return_unit
  >>= fun _ -> 

  let%lwt _ = Lwt_io.printf "\n<<<< [APP] Calling eval %s with name=Carl\n" "/afos/0/test_*" in
  let%lwt _ = Lwt_io.read_line Lwt_io.stdin in
  let%lwt _ = Yaks.Workspace.get ~/*"test_*?(name=Carl)" workspace
  >>= fun data -> List.iter (
    fun (k,v) -> 
      ignore @@ Lwt_io.printf ">>>> [APP] K %s - V: %s\n"  (Yaks.Path.to_string k) (Yaks.Value.to_string v);
  ) data; Lwt.return_unit
  in

  let%lwt _ = Lwt_io.printf "\n<<<< [APP] Unregister eval %s \n" "/afos/0/test_eval" in
  let%lwt _ = Lwt_io.read_line Lwt_io.stdin in
  let%lwt _ = Yaks.Workspace.unregister_eval ~//"/afos/0/test_eval" workspace in

  let%lwt _ = print_admin_space workspace yaksid in

  let%lwt _ = Lwt_io.printf "\n<<<< [APP] Unsub\n" in
  let%lwt _ = Lwt_io.read_line Lwt_io.stdin in
  let%lwt _ = Yaks.Workspace.unsubscribe subid workspace in

  let%lwt _ = print_admin_space workspace yaksid in

  let%lwt _ = Lwt_io.printf "\n<<<< [APP] Remove storage\n" in
  let%lwt _ = Lwt_io.read_line Lwt_io.stdin in
  let%lwt _ = Yaks.Admin.remove_storage "AFOS-0" admin in

  let%lwt _ = print_admin_space workspace yaksid in

  let%lwt _ = Lwt_io.printf "\n<<<< [APP] logout\n" in
  let%lwt _ = Lwt_io.read_line Lwt_io.stdin in
  let%lwt _ = Yaks.logout api in

  let%lwt _ = Lwt_io.printf "\n<<<< [APP] Bye!\n" in
  Lwt.return_unit


let _ =
  let argv = Sys.argv in
  let level = Apero.Result.get @@ Logs.level_of_string "debug" in
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ());
  if Array.length argv < 3 then usage ()
  else
    begin
      Lwt_main.run (main argv)
    end
