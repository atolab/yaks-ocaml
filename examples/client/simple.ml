open Apero
open Yaks_ocaml
open Lwt.Infix
let usage () = ignore( print_endline "USAGE:\n\t simple <yaks_address> <yaks_port>\n" )


let observer data = 
  Lwt_io.printf ">>>> [APP] OBSERVER\n"
  >>= fun _ -> Lwt_list.iter_p (fun (k,v) -> 
      Lwt_io.printf ">>>> [APP] [OBS] K %s - V: %s\n"  (Yaks.Path.to_string k) (Yaks.Value.to_string v) 
    ) data

let eval_callback path props =
  let name = Properties.get_or_default "name" ~default:"World"  props in
  ignore @@ Lwt_io.printf ">>>> [APP] eval_callback called for %s with %s\n"  (Yaks.Path.to_string path) (Properties.to_string props);
  Lwt.return @@ Yaks.Value.StringValue ("Hello "^name^" !!")


let print_admin_space workspace =
  let sel = Yaks.Selector.of_string "/yaks/local/**" in
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
  let locator = Apero.Option.get @@ Apero_net.Locator.of_string @@ Printf.sprintf "tcp/%s:%s" addr port in
  let%lwt api = Yaks.login locator Properties.empty in

  let%lwt _ = Lwt_io.printf "\n<<<< [APP] Creating workspace on %s\n"  "/afos/0" in
  let%lwt _ = Lwt_io.read_line Lwt_io.stdin in
  let%lwt workspace = Yaks.workspace (Yaks.Path.of_string "/afos/0") api in

  let%lwt _ = print_admin_space workspace in

  let%lwt _ = Lwt_io.printf "\n<<<< [APP] Creating storage on %s\n"  "/afos/0" in
  let%lwt admin = Yaks.admin api in
  let%lwt _ = Lwt_io.read_line Lwt_io.stdin in
  let%lwt _ = Yaks.Admin.add_storage "AFOS-0" (Properties.singleton "path" "/afos/0") admin in

  ignore @@ print_admin_space workspace;

  ignore @@ Lwt_io.printf "\n<<<< [APP] Add eval for to %s\n"  "/afos/0/test_eval";
  let%lwt _ = Lwt_io.read_line Lwt_io.stdin in
  let%lwt _ = Yaks.Workspace.eval (Yaks.Path.of_string "test_eval") eval_callback workspace in

  ignore @@ Lwt_io.printf "\n<<<< [APP] Subscribing to %s\n"  "/afos/0/**";
  let%lwt _ = Lwt_io.read_line Lwt_io.stdin in
  let%lwt subid = Yaks.Workspace.subscribe ~listener:observer (Yaks.Selector.of_string "**") workspace in

  ignore @@ print_admin_space workspace;

  ignore @@ Lwt_io.printf "\n<<<< [APP] Put %s -> %s\n" "/afos/0" "hello!";
  let%lwt _ = Lwt_io.read_line Lwt_io.stdin in
  let t0 = Unix.gettimeofday () in 
  let%lwt _ = Yaks.Workspace.put 
      (Yaks.Path.of_string "/afos/0/1")
      (Apero.Result.get (Yaks.Value.of_string "hello!" Yaks.Value.Raw_Encoding)) workspace in
  let t1 = Float.sub (Unix.gettimeofday ()) t0 in
  ignore @@ Lwt_io.printf "\n<<<< [APP] Put took %f\n" t1;

  ignore @@ Lwt_io.printf "\n<<<< [APP] Getting %s \n" "/afos/0";
  let%lwt _ = Lwt_io.read_line Lwt_io.stdin in
  Yaks.Workspace.get (Yaks.Selector.of_string "*") workspace
  >>= fun data -> List.iter (
    fun (k,v) -> 
      ignore @@ Lwt_io.printf ">>>> [APP] K %s - V: %s\n"  (Yaks.Path.to_string k) (Yaks.Value.to_string v);
  ) data; Lwt.return_unit
  >>= fun _ -> 

  ignore @@ Lwt_io.printf "\n<<<< [APP] Getting on eval %s \n" "/afos/0/test_eval";
  let%lwt _ = Lwt_io.read_line Lwt_io.stdin in
  let%lwt _ = Yaks.Workspace.get (Yaks.Selector.of_string "/afos/0/test_eval?[]") workspace
  >>= fun data -> List.iter (
    fun (k,v) -> 
      ignore @@ Lwt_io.printf ">>>> [APP] K %s - V: %s\n"  (Yaks.Path.to_string k) (Yaks.Value.to_string v);
  ) data; Lwt.return_unit
  in
  
  ignore @@ Lwt_io.printf "\n<<<< [APP] Getting on eval %s with name=Bob\n" "/afos/0/test_eval";
  let%lwt _ = Lwt_io.read_line Lwt_io.stdin in
  let%lwt _ =Yaks.Workspace.get (Yaks.Selector.of_string "/afos/0/test_eval?[name=Bob]") workspace
  >>= fun data -> List.iter (
    fun (k,v) -> 
      ignore @@ Lwt_io.printf ">>>> [APP] K %s - V: %s\n"  (Yaks.Path.to_string k) (Yaks.Value.to_string v);
  ) data; Lwt.return_unit
  in

  ignore @@ Lwt_io.printf "\n<<<< [APP] Remove eval %s \n" "/afos/0/test_eval";
  let%lwt _ = Lwt_io.read_line Lwt_io.stdin in
  let%lwt _ = Yaks.Workspace.remove (Yaks.Path.of_string "/afos/0/test_eval") workspace in

  ignore @@ print_admin_space workspace;

  ignore @@ Lwt_io.printf "\n<<<< [APP] Unsub\n";
  let%lwt _ = Lwt_io.read_line Lwt_io.stdin in
  let%lwt _ = Yaks.Workspace.unsubscribe subid workspace in

  ignore @@ print_admin_space workspace;

  ignore @@ Lwt_io.printf "\n<<<< [APP] Remove storage\n";
  let%lwt _ = Lwt_io.read_line Lwt_io.stdin in
  let%lwt _ = Yaks.Admin.remove_storage "AFOS-0" admin in

  ignore @@ print_admin_space workspace;

  ignore @@ Lwt_io.printf "\n<<<< [APP] logout\n";
  let%lwt _ = Lwt_io.read_line Lwt_io.stdin in
  let%lwt _ = Yaks.logout api in

  ignore @@ Lwt_io.printf "\n<<<< [APP] Bye!\n";
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

