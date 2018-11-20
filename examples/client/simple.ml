open Yaks_ocaml
open Yaks_ocaml.Yaks
open Lwt.Infix
let usage () = ignore( print_endline "USAGE:\n\t simple <yaks_address> <yaks_port>\n" )


let observer data = 
  Lwt_io.printf ">>>> [APP] OBSERVER\n"
  >>= fun _ -> Lwt_list.iter_p (fun (k,v) -> 
      Lwt_io.printf ">>>> [APP] [OBS] K %s - V: %s\n"  (Path.to_string k) (Value.to_string v) 
    ) data

let main argv = 
  let%lwt _ = Lwt_io.printf "[APP] Press enter at each step!!\n" in
  let addr = Array.get argv 1 in
  let port = Array.get argv 2 in 
  let locator = Apero.Option.get @@ Apero_net.Locator.of_string @@ Printf.sprintf "tcp/%s:%s" addr port in
  let%lwt api = Yaks.connect locator in
  ignore @@ Lwt_io.printf "<<<< [APP] Creating storage on %s\n"  "//afos/0";
  let%lwt _ = Lwt_io.read_line Lwt_io.stdin in
  let%lwt storage = Yaks.create_storage (Path.of_string "//afos/0") api in
  let comp = 
    ignore @@ Lwt_io.printf "<<<< [APP] Creating access on %s\n"  "//afos/0";
    let%lwt _ = Lwt_io.read_line Lwt_io.stdin in
    Yaks.create_access (Path.of_string "//afos/0") api
    >>= fun access -> 
    ignore @@ Lwt_io.printf "<<<< [APP] Subscribing to %s\n"  "//afos/**";
    let%lwt _ = Lwt_io.read_line Lwt_io.stdin in
    Yaks.Access.subscribe ~listener:observer (Selector.of_string "//afos/0/**") access
    >>= fun subid ->
    ignore @@ Lwt_io.printf "<<<< [APP] Put %s -> %s\n" "//afos/0" "hello!";
    let%lwt _ = Lwt_io.read_line Lwt_io.stdin in
    let t0 = Unix.gettimeofday () in 
    let _ = Yaks.Access.put 
        (Path.of_string "//afos/0/1")
        (Apero.Result.get (Value.of_string "hello!" Value.Raw_Encoding)) access in
    let t1 = Float.sub (Unix.gettimeofday ()) t0 in
    Lwt_io.printf "<<<< [APP] Put took %f\n"  t1
    >>= fun _ -> 
    ignore @@ Lwt_io.printf "<<<< [APP] Getting %s \n" "//afos/0";
    let%lwt _ = Lwt_io.read_line Lwt_io.stdin in
    Yaks.Access.get (Selector.of_string "//afos/0/*") access
    >>= fun data -> List.iter (
      fun (k,v) -> 
        ignore @@ Lwt_io.printf ">>>> [APP] K %s - V: %s\n"  (Path.to_string k) (Value.to_string v);
    ) data; Lwt.return_unit
    >>= fun _ -> 
    ignore @@ Lwt_io.printf "<<<< [APP] Unsub\n";
    let%lwt _ = Lwt_io.read_line Lwt_io.stdin in
    Yaks.Access.unsubscribe subid access
    >>= fun _ ->
    ignore @@ Lwt_io.printf "<<<< [APP] Dispose access\n";
    let%lwt _ = Lwt_io.read_line Lwt_io.stdin in
    Yaks.dispose_access access api
    >>= fun _ -> 
    ignore @@ Lwt_io.printf "<<<< [APP] Dispose storage\n";
    let%lwt _ = Lwt_io.read_line Lwt_io.stdin in
    Yaks.dispose_storage storage api
    >>= fun _ -> 
    ignore @@ Lwt_io.printf "<<<< [APP] Bye!\n";
    Yaks.close api
  in Lwt.join [comp]


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



