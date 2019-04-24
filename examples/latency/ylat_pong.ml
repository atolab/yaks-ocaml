open Apero
open Apero.Infix
open Yaks_ocaml
open Yaks
open Yaks.Infix

let rec infinitewait () = 
    let%lwt _ = Lwt_unix.sleep 1000.0 in 
    infinitewait ()

let run locator =
    let%lwt y = Yaks.login locator Properties.empty in 
    let%lwt ws = Yaks.workspace ~//"/" y  in
    let%lwt _ = Yaks.Workspace.subscribe ~/*"/test/lat/ping" ws 
      ~listener:(List.split %> snd %> Lwt_list.iter_s (function
                 | Put tv -> Yaks.Workspace.put ~//"/test/lat/pong" tv.value ws
                 | _ -> Lwt.return_unit
                 ))
    in

    infinitewait ()

let () =
    let addr = Array.get Sys.argv 1 in
    let port = Array.get Sys.argv 2 in 
    let locator = Apero.Option.get @@ Apero_net.Locator.of_string @@ Printf.sprintf "tcp/%s:%s" addr port in 
    Lwt_main.run @@ run locator 
