open Apero
open Apero.Infix
open Yaks_ocaml
open Yaks
open Yaks.Infix
open Cmdliner

let addr = Arg.(value & opt string "127.0.0.1" & info ["a"; "addr"] ~docv:"ADDRESS" ~doc:"address")
let port = Arg.(value & opt string "7887" & info ["p"; "port"] ~docv:"PORT" ~doc:"port")

let rec infinitewait () = 
    let%lwt _ = Lwt_unix.sleep 1000.0 in 
    infinitewait ()

let run addr port =
  Lwt_main.run 
  (
    let locator = Apero.Option.get @@ Apero_net.Locator.of_string @@ Printf.sprintf "tcp/%s:%s" addr port in 
    let%lwt y = Yaks.login locator Properties.empty in 
    let%lwt ws = Yaks.workspace ~//"/" y  in
    let%lwt _ = Yaks.Workspace.subscribe ~/*"/test/lat/ping" ws 
      ~listener:(List.split %> snd %> Lwt_list.iter_s (function
                 | Put tv -> Yaks.Workspace.put ~//"/test/lat/pong" tv.value ws
                 | _ -> Lwt.return_unit
                 ))
    in

    infinitewait ()
  )

let () =
    let _ = Term.(eval (const run $ addr $port, Term.info "ylat_pong")) in  ()