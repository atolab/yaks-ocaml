open Apero
open Yaks_ocaml
open Yaks
open Yaks.Infix


let _ = 
  let argv = Sys.argv in
  Lwt_main.run 
  (
    let locator = if (Array.length argv > 1) then Array.get argv 1 else "tcp/127.0.0.1:7447" in
    (* If not specified as 2nd argument, use a relative path (to the workspace below): "yaks-ocaml-put" *)
    let path = if (Array.length argv > 2) then Array.get argv 2 else "yaks-ocaml-put" in

    let%lwt () = Lwt_io.printf "Login to %s\n" locator in
    let%lwt y = Yaks.login locator Properties.empty in

    let%lwt () = Lwt_io.printf "Use Workspace on '/demo/example'\n" in
    let%lwt w = Yaks.workspace ~//"/demo/example" y in

    let%lwt () = Lwt_io.printf "Remove %s\n" path in
    let%lwt () = Workspace.remove ~//path w in

    Yaks.logout y
  )
