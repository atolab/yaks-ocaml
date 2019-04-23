
open Apero
open Yaks_ocaml
open Yaks.Infix

type state = {mutable starttime:float; mutable count:int;}

let state =  {starttime=0.0; count=0}


let obs _ = 
    let now = Unix.gettimeofday() in 
      Lwt.return @@ match state.starttime with 
    | 0.0 -> state.starttime <- now; state.count <- 1
    | time when now < (time +. 1.0) -> state.count <- state.count + 1
    | _ -> 
      Printf.printf "%d\n%!" (state.count + 1)
      ; state.starttime <-now
      ; state.count <- 0 

let run locator =
    let%lwt y = Yaks.login locator Properties.empty in 
    let%lwt ws = Yaks.workspace ~//"/" y  in
    let base_path = "/test/thr" in  
    let%lwt _ = Yaks.Workspace.subscribe ~listener:obs ~/*base_path ws in
    let%lwt _ = Lwt_unix.sleep 3000.0 in 
    Lwt.return_unit

let () =
    let addr = Array.get Sys.argv 1 in
    let port = Array.get Sys.argv 2 in 
    let locator = Apero.Option.get @@ Apero_net.Locator.of_string @@ Printf.sprintf "tcp/%s:%s" addr port in 
    Lwt_main.run @@ run locator 
