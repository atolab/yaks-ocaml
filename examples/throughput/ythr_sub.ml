
open Apero
open Yaks_ocaml
open Yaks.Infix

type state = {mutable starttime: float; mutable count: int; n: int}

let state =  {starttime=0.0; count=0; n = 50000}


let obs _ = 
    (match state.count with 
      | 0 ->         
        state.starttime <- Unix.gettimeofday ();
        state.count <- state.count +1 
      | i when i = state.n -> 
        let delta = Unix.gettimeofday () -. state.starttime in 
        let thr = ((float_of_int) state.n) /. delta in            
        print_endline (string_of_float thr);
        state.count <- 0
      | _ ->                  
        state.count <- state.count +1)
      ; Lwt.return_unit

let run locator =
    let%lwt y = Yaks.login locator Properties.empty in 
    let%lwt ws = Yaks.workspace ~//"/" y  in
    let base_path = "/ythrp/sample" in  
    let%lwt _ = Yaks.Workspace.subscribe ~listener:obs ~/*base_path ws in
    let%lwt _ = Lwt_unix.sleep 3000.0 in 
    Lwt.return_unit

let () =
    let addr = Array.get Sys.argv 1 in
    let port = Array.get Sys.argv 2 in 
    let locator = Apero.Option.get @@ Apero_net.Locator.of_string @@ Printf.sprintf "tcp/%s:%s" addr port in 
    Lwt_main.run @@ run locator 
