open Apero
open Yaks_ocaml
open Yaks
open Yaks.Infix
open Cmdliner

let addr = Arg.(value & opt string "127.0.0.1" & info ["a"; "addr"] ~docv:"ADDRESS" ~doc:"address")
let port = Arg.(value & opt string "7887" & info ["p"; "port"] ~docv:"PORT" ~doc:"port")
let samples = Arg.(value & opt int 10000 & info ["n"; "samples"] ~docv:"SAMPLES" ~doc:"number of samples")
let size = Arg.(value & opt int 1024 & info ["s"; "size"] ~docv:"SIZE" ~doc:"payload size")
let exec_type = Arg.(value & opt string "s" & info ["t"; "exec_type"] ~docv:"EXEC_TYPE" ~doc:"s:serial or p:parallel")

(* performs gets in parallel*)
let rec get_n_p n selector ws result = 
    if n > 1 then
        begin
            let start = Unix.gettimeofday () in    
            let _ = Yaks.Workspace.get selector ws  in 
            let stop = Unix.gettimeofday () in 
            let delta = stop -. start in 
            let lt = int_of_float (delta *. 1000000.0) in
            Array.set result (n-1) lt;
            get_n_p (n-1) selector ws result 
        end 
    else Yaks.Workspace.get selector ws  
    
(* performs gets in serial*)
let rec get_n_s n selector ws result  = 
    if n > 1 then
        begin
            let start = Unix.gettimeofday () in    
            let%lwt _ = Yaks.Workspace.get selector ws in 
            let stop = Unix.gettimeofday () in 
            let delta = stop -. start in 
            let lt = int_of_float (delta *. 1000000.0) in
            Array.set result (n-1) lt;
            get_n_s (n-1) selector ws result
        end 
    else Yaks.Workspace.get selector ws

let create_data n =
    let rec r_create_data n s = 
        if n > 0 then
            r_create_data (n-1) (s ^ (string_of_int @@ n mod 10))
        else s
    in r_create_data n ""


let run addr port samples size exec_type =
  Lwt_main.run 
  (
    let result = Array.make samples 0 in
    let locator = Apero.Option.get @@ Apero_net.Locator.of_string @@ Printf.sprintf "tcp/%s:%s" addr port in  
    let%lwt y = Yaks.login locator Properties.empty in 
    let%lwt ws = Yaks.workspace ~//"/" y in 
    let value = Value.StringValue (create_data size) in
    let _ = Yaks.Workspace.put ~//"test/lat/get" value ws in
    let selector = ~/*"/test/thr/get" in 
    let%lwt _ = (match exec_type with
                    | "p" | "P" -> get_n_p samples selector ws result
                    | _ -> get_n_s samples selector ws result) in
    let _ = Array.iter (Printf.printf "%i\n") result in
    Lwt.return_unit
  )

let () =
    
    let _ = Term.(eval (const run $ addr $port $ samples $ size $exec_type, Term.info "ylatg")) in  ()
