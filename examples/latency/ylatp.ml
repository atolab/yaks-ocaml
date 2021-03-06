open Apero
open Yaks_ocaml
open Yaks
open Yaks.Infix
open Cmdliner

let addr = Arg.(value & opt string "127.0.0.1" & info ["a"; "addr"] ~docv:"ADDRESS" ~doc:"address")
let port = Arg.(value & opt string "7887" & info ["p"; "port"] ~docv:"PORT" ~doc:"port")
let samples = Arg.(value & opt int 10000 & info ["n"; "samples"] ~docv:"SAMPLES" ~doc:"number of samples")
let size = Arg.(value & opt int 1024 & info ["s"; "size"] ~docv:"SIZE" ~doc:"payload size")

let rec put_n_s n ws base_path value result =
    let path = ~//base_path in
    if n > 1 then
        begin
            let start = Unix.gettimeofday () in    
            let%lwt _ = Yaks.Workspace.put path value ws in
            let stop = Unix.gettimeofday () in 
            let delta = stop -. start in 
            let lt = int_of_float (delta *. 1000000.0) in
            Array.set result (n-1) lt;
            put_n_s (n-1) ws base_path value result
        end
    else Yaks.Workspace.put path value ws
let create_data n =
    let rec r_create_data n s = 
        if n > 0 then
            r_create_data (n-1) (s ^ (string_of_int @@ n mod 10))
        else s
    in r_create_data n ""


let run addr port samples size =
  Lwt_main.run 
  (
    let result = Array.make samples 0 in
    let locator = Printf.sprintf "tcp/%s:%s" addr port in  
    let%lwt y = Yaks.login locator Properties.empty in 
    let%lwt ws = Yaks.workspace ~//"/" y in 
    let base_path = "/test/lat/put" in 
    let value = Value.StringValue (create_data size) in
    let%lwt _ = put_n_s samples ws base_path value result in
    let _ = Array.iter (Printf.printf "%i\n") result in
    Lwt.return_unit
  )

let () =
    let _ = Term.(eval (const run $ addr $port $ samples $ size, Term.info "ylatp")) in  ()
