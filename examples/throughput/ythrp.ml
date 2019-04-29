open Apero
open Yaks_ocaml
open Yaks
open Yaks.Infix
open Cmdliner

let addr = Arg.(value & opt string "127.0.0.1" & info ["a"; "addr"] ~docv:"ADDRESS" ~doc:"address")
let port = Arg.(value & opt string "7887" & info ["p"; "port"] ~docv:"PORT" ~doc:"port")
let samples = Arg.(value & opt int 100000 & info ["n"; "samples"] ~docv:"SAMPLES" ~doc:"number of samples")
let size = Arg.(value & opt int 1024 & info ["s"; "size"] ~docv:"SIZE" ~doc:"payload size")
let exec_type = Arg.(value & opt string "s" & info ["t"; "exec_type"] ~docv:"EXEC_TYPE" ~doc:"s:serial or p:parallel")

let rec put_n_p n ws path value = 
    if n > 1 then
        begin
            let _ = Yaks.Workspace.put path value ws in 
            put_n_p (n-1) ws path value 
        end 
    else Yaks.Workspace.put path value ws

let rec put_n_s n ws path value = 
    if n > 1 then
        begin
            let%lwt _ = Yaks.Workspace.put path value ws in 
            put_n_s (n-1) ws path value 
        end 
    else Yaks.Workspace.put path value ws

let create_data n =
    let rec r_create_data n s = 
        if n > 0 then
            r_create_data (n-1) (s ^ (string_of_int @@ n mod 10))
        else s
    in  r_create_data n ""

let run addr port samples size exec_type =
  Lwt_main.run 
  (
    let locator = Apero.Option.get @@ Apero_net.Locator.of_string @@ Printf.sprintf "tcp/%s:%s" addr port in 
    let%lwt y = Yaks.login locator Properties.empty in 
    let%lwt ws = Yaks.workspace ~//"/" y  in
    let path = ~//"/ythrp/sample" in 
    let value = Value.StringValue (create_data size) in 
    let start = Unix.gettimeofday () in 
    let%lwt () = (match exec_type with
                | "p" | "P" -> put_n_p samples ws path value
                | _ -> put_n_s samples ws path value) in
    let stop = Unix.gettimeofday () in 
    let delta = stop -. start in 
    let%lwt  _ = Lwt_io.printf "Sent %i samples in %fsec \n" samples delta in
    let%lwt  _ = Lwt_io.printf "Throughput: %f msg/sec\n" ((float_of_int samples) /. delta) in
    let%lwt  _ = Lwt_io.printf "Average: %f\n" (( delta) /. float_of_int samples) in
    Lwt.return_unit
  )

let () =
    let _ = Term.(eval (const run $ addr $port $ samples $ size $exec_type, Term.info "ythrp")) in  ()

