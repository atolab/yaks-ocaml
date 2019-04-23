
open Apero
open Yaks_ocaml
open Yaks
open Yaks.Infix

let rec put_n n ws base_path value = 
    let path = ~//base_path in 
    if n > 1 then
        begin
            let _ = Yaks.Workspace.put path value ws in 
            put_n (n-1) ws base_path value  
        end 
    else Yaks.Workspace.put  path value ws

let create_data n =
    let rec r_create_data n s = 
        if n > 0 then
            r_create_data (n-1) (s ^ (string_of_int @@ n mod 10))
        else s
    in  r_create_data n ""

let run locator samples size =
    let%lwt y = Yaks.login locator Properties.empty in 
    let%lwt ws = Yaks.workspace ~//"/" y  in
    let base_path = "/test/thr" in 
    let value = Value.StringValue (create_data size) in 
    let%lwt () = put_n samples ws base_path value  in
    Lwt.return_unit

let () =
    let addr = Array.get Sys.argv 1 in
    let port = Array.get Sys.argv 2 in 
    let samples = int_of_string (Array.get Sys.argv 3) in
    let size = int_of_string (Array.get Sys.argv 4) in
    let locator = Apero.Option.get @@ Apero_net.Locator.of_string @@ Printf.sprintf "tcp/%s:%s" addr port in 
    Lwt_main.run @@ run locator samples size 

