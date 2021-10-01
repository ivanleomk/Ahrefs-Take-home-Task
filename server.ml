open Cohttp_lwt_unix
open Yojson
open Printf
open Lwt




let callback conn req body =
  body |> Cohttp_lwt.Body.to_string >|= (fun data ->
    eprintf "Body: %s\n" data;flush stderr;
    sprintf "%i\n" 3;
  )
  >>= (fun body -> Server.respond_string ~status:`OK ~body ())

let create_server _ = 
  eprintf "Server is online\n";flush stderr;
    Server.create ~mode:(`TCP (`Port 2017)) (Server.make ~callback ())



let _ = 
  begin
    Printf.printf "Would you like to run this as a Server? (1 : No, 0 : Yes) : ";
    let res =  read_int () in
    match res = 0 with 
    | true -> 
    begin
      Printf.printf "Running as a Server\n";
      Lwt_main.run (create_server ())
    end
    | false -> 
    begin
    Printf.printf "Running as a Client\n";
    Printf.printf "Please enter the IP Address of the server to connect to : ";
    let ip_addr = read_line () in
    Printf.printf "You have indicated IP Address : %s, \nconnecting now ....." ip_addr;
    end
    (* Lwt_main.run (server (res = 0)); *)
  end
