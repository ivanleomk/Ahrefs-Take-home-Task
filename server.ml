open Cohttp_lwt_unix
open Yojson
open Printf
open Lwt

(* Defining References *)
let curr_connection = ref (-1)
let terminate_message = "TERIMINATE_MESSAGE"
let last_server_message = ref (0.0);;



(* Helper Functions *)
let get_current_time _ = let open Unix in Unix.time();;

let parse_json_body body key = 
  let open Yojson.Basic.Util in
  Yojson.Basic.from_string body |> member key |> to_string;;

let parse_json_body body key = 
  let open Yojson.Basic.Util in
  Yojson.Basic.from_string body |> member key |> to_string;;

let generate_yojson_string x : Yojson.Basic.t = `String x

let generate_assoc_list l = 
  let assoc_list = List.map (fun x -> (fst x, generate_yojson_string (snd x))) l in
  `Assoc assoc_list;;
  
let generate_JSON l = 
    generate_assoc_list l |> Yojson.Basic.to_string;;

(* Main Code *)


(* Server Code *)
let generate_json_error_body user_message = 
  let json_vals = [("status","error");("message",user_message)] in
  let resp_body = generate_JSON json_vals in
  resp_body;;

let parse_user_response body connection_id= 
  match !curr_connection with 
  | -1 -> 
    begin
      Logs.app (fun m -> m "Please Enter Your Message: " );
      let user_message = read_line () in
      let parsed_id = Random.int 10000 in
      let parsed_id_string = string_of_int parsed_id in
      let json_vals = [("curr_id",parsed_id_string);("message",user_message)] in
      let resp_body = generate_JSON json_vals in
      curr_connection:=parsed_id; 
      last_server_message := get_current_time ();
      flush stderr;
      sprintf "%s" resp_body
    end
  | curr_id -> 
    if connection_id <> curr_id 
    then sprintf "Connection Refused. Please try again later"
    else 
      begin
        let parsed_message = parse_json_body body "message" in
        if parsed_message = terminate_message then
          begin
            curr_connection:=-1;
            Logs.app (fun m -> m "Connection Terminated " );
            sprintf "Connection Terminated"
          end
        else
          begin
          let new_time = get_current_time () in
          let time_taken = (new_time -. !last_server_message) in
          Logs.app (fun m -> m "Message response took %i s " (int_of_float time_taken) );
          last_server_message := new_time;
          Logs.app (fun m -> m "Please enter your message:  " );
          let user_message = read_line () in
          Logs.app (fun m -> m "Recieved message of %s " user_message);
          let json_vals = [("status","success");("message",user_message)] in
          let resp_body = generate_JSON json_vals in
          sprintf "%s" resp_body;
          end
      end

let server_callback conn req body =
  body |> Cohttp_lwt.Body.to_string >|= (fun data ->
    try 
      let connection_id = parse_json_body data "curr_id" |> int_of_string in
      parse_user_response data connection_id
    with 
    | _ -> 
    Logs.app (fun m -> m "Server was unable to process user input\n" );
    generate_json_error_body "Unable to parse user input"
  )
  >>= (fun body -> Server.respond_string ~status:`OK ~body ())

let create_server _ = 
  Logs.app (fun m -> m "Server is now online\n");
  Server.create ~mode:(`TCP (`Port 9000)) (Server.make server_callback ())


(* Client Code *)
let client_callback conn req body =
  body |> Cohttp_lwt.Body.to_string >|= (fun data ->
    try 
      let connection_id = parse_json_body data "curr_id" |> int_of_string in
      parse_user_response data connection_id
    with 
    | _ -> 
    Logs.app (fun m -> m "Server was unable to process user input\n" );
    generate_json_error_body "Unable to parse user input"
  )
  >>= (fun body -> Server.respond_string ~status:`OK ~body ())


let create_client port = 
  Logs.app (fun m -> m "Server is now online\n");
  Server.create ~mode:(`TCP (`Port port)) (Server.make server_callback ())

  

let _ = 
  begin
    Printf.printf "Would you like to run this as a Server? (1 : No, 0 : Yes) : ";
    let res =  read_int () in
    match res = 0 with 
    | true -> 
    begin
      let () = Logs.set_reporter (Logs.format_reporter ()) in
      let () = Logs.set_level (Some Logs.Info) in
      Printf.printf "Running as a Server\n";
      Lwt_main.run (create_server ())
    end
    | false -> 
    begin
    Printf.printf "Running as a Client\n";
    Printf.printf "Please enter the Port that you want the client to run on: ";
    let port = read_line () in
    Printf.printf "Please enter the IP Address of the server to connect to : ";
    let ip_addr = read_line () in
    Printf.printf "You have indicated Port : %s , IP Address : %s, \nconnecting now ....." port ip_addr;
    Lwt_main.run (create_client port)
    end
    (* Lwt_main.run (server (res = 0)); *)
  end