(* Set start time in admin handler *)
let start_time = Unix.gettimeofday ()
let () = Handlers.Admin_handler.set_start_time start_time

(* Keep some simple handlers in main for now *)
let hello_handler _request = Dream.html "Hello, World!"

let api_handler _request =
  `Assoc [ "data", `String "Hello, World!"; "status", `String "success" ]
  |> Yojson.Safe.to_string
  |> Dream.json

let person_handler request =
  let name = Dream.param request "name" in
  let age =
    match Dream.query request "age" with
    | Some a -> ( try int_of_string a with Failure _ -> -1)
    | None -> -1
  in
  `Assoc
    [
      "data", `Assoc [ "name", `String name; "age", `Int age ];
      "status", `String "success";
    ]
  |> Yojson.Safe.to_string
  |> Dream.json

let () =
  Dream.run ~port:8080
  @@ Dream.logger
  @@ Dream.router
       [
         (* Admin and testing endpoints *)
         Dream.get "/" hello_handler;
         Dream.get "/health" Handlers.Admin_handler.health_check;
         Dream.get "/api/hello" api_handler;
         Dream.get "/api/greet/:name" person_handler;
         Dream.post "/api/validate" Handlers.Gateway_handler.validate_request;
         (* Main gateway routing - catch all other requests *)
         Dream.any "/**" Handlers.Gateway_handler.handle_request;
       ]
