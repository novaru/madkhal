let hello_handler _request = Dream.html "Hello, World!"

let api_handler _request =
  `Assoc [ ("data", `String "Hello, World!"); ("status", `String "success") ]
  |> Yojson.Safe.to_string |> Dream.json

let person_handler request =
  let name = Dream.param request "name" in
  let age =
    match Dream.query request "age" with
    | Some a -> ( try int_of_string a with Failure _ -> -1)
    | None -> -1
  in
  `Assoc
    [
      ("data", `Assoc [ ("name", `String name); ("age", `Int age) ]);
      ("status", `String "success");
    ]
  |> Yojson.Safe.to_string |> Dream.json

let () =
  Dream.run ~port:8080 @@ Dream.logger
  @@ Dream.router
       [
         Dream.get "/" hello_handler;
         Dream.get "/api/hello" api_handler;
         Dream.get "/api/greet/:name" person_handler;
       ]
