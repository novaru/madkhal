(** Main gateway request handlers *)

open Validation.Validator
open Types.Validation
open Types.Request
open Types.Routing
open Routing.Router

(** Convert Dream request to our internal request type *)
let dream_request_to_internal request =
  let method_type =
    match Dream.method_ request with
    | `GET -> GET
    | `POST -> POST
    | `PUT -> PUT
    | `DELETE -> DELETE
    | `PATCH -> PATCH
    | `OPTIONS -> OPTIONS
    | `HEAD -> HEAD
    | _ -> GET (* Default fallback *)
  in
  let path_str = Dream.target request in
  let path =
    String.split_on_char '/' path_str |> List.filter (fun s -> s <> "")
  in
  let headers = Dream.all_headers request in
  let request_id = Printf.sprintf "req-%f" (Unix.gettimeofday ()) in
  let metadata =
    {
      request_id;
      timestamp = Unix.gettimeofday ();
      client_ip = "127.0.0.1";
      (* TODO: Extract real IP *)
      user_agent = None;
      (* TODO: Extract from headers *)
      correlation_id = None;
    }
  in
  {
    method_type;
    path;
    query_params = [];
    (* TODO: Parse query params *)
    headers;
    body = None;
    (* Will be set later if needed *)
    metadata;
  }

(** Main gateway request handler with routing *)
let handle_request request =
  let internal_req = dream_request_to_internal request in

  (* Try to find a matching route *)
  let routes = get_default_routes () in
  match match_route routes internal_req with
  | Some route ->
      (* Route found, process the request *)
      let%lwt body_str = Dream.body request in
      let json_opt =
        if body_str = "" then None
        else try Some (Yojson.Safe.from_string body_str) with _ -> None
      in
      let _internal_req_with_body = { internal_req with body = json_opt } in

      (* For now, simulate successful routing *)
      let response_data =
        `Assoc
          [
            "status", `String "routed";
            "route_id", `String route.id;
            "target_service", `String route.target_service;
            "path", `String (String.concat "/" internal_req.path);
            ( "method",
              `String
                (match internal_req.method_type with
                | GET -> "GET"
                | POST -> "POST"
                | PUT -> "PUT"
                | DELETE -> "DELETE"
                | PATCH -> "PATCH"
                | OPTIONS -> "OPTIONS"
                | HEAD -> "HEAD") );
            "message", `String ("Request routed to " ^ route.target_service);
          ]
      in
      Dream.json (Yojson.Safe.to_string response_data)
  | None ->
      (* No route found *)
      let error_response =
        `Assoc
          [
            "status", `String "no_route";
            "message", `String "No matching route found";
            "path", `String (String.concat "/" internal_req.path);
            ( "method",
              `String
                (match internal_req.method_type with
                | GET -> "GET"
                | POST -> "POST"
                | PUT -> "PUT"
                | DELETE -> "DELETE"
                | PATCH -> "PATCH"
                | OPTIONS -> "OPTIONS"
                | HEAD -> "HEAD") );
          ]
      in
      Dream.json ~status:`Not_Found (Yojson.Safe.to_string error_response)

(** Validation endpoint using our modular validator *)
let validate_request request =
  let%lwt body = Dream.body request in
  try
    let json = Yojson.Safe.from_string body in
    let schema = create_user_schema () in
    match validate schema json with
    | Valid _validated_req ->
        `Assoc
          [
            "status", `String "valid";
            "message", `String "Validation successful";
            "validated_data", json;
          ]
        |> Yojson.Safe.to_string
        |> Dream.json
    | ValidWithWarnings (_validated_req, warnings) ->
        let warning_json =
          List.map
            (fun warning ->
              `Assoc
                [
                  ( "type",
                    `String
                      (match warning with
                      | DeprecatedField _ -> "deprecated_field"
                      | UnknownField _ -> "unknown_field"
                      | ImplicitCoercion _ -> "implicit_coercion") );
                  ( "message",
                    `String
                      (match warning with
                      | DeprecatedField field ->
                          "Field '" ^ field ^ "' is deprecated"
                      | UnknownField field -> "Unknown field '" ^ field ^ "'"
                      | ImplicitCoercion { field; from_type; to_type } ->
                          Printf.sprintf "Field '%s' coerced from %s to %s"
                            field from_type to_type) );
                ])
            warnings
        in
        `Assoc
          [
            "status", `String "valid_with_warnings";
            "message", `String "Validation successful with warnings";
            "validated_data", json;
            "warnings", `List warning_json;
          ]
        |> Yojson.Safe.to_string
        |> Dream.json
    | Invalid errors ->
        let error_json = List.map validation_error_to_json errors in
        `Assoc
          [
            "status", `String "invalid";
            "message", `String "Validation failed";
            "errors", `List error_json;
          ]
        |> Yojson.Safe.to_string
        |> Dream.json ~status:`Bad_Request
  with
  | Yojson.Json_error msg ->
      `Assoc
        [
          "status", `String "error"; "message", `String ("Invalid JSON: " ^ msg);
        ]
      |> Yojson.Safe.to_string
      |> Dream.json ~status:`Bad_Request
  | e ->
      `Assoc
        [
          "status", `String "error";
          "message", `String ("Server error: " ^ Printexc.to_string e);
        ]
      |> Yojson.Safe.to_string
      |> Dream.json ~status:`Internal_Server_Error
