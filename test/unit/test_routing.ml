(** Tests for the routing module *)

open Routing.Router
open Types.Request
open Types.Routing

(** Test helpers *)
let create_test_request ?(method_type = POST) ?(path = [ "api"; "v1"; "users" ])
    ?(headers = []) ?(body = None) () =
  {
    method_type;
    path;
    query_params = [];
    headers;
    body;
    metadata =
      {
        request_id = "test-123";
        timestamp = Unix.gettimeofday ();
        client_ip = "127.0.0.1";
        user_agent = Some "test-client";
        correlation_id = None;
      };
  }

let create_test_validated_request ?(schema_version = "v1") raw_request
    validated_body =
  {
    original = raw_request;
    schema_version;
    validated_body;
    required_fields = [ "name"; "email" ];
  }

(** Test 1: Path prefix matching *)
let test_path_prefix_match () =
  print_endline "Testing path prefix matching...";
  let rule = PathPrefix [ "api"; "v1"; "users" ] in
  let req1 = create_test_request ~path:[ "api"; "v1"; "users"; "123" ] () in
  let req2 = create_test_request ~path:[ "api"; "v2"; "users" ] () in

  assert (evaluate_rule rule req1 = true);
  assert (evaluate_rule rule req2 = false);
  print_endline "Path prefix matching test passed"

(** Test 2: Method matching *)
let test_method_match () =
  print_endline "Testing method matching...";
  let rule = MethodMatch POST in
  let req1 = create_test_request ~method_type:POST () in
  let req2 = create_test_request ~method_type:GET () in

  assert (evaluate_rule rule req1 = true);
  assert (evaluate_rule rule req2 = false);
  print_endline "Method matching test passed"

(** Test 3: Combined AND rule *)
let test_and_rule () =
  print_endline "Testing AND rule...";
  let rule = And [ PathPrefix [ "api"; "v1"; "users" ]; MethodMatch POST ] in
  let req1 =
    create_test_request ~method_type:POST ~path:[ "api"; "v1"; "users" ] ()
  in
  let req2 =
    create_test_request ~method_type:GET ~path:[ "api"; "v1"; "users" ] ()
  in
  let req3 =
    create_test_request ~method_type:POST ~path:[ "api"; "v1"; "orders" ] ()
  in

  assert (evaluate_rule rule req1 = true);
  assert (evaluate_rule rule req2 = false);
  (* Wrong method *)
  assert (evaluate_rule rule req3 = false);
  (* Wrong path *)
  print_endline "AND rule test passed"

(** Test 4: Route matching by priority *)
let test_route_priority () =
  print_endline "Testing route priority...";
  let high_priority_route =
    {
      id = "high";
      rule = PathPrefix [ "api"; "v1"; "users" ];
      target_service = "user-service-v2";
      transformation_rules = [];
      priority = 10;
    }
  in
  let low_priority_route =
    {
      id = "low";
      rule = PathPrefix [ "api"; "v1"; "users" ];
      target_service = "user-service-v1";
      transformation_rules = [];
      priority = 5;
    }
  in
  let routes = [ low_priority_route; high_priority_route ] in
  let req = create_test_request ~path:[ "api"; "v1"; "users" ] () in

  match match_route routes req with
  | Some route ->
      assert (route.id = "high");
      print_endline "Route priority test passed"
  | None -> assert false (* Should match *)

(** Test 5: Body field matching *)
let test_body_field_match () =
  print_endline "Testing body field matching...";
  let rule =
    BodyFieldMatch { field = "user_type"; value = `String "premium" }
  in

  let json1 =
    `Assoc [ "user_type", `String "premium"; "name", `String "Alice" ]
  in
  let json2 = `Assoc [ "user_type", `String "basic"; "name", `String "Bob" ] in

  let req1 = create_test_request ~body:(Some json1) () in
  let req2 = create_test_request ~body:(Some json2) () in
  let req3 = create_test_request ~body:None () in

  assert (evaluate_rule rule req1 = true);
  assert (evaluate_rule rule req2 = false);
  assert (evaluate_rule rule req3 = false);
  print_endline "Body field matching test passed"

(** Test 6: Routing decision *)
let test_routing_decision () =
  print_endline "Testing routing decision...";
  (* Initialize test backends *)
  init_test_backends ();

  let route =
    {
      id = "test-route";
      rule = PathPrefix [ "api"; "v1"; "users" ];
      target_service = "user-service";
      transformation_rules = [];
      priority = 10;
    }
  in

  let raw_req = create_test_request () in
  let validated_req =
    create_test_validated_request raw_req (`Assoc [ "name", `String "Alice" ])
  in

  match make_routing_decision route validated_req with
  | RouteToService { service; transformed; target_path = _ } ->
      assert (service.id = "user-service");
      assert (transformed.original = validated_req);
      print_endline "Routing decision test passed"
  | _ -> assert false (* Should route to service *)

(** Run all tests *)
let run_tests () =
  print_endline "Running routing tests...";
  print_endline "";

  test_path_prefix_match ();
  test_method_match ();
  test_and_rule ();
  test_route_priority ();
  test_body_field_match ();
  test_routing_decision ();

  print_endline "";
  print_endline "All routing tests passed!"

let () = run_tests ()
