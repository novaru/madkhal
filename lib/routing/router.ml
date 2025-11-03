open Types.Routing
open Types.Request
open Types.Transformation

(** Simple in-memory backend registry *)
let backend_registry : (string, backend_service) Hashtbl.t = Hashtbl.create 10

(** Register a backend service *)
let register_backend (service : backend_service) =
  Hashtbl.replace backend_registry service.id service

(** Find backend by id *)
let find_backend id = Hashtbl.find_opt backend_registry id

(** Initialize with some test backends *)
let init_test_backends () =
  let user_service =
    {
      id = "user-service";
      name = "User Service";
      base_url = Uri.of_string "http://localhost:3001";
      protocol = `Http;
      timeout_ms = 5000;
      retry_attempts = 2;
      health_check_path = Some "/health";
    }
  in
  let order_service =
    {
      id = "order-service";
      name = "Order Service";
      base_url = Uri.of_string "http://localhost:3002";
      protocol = `Http;
      timeout_ms = 5000;
      retry_attempts = 2;
      health_check_path = Some "/health";
    }
  in
  register_backend user_service;
  register_backend order_service

(** Evaluate a single routing rule against a request *)
let rec evaluate_rule rule request =
  match rule with
  | PathPrefix prefix ->
      let path_matches prefix path =
        let rec check p1 p2 =
          match p1, p2 with
          | [], _ -> true (* Empty prefix matches anything *)
          | _, [] -> false (* Prefix longer than path *)
          | h1 :: t1, h2 :: t2 when h1 = h2 -> check t1 t2
          | _ -> false
        in
        check prefix path
      in
      path_matches prefix request.path
  | ExactPath path -> request.path = path
  | MethodMatch method_type -> request.method_type = method_type
  | HeaderMatch { key; value } ->
      List.exists (fun (k, v) -> k = key && v = value) request.headers
  | BodyFieldMatch { field; value } -> (
      match request.body with
      | Some json -> (
          (* Simple field lookup - could be enhanced for nested fields *)
          match json with
          | `Assoc fields -> (
              match List.assoc_opt field fields with
              | Some field_value -> Yojson.Safe.equal field_value value
              | None -> false)
          | _ -> false)
      | None -> false)
  | SchemaVersion version -> (
      match List.assoc_opt "X-Schema-Version" request.headers with
      | Some v -> v = version
      | None -> version = "v1" (* Default to v1 *))
  | And rules -> List.for_all (fun r -> evaluate_rule r request) rules
  | Or rules -> List.exists (fun r -> evaluate_rule r request) rules

(** Find matching route for a request, and sort routes by priority (highest
    first)*)
let match_route routes request =
  let sorted_routes =
    List.sort (fun r1 r2 -> compare r2.priority r1.priority) routes
  in
  List.find_opt (fun route -> evaluate_rule route.rule request) sorted_routes

(** Simple route definitions for Phase 1 testing *)
let test_routes =
  [
    {
      id = "users-api";
      rule = And [ PathPrefix [ "api"; "v1"; "users" ]; MethodMatch POST ];
      target_service = "user-service";
      transformation_rules = [];
      priority = 10;
    };
    {
      id = "orders-api";
      rule = And [ PathPrefix [ "api"; "v1"; "orders" ]; MethodMatch POST ];
      target_service = "order-service";
      transformation_rules = [];
      priority = 10;
    };
    {
      id = "default-users";
      rule = PathPrefix [ "api"; "v1"; "users" ];
      target_service = "user-service";
      transformation_rules = [];
      priority = 5;
      (* Lower priority fallback *)
    };
  ]

(** Get default routes for testing *)
let get_default_routes () = test_routes

(** Make routing decision *)
let make_routing_decision route validated_request =
  match find_backend route.target_service with
  | Some service ->
      (* For Phase 1, create a simple transformed request *)
      let transformed : transformed_request =
        {
          original = validated_request;
          transformed_body = validated_request.validated_body;
          applied_rules = route.transformation_rules;
        }
      in
      RouteToService
        {
          service;
          transformed;
          target_path = "/" ^ String.concat "/" validated_request.original.path;
        }
  | None ->
      Reject
        {
          reason = "Backend service not found: " ^ route.target_service;
          status_code = 503;
        }
