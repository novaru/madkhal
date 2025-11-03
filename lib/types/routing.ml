open Request
open Transformation

type backend_service = {
  id : string;
  name : string;
  base_url : Uri.t;
  protocol : [ `Http | `Grpc ];
  timeout_ms : int;
  retry_attempts : int;
  health_check_path : string option;
}
(** Backend service definition *)

(** Backend health status *)
type health_status =
  | Healthy
  | Degraded of string
  (* reason *)
  | Unhealthy of string
  (* reason *)
  | Unknown

type backend_health = {
  service : backend_service;
  status : health_status;
  last_check : float;
  consecutive_failures : int;
}

(** Routing rule *)
type routing_rule =
  | PathPrefix of string list
  | ExactPath of string list
  | MethodMatch of http_method
  | HeaderMatch of { key : string; value : string }
  | BodyFieldMatch of { field : string; value : Yojson.Safe.t }
  | SchemaVersion of string
  | And of routing_rule list
  | Or of routing_rule list

type route = {
  id : string;
  rule : routing_rule;
  target_service : string; (* backend_service.id *)
  transformation_rules : string list; (* IDs of transformation rules *)
  priority : int; (* Higher priority = checked first *)
}
(** Route definition *)

(** Routing decision *)
type routing_decision =
  | RouteToService of {
      service : backend_service;
      transformed : transformed_request;
      target_path : string;
    }
  | RouteToMultiple of {
      targets : (backend_service * transformed_request * string) list;
      aggregation_strategy : [ `First | `All | `Fastest ];
    }
  | Reject of { reason : string; status_code : int }
