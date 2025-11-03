(** Core request types for the API Gateway *)

(** HTTP method types *)
type http_method = GET | POST | PUT | PATCH | DELETE | OPTIONS | HEAD

type request_metadata = {
  request_id : string;
  timestamp : float;
  client_ip : string;
  user_agent : string option;
  correlation_id : string option;
}
(** Request metadata *)

type raw_request = {
  method_type : http_method;
  path : string list; (* ["api"; "v1"; "users"; "123"] *)
  query_params : (string * string) list;
  headers : (string * string) list;
  body : Yojson.Safe.t option;
  metadata : request_metadata;
}
(** Raw incoming request *)

type validated_request = {
  original : raw_request;
  schema_version : string;
  validated_body : Yojson.Safe.t;
  required_fields : string list; (* Fields that were validated *)
}
(** Validated request (can only be constructed after validation) *)

(** Helper functions *)
let string_of_http_method = function
  | GET -> "GET"
  | POST -> "POST"
  | PUT -> "PUT"
  | PATCH -> "PATCH"
  | DELETE -> "DELETE"
  | OPTIONS -> "OPTIONS"
  | HEAD -> "HEAD"

let http_method_of_string = function
  | "GET" -> Some GET
  | "POST" -> Some POST
  | "PUT" -> Some PUT
  | "PATCH" -> Some PATCH
  | "DELETE" -> Some DELETE
  | "OPTIONS" -> Some OPTIONS
  | "HEAD" -> Some HEAD
  | _ -> None
