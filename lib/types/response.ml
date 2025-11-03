type backend_response = {
  status_code : int;
  headers : (string * string) list;
  body : Yojson.Safe.t option;
  latency_ms : float;
  service_id : string;
}
(** Backend response *)

(** Gateway response *)
type gateway_response =
  | Success of backend_response
  | Error of {
      code : string;
      message : string;
      details : Yojson.Safe.t option;
      status_code : int;
    }
  | Aggregated of {
      responses : backend_response list;
      strategy : [ `First | `All | `Fastest ];
    }
