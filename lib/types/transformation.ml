open Request
open Schema

(** Transformation operations *)
type transformation_op =
  | RenameField of { from : string; to_ : string }
  | RemoveField of string
  | AddField of { name : string; value : Yojson.Safe.t }
  | TransformField of {
      field : string;
      transform : Yojson.Safe.t -> Yojson.Safe.t;
    }
  | NestFields of { fields : string list; under : string }
  | FlattenField of string
  | CoerceType of { field : string; to_type : field_type }

type transformation_rule = {
  name : string;
  description : string option;
  condition : validated_request -> bool; (* When to apply *)
  operations : transformation_op list;
}
(** Transformation rule *)

type transformed_request = {
  original : validated_request;
  transformed_body : Yojson.Safe.t;
  applied_rules : string list; (* Names of rules applied *)
}
(** Transformed request *)
