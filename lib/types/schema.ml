(** Field types *)
type field_type =
  | String of {
      min_length : int option;
      max_length : int option;
      pattern : string option;
    }
  | Integer of { min : int option; max : int option }
  | Float of { min : float option; max : float option }
  | Boolean
  | Array of {
      item_type : field_type;
      min_items : int option;
      max_items : int option;
    }
  | Object of field_definition list
  | Enum of string list
  | DateTime of { format : string }
  | Any

and field_definition = {
  name : string;
  field_type : field_type;
  required : bool;
  default : Yojson.Safe.t option;
  description : string option;
}
(** Field definition *)

type schema = {
  version : string;
  name : string;
  fields : field_definition list;
  strict : bool; (* Reject unknown fields? *)
}
(** Schema definition *)

type schema_registry = {
  schemas : (string * schema) list;
  (* version -> schema *) default_version : string;
}
(** Schema registry *)
