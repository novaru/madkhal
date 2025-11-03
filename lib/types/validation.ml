(** Validation types and result definitions *)

(** Validation error types *)
type field_error =
  | MissingRequiredField of string
  | InvalidType of { field : string; expected : string; got : string }
  | InvalidFormat of { field : string; reason : string }
  | OutOfRange of {
      field : string;
      min : float option;
      max : float option;
      value : float;
    }
  | InvalidLength of {
      field : string;
      min : int option;
      max : int option;
      length : int;
    }
  | CustomError of string

type validation_error = {
  field_path : string list; (* ["user"; "address"; "zipcode"] *)
  error : field_error;
  message : string;
}

(** Validation warnings (non-fatal) *)
type validation_warning =
  | DeprecatedField of string
  | UnknownField of string
  | ImplicitCoercion of { field : string; from_type : string; to_type : string }

(** Validation result *)
type validation_result =
  | Valid of Request.validated_request
  | Invalid of validation_error list
  | ValidWithWarnings of Request.validated_request * validation_warning list

(** Helper functions *)
let string_of_field_error = function
  | MissingRequiredField field ->
      Printf.sprintf "Missing required field: %s" field
  | InvalidType { field; expected; got } ->
      Printf.sprintf "Field %s: expected %s, got %s" field expected got
  | InvalidFormat { field; reason } ->
      Printf.sprintf "Field %s: invalid format - %s" field reason
  | OutOfRange { field; min; max; value } ->
      let min_str =
        Option.value ~default:"∞ " (Option.map string_of_float min)
      in
      let max_str =
        Option.value ~default:"∞ " (Option.map string_of_float max)
      in
      Printf.sprintf "Field %s: value %.2f not in range [%s, %s]" field value
        min_str max_str
  | InvalidLength { field; min; max; length } ->
      let min_str = Option.value ~default:"0" (Option.map string_of_int min) in
      let max_str = Option.value ~default:"∞ " (Option.map string_of_int max) in
      Printf.sprintf "Field %s: length %d not in range [%s, %s]" field length
        min_str max_str
  | CustomError msg -> msg

let validation_error_to_json error =
  `Assoc
    [
      "field_path", `List (List.map (fun s -> `String s) error.field_path);
      "message", `String error.message;
      ( "error_type",
        `String
          (match error.error with
          | MissingRequiredField _ -> "missing_required_field"
          | InvalidType _ -> "invalid_type"
          | InvalidFormat _ -> "invalid_format"
          | OutOfRange _ -> "out_of_range"
          | InvalidLength _ -> "invalid_length"
          | CustomError _ -> "custom_error") );
    ]
