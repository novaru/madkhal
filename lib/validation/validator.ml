(** Core validation logic using shared types *)

open Types.Schema
open Types.Validation
open Types.Request

(** Helper function to get type name for error messages *)
let type_name_of_json (json : Yojson.Safe.t) =
  match json with
  | `String _ -> "string"
  | `Int _ -> "integer"
  | `Intlit _ -> "integer"
  | `Float _ -> "float"
  | `Bool _ -> "boolean"
  | `List _ -> "array"
  | `Assoc _ -> "object"
  | `Null -> "null"
  | `Tuple _ -> "tuple"
  | `Variant (name, _) -> "variant(" ^ name ^ ")"

let expected_type_name = function
  | String _ -> "string"
  | Integer _ -> "integer"
  | Float _ -> "float"
  | Boolean -> "boolean"
  | Array _ -> "array"
  | Object _ -> "object"
  | Enum _ -> "enum"
  | DateTime _ -> "datetime"
  | Any -> "any"

(** Validate a single field against its definition *)
let validate_field field_def (json_value : Yojson.Safe.t) =
  match field_def.field_type, json_value with
  (* String validation *)
  | String { min_length; max_length; pattern = _ }, `String s ->
      let len = String.length s in
      let check_min =
        match min_length with None -> true | Some min -> len >= min
      in
      let check_max =
        match max_length with None -> true | Some max -> len <= max
      in
      if check_min && check_max then Ok json_value
      else
        Error
          (InvalidLength
             {
               field = field_def.name;
               min = min_length;
               max = max_length;
               length = len;
             })
  (* Integer validation *)
  | Integer { min; max }, `Int i ->
      let check_min = match min with None -> true | Some m -> i >= m in
      let check_max = match max with None -> true | Some m -> i <= m in
      if check_min && check_max then Ok json_value
      else
        Error
          (OutOfRange
             {
               field = field_def.name;
               min = Option.map float_of_int min;
               max = Option.map float_of_int max;
               value = float_of_int i;
             })
  (* Float validation *)
  | Float { min; max }, `Float f ->
      let check_min = match min with None -> true | Some m -> f >= m in
      let check_max = match max with None -> true | Some m -> f <= m in
      if check_min && check_max then Ok json_value
      else Error (OutOfRange { field = field_def.name; min; max; value = f })
  (* Boolean validation *)
  | Boolean, `Bool _ -> Ok json_value
  (* Array validation *)
  | Array { item_type = _; min_items; max_items }, `List items ->
      let len = List.length items in
      let check_min =
        match min_items with None -> true | Some min -> len >= min
      in
      let check_max =
        match max_items with None -> true | Some max -> len <= max
      in
      if check_min && check_max then
        (* TODO: Validate individual items against item_type *)
        Ok json_value
      else
        Error
          (InvalidLength
             {
               field = field_def.name;
               min = min_items;
               max = max_items;
               length = len;
             })
  (* Enum validation *)
  | Enum allowed_values, `String s ->
      if List.mem s allowed_values then Ok json_value
      else
        Error
          (InvalidFormat
             {
               field = field_def.name;
               reason =
                 Printf.sprintf "Value '%s' not in allowed values: [%s]" s
                   (String.concat ", " allowed_values);
             })
  (* DateTime validation (basic) *)
  | DateTime { format = _ }, `String _ ->
      (* TODO: Implement proper datetime validation *)
      Ok json_value
  (* Any type accepts anything *)
  | Any, _ -> Ok json_value
  (* Type mismatch *)
  | field_type, json_val ->
      Error
        (InvalidType
           {
             field = field_def.name;
             expected = expected_type_name field_type;
             got = type_name_of_json json_val;
           })

(** Main validation function *)
let validate schema (json : Yojson.Safe.t) =
  let json_fields = match json with `Assoc fields -> fields | _ -> [] in
  let errors = ref [] in
  let warnings = ref [] in

  (* Check each field in schema *)
  List.iter
    (fun (field_def : field_definition) ->
      match List.assoc_opt field_def.name json_fields with
      | None when field_def.required ->
          errors :=
            {
              field_path = [ field_def.name ];
              error = MissingRequiredField field_def.name;
              message =
                string_of_field_error (MissingRequiredField field_def.name);
            }
            :: !errors
      | None ->
          (* Optional field missing is OK *)
          ()
      | Some value -> (
          match validate_field field_def value with
          | Error err ->
              errors :=
                {
                  field_path = [ field_def.name ];
                  error = err;
                  message = string_of_field_error err;
                }
                :: !errors
          | Ok _ ->
              (* Field is valid *)
              ()))
    schema.fields;

  (* Check for unknown fields if schema is strict *)
  if schema.strict then
    List.iter
      (fun (field_name, _) ->
        if
          not
            (List.exists
               (fun (def : Types.Schema.field_definition) ->
                 def.name = field_name)
               schema.fields)
        then warnings := UnknownField field_name :: !warnings)
      json_fields;

  (* Return result based on errors and warnings *)
  match !errors, !warnings with
  | [], [] ->
      (* For now, return a simple validated request - we'll enhance this later *)
      let validated_req =
        {
          original =
            {
              method_type = GET;
              (* placeholder *)
              path = [];
              query_params = [];
              headers = [];
              body = Some json;
              metadata =
                {
                  request_id = "temp";
                  timestamp = Unix.gettimeofday ();
                  client_ip = "127.0.0.1";
                  user_agent = None;
                  correlation_id = None;
                };
            };
          schema_version = schema.version;
          validated_body = json;
          required_fields =
            List.filter_map
              (fun def -> if def.required then Some def.name else None)
              schema.fields;
        }
      in
      Valid validated_req
  | [], warnings ->
      let validated_req =
        {
          original =
            {
              method_type = GET;
              path = [];
              query_params = [];
              headers = [];
              body = Some json;
              metadata =
                {
                  request_id = "temp";
                  timestamp = Unix.gettimeofday ();
                  client_ip = "127.0.0.1";
                  user_agent = None;
                  correlation_id = None;
                };
            };
          schema_version = schema.version;
          validated_body = json;
          required_fields =
            List.filter_map
              (fun def -> if def.required then Some def.name else None)
              schema.fields;
        }
      in
      ValidWithWarnings (validated_req, List.rev warnings)
  | errors, _ -> Invalid (List.rev errors)

(** Helper function to create a simple user schema for testing *)
let create_user_schema () =
  {
    version = "v1";
    name = "User";
    fields =
      [
        {
          name = "name";
          field_type =
            String
              { min_length = Some 1; max_length = Some 100; pattern = None };
          required = true;
          default = None;
          description = Some "User's full name";
        };
        {
          name = "email";
          field_type =
            String { min_length = None; max_length = None; pattern = None };
          required = true;
          default = None;
          description = Some "User's email address";
        };
        {
          name = "age";
          field_type = Integer { min = Some 0; max = Some 150 };
          required = false;
          default = None;
          description = Some "User's age";
        };
      ];
    strict = false;
  }
