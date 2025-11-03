open Validation.Validator
open Types.Validation

let test_valid_user () =
  let schema = create_user_schema () in
  let json =
    `Assoc
      [
        "name", `String "Alice Smith";
        "email", `String "alice@example.com";
        "age", `Int 30;
      ]
  in

  match validate schema json with
  | Valid _ -> Printf.printf "Valid user test passed\n"
  | ValidWithWarnings (_, warnings) ->
      Printf.printf "Valid user test passed (with warnings)\n";
      List.iter
        (fun w ->
          Printf.printf "  Warning: %s\n"
            (match w with
            | UnknownField f -> "Unknown field: " ^ f
            | DeprecatedField f -> "Deprecated field: " ^ f
            | ImplicitCoercion { field; from_type; to_type } ->
                Printf.sprintf "Coerced %s from %s to %s" field from_type
                  to_type))
        warnings
  | Invalid errors ->
      Printf.printf "Valid user test failed with errors:\n";
      List.iter (fun err -> Printf.printf "  - %s\n" err.message) errors;
      exit 1

let test_missing_required () =
  let schema = create_user_schema () in
  let json =
    `Assoc [ "name", `String "Bob"; (* missing email *) "age", `Int 25 ]
  in

  match validate schema json with
  | Invalid errors ->
      Printf.printf "Missing required field test passed\n";
      List.iter
        (fun err -> Printf.printf "  Expected error: %s\n" err.message)
        errors
  | Valid _ ->
      Printf.printf
        "Missing required field test failed - should have been invalid\n";
      exit 1
  | ValidWithWarnings (_, _) ->
      Printf.printf
        "Missing required field test failed - should have been invalid, not \
         valid with warnings\n";
      exit 1

let test_type_mismatch () =
  let schema = create_user_schema () in
  let json =
    `Assoc
      [
        "name", `String "Charlie";
        "email", `String "charlie@example.com";
        "age", `String "thirty" (* wrong type *);
      ]
  in

  match validate schema json with
  | Invalid errors ->
      Printf.printf "Type mismatch test passed\n";
      List.iter
        (fun err -> Printf.printf "  Expected error: %s\n" err.message)
        errors
  | Valid _ ->
      Printf.printf "Type mismatch test failed - should have been invalid\n";
      exit 1
  | ValidWithWarnings (_, _) ->
      Printf.printf
        "Type mismatch test failed - should have been invalid, not valid with \
         warnings\n";
      exit 1

let test_out_of_range () =
  let schema = create_user_schema () in
  let json =
    `Assoc
      [
        "name", `String "Dave";
        "email", `String "dave@example.com";
        "age", `Int 200 (* out of range *);
      ]
  in

  match validate schema json with
  | Invalid errors ->
      Printf.printf "Out of range test passed\n";
      List.iter
        (fun err -> Printf.printf "  Expected error: %s\n" err.message)
        errors
  | Valid _ ->
      Printf.printf "Out of range test failed - should have been invalid\n";
      exit 1
  | ValidWithWarnings (_, _) ->
      Printf.printf
        "Out of range test failed - should have been invalid, not valid with \
         warnings\n";
      exit 1

let () =
  Printf.printf "Running validator tests...\n\n";
  test_valid_user ();
  test_missing_required ();
  test_type_mismatch ();
  test_out_of_range ();
  Printf.printf "\nAll tests passed!\n"
