(* Admin and health endpoints *)

let start_time = ref (Unix.gettimeofday ())
let set_start_time time = start_time := time

(* Health check endpoint *)
let health_check _request =
  let uptime = Unix.gettimeofday () -. !start_time in
  `Assoc
    [
      "status", `String "healthy";
      "service", `String "madkhal-api-gateway";
      "version", `String "1.0.0";
      "uptime_seconds", `Float uptime;
      "timestamp", `String (string_of_float (Unix.gettimeofday ()));
    ]
  |> Yojson.Safe.to_string
  |> Dream.json
