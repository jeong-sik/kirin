(** Testing Utilities (Phase 17)

    Test helpers, mock servers, and HTTP client testing.

    {b Quick Start:}
    {[
      open Kirin
      open Testing

      let%test "GET /users returns 200" =
        let req = Test_request.get "/users" in
        let resp = Test.call routes req in
        Test.assert_status `OK resp;
        Test.assert_json_path ".[0].name" (`String "Alice") resp

      let%test "POST /users creates user" =
        let req = Test_request.post "/users"
          ~body:{|{"name": "Bob", "email": "bob@example.com"}|}
          ~content_type:"application/json" in
        let resp = Test.call routes req in
        Test.assert_status `Created resp
    ]}
*)

(** {1 Test Request Builder} *)

module Test_request = struct
  include Testing_request
end

(** {1 Test Response} *)

module Test_response = struct
  include Testing_response
end

(** {1 JSON Path} *)

module Json_path = struct
  include Testing_json_path
end

(** {1 Assertions} *)

module Assert = struct
  include Testing_assert
end

(** {1 Mock Server} *)

module Mock = struct
  include Testing_mock
end

(** {1 Test Utilities} *)

(** Run test and return result *)
let run_test name f =
  try
    f ();
    Printf.printf "\xe2\x9c\x93 %s\n" name;
    true
  with
  | Assert.Assertion_error msg ->
    Printf.printf "\xe2\x9c\x97 %s: %s\n" name msg;
    false
  | exn ->
    Printf.printf "\xe2\x9c\x97 %s: Exception: %s\n" name (Printexc.to_string exn);
    false

(** Run multiple tests *)
let run_tests tests =
  let results = List.map (fun (name, f) -> run_test name f) tests in
  let passed = List.filter (fun x -> x) results |> List.length in
  let total = List.length results in
  Printf.printf "\n%d/%d tests passed\n" passed total;
  passed = total

(** {1 Fixture Helpers} *)

(** Generate random string *)
let random_string ?(length = 8) () =
  let chars = "abcdefghijklmnopqrstuvwxyz0123456789" in
  String.init length (fun _ -> chars.[Random.int (String.length chars)])

(** Generate random email *)
let random_email () =
  Printf.sprintf "%s@example.com" (random_string ())

(** Generate random int in range *)
let random_int ~min ~max () =
  min + Random.int (max - min + 1)

(** Temp directory for test files *)
let with_temp_dir f =
  let dir = Filename.concat (Filename.get_temp_dir_name ())
    ("kirin_test_" ^ random_string ()) in
  Unix.mkdir dir 0o755;
  Fun.protect ~finally:(fun () ->
    (* Clean up temp directory *)
    Array.iter (fun file ->
      Sys.remove (Filename.concat dir file)
    ) (Sys.readdir dir);
    Unix.rmdir dir
  ) (fun () -> f dir)

(** Temp file for test *)
let with_temp_file ?content f =
  let path = Filename.temp_file "kirin_test_" ".tmp" in
  Fun.protect ~finally:(fun () -> Sys.remove path) (fun () ->
    (match content with
     | None -> ()
     | Some c -> Fs_compat.save path c);
    f path
  )
