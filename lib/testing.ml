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
  (** Build a test request *)
  type t = {
    meth : Http.Method.t;
    path : string;
    headers : (string * string) list;
    body : string;
    query : (string * string) list;
  }

  (** Create empty request *)
  let empty = {
    meth = `GET;
    path = "/";
    headers = [];
    body = "";
    query = [];
  }

  (** Set method *)
  let with_method meth req = { req with meth }

  (** Set path *)
  let with_path path req = { req with path }

  (** Add header *)
  let with_header name value req =
    { req with headers = (name, value) :: req.headers }

  (** Set headers *)
  let with_headers headers req = { req with headers }

  (** Set body *)
  let with_body body req = { req with body }

  (** Set JSON body *)
  let with_json_body json req =
    { req with
      body = Yojson.Safe.to_string json;
      headers = ("Content-Type", "application/json") :: req.headers
    }

  (** Add query parameter *)
  let with_query name value req =
    { req with query = (name, value) :: req.query }

  (** Set query parameters *)
  let with_queries query req = { req with query }

  (** GET request *)
  let get ?headers ?query path =
    let req = { empty with meth = `GET; path } in
    let req = match headers with None -> req | Some h -> with_headers h req in
    match query with None -> req | Some q -> with_queries q req

  (** POST request *)
  let post ?headers ?body ?content_type path =
    let req = { empty with meth = `POST; path } in
    let req = match headers with None -> req | Some h -> with_headers h req in
    let req = match body with None -> req | Some b -> with_body b req in
    match content_type with
    | None -> req
    | Some ct -> with_header "Content-Type" ct req

  (** POST JSON request *)
  let post_json ?headers path json =
    let req = { empty with meth = `POST; path } in
    let req = match headers with None -> req | Some h -> with_headers h req in
    with_json_body json req

  (** PUT request *)
  let put ?headers ?body ?content_type path =
    let req = { empty with meth = `PUT; path } in
    let req = match headers with None -> req | Some h -> with_headers h req in
    let req = match body with None -> req | Some b -> with_body b req in
    match content_type with
    | None -> req
    | Some ct -> with_header "Content-Type" ct req

  (** PUT JSON request *)
  let put_json ?headers path json =
    let req = { empty with meth = `PUT; path } in
    let req = match headers with None -> req | Some h -> with_headers h req in
    with_json_body json req

  (** DELETE request *)
  let delete ?headers path =
    let req = { empty with meth = `DELETE; path } in
    match headers with None -> req | Some h -> with_headers h req

  (** PATCH request *)
  let patch ?headers ?body path =
    let req = { empty with meth = `PATCH; path } in
    let req = match headers with None -> req | Some h -> with_headers h req in
    match body with None -> req | Some b -> with_body b req

  (** Set Authorization header *)
  let with_bearer_token token req =
    with_header "Authorization" ("Bearer " ^ token) req

  (** Set Basic Auth *)
  let with_basic_auth username password req =
    let credentials = Base64.encode_string (username ^ ":" ^ password) in
    with_header "Authorization" ("Basic " ^ credentials) req

  (** Set Accept header *)
  let with_accept content_type req =
    with_header "Accept" content_type req

  (** Set cookie *)
  let with_cookie name value req =
    let existing = List.assoc_opt "Cookie" req.headers in
    let cookie_value = match existing with
      | None -> name ^ "=" ^ value
      | Some existing -> existing ^ "; " ^ name ^ "=" ^ value
    in
    with_header "Cookie" cookie_value req
end

(** {1 Test Response} *)

module Test_response = struct
  (** Test response *)
  type t = {
    status : Http.Status.t;
    headers : (string * string) list;
    body : string;
  }

  (** Get status *)
  let status resp = resp.status

  (** Get status code as int *)
  let status_code resp = Http.Status.to_int resp.status

  (** Get header *)
  let header name resp =
    List.assoc_opt (String.lowercase_ascii name)
      (List.map (fun (k, v) -> (String.lowercase_ascii k, v)) resp.headers)

  (** Get all headers *)
  let headers resp = resp.headers

  (** Get body as string *)
  let body resp = resp.body

  (** Get body as JSON *)
  let json resp =
    try Some (Yojson.Safe.from_string resp.body)
    with _ -> None

  (** Check if response is success (2xx) *)
  let is_success resp =
    let code = Http.Status.to_int resp.status in
    code >= 200 && code < 300

  (** Check if response is redirect (3xx) *)
  let is_redirect resp =
    let code = Http.Status.to_int resp.status in
    code >= 300 && code < 400

  (** Check if response is client error (4xx) *)
  let is_client_error resp =
    let code = Http.Status.to_int resp.status in
    code >= 400 && code < 500

  (** Check if response is server error (5xx) *)
  let is_server_error resp =
    let code = Http.Status.to_int resp.status in
    code >= 500 && code < 600
end

(** {1 JSON Path} *)

module Json_path = struct
  (** Get value at JSON path (e.g., "user.name" or "users[0].id") *)
  let get path json =
    let segments = String.split_on_char '.' path in
    let rec traverse segments json =
      match segments, json with
      | [], v -> Some v
      | seg :: rest, `Assoc fields ->
        (* Check for array index notation: field[0] *)
        if String.contains seg '[' then begin
          let bracket_pos = String.index seg '[' in
          let field = String.sub seg 0 bracket_pos in
          let idx_str = String.sub seg (bracket_pos + 1) (String.length seg - bracket_pos - 2) in
          let idx = int_of_string idx_str in
          match List.assoc_opt field fields with
          | Some (`List items) when idx < List.length items ->
            traverse rest (List.nth items idx)
          | _ -> None
        end else begin
          match List.assoc_opt seg fields with
          | Some v -> traverse rest v
          | None -> None
        end
      | seg :: rest, `List items when String.length seg > 0 && seg.[0] = '[' ->
        (* Array index: [0] *)
        let idx_str = String.sub seg 1 (String.length seg - 2) in
        let idx = int_of_string idx_str in
        if idx < List.length items then traverse rest (List.nth items idx)
        else None
      | _ -> None
    in
    traverse segments json

  (** Check if path exists *)
  let exists path json = Option.is_some (get path json)

  (** Get string at path *)
  let get_string path json =
    match get path json with
    | Some (`String s) -> Some s
    | _ -> None

  (** Get int at path *)
  let get_int path json =
    match get path json with
    | Some (`Int i) -> Some i
    | _ -> None

  (** Get bool at path *)
  let get_bool path json =
    match get path json with
    | Some (`Bool b) -> Some b
    | _ -> None

  (** Get list at path *)
  let get_list path json =
    match get path json with
    | Some (`List l) -> Some l
    | _ -> None
end

(** {1 Assertions} *)

module Assert = struct
  exception Assertion_error of string

  (** Assertion failed *)
  let fail msg = raise (Assertion_error msg)

  (** Assert equal *)
  let equal ?(msg = "") expected actual =
    if expected <> actual then
      fail (Printf.sprintf "%sExpected %s but got %s"
        (if msg = "" then "" else msg ^ ": ")
        (Yojson.Safe.to_string expected)
        (Yojson.Safe.to_string actual))

  (** Assert true *)
  let is_true ?(msg = "Expected true") value =
    if not value then fail msg

  (** Assert false *)
  let is_false ?(msg = "Expected false") value =
    if value then fail msg

  (** Assert status *)
  let status expected resp =
    if Test_response.status resp <> expected then
      fail (Printf.sprintf "Expected status %d but got %d"
        (Http.Status.to_int expected)
        (Test_response.status_code resp))

  (** Assert status code *)
  let status_code expected resp =
    if Test_response.status_code resp <> expected then
      fail (Printf.sprintf "Expected status %d but got %d"
        expected (Test_response.status_code resp))

  (** Assert success (2xx) *)
  let success resp =
    if not (Test_response.is_success resp) then
      fail (Printf.sprintf "Expected success (2xx) but got %d"
        (Test_response.status_code resp))

  (** Assert header exists *)
  let header_exists name resp =
    if Option.is_none (Test_response.header name resp) then
      fail (Printf.sprintf "Expected header '%s' to exist" name)

  (** Assert header value *)
  let header name expected resp =
    match Test_response.header name resp with
    | None -> fail (Printf.sprintf "Expected header '%s' to exist" name)
    | Some actual ->
      if actual <> expected then
        fail (Printf.sprintf "Expected header '%s' to be '%s' but got '%s'"
          name expected actual)

  (** Assert header contains *)
  let header_contains name substring resp =
    match Test_response.header name resp with
    | None -> fail (Printf.sprintf "Expected header '%s' to exist" name)
    | Some actual ->
      if not (String.length actual >= String.length substring &&
              try ignore (Str.search_forward (Str.regexp_string substring) actual 0); true
              with Not_found -> false) then
        fail (Printf.sprintf "Expected header '%s' to contain '%s' but got '%s'"
          name substring actual)

  (** Assert body equals *)
  let body expected resp =
    let actual = Test_response.body resp in
    if actual <> expected then
      fail (Printf.sprintf "Expected body '%s' but got '%s'"
        expected actual)

  (** Assert body contains *)
  let body_contains substring resp =
    let actual = Test_response.body resp in
    if not (try ignore (Str.search_forward (Str.regexp_string substring) actual 0); true
            with Not_found -> false) then
      fail (Printf.sprintf "Expected body to contain '%s'" substring)

  (** Assert JSON body *)
  let json expected resp =
    match Test_response.json resp with
    | None -> fail "Expected JSON response body"
    | Some actual ->
      if actual <> expected then
        fail (Printf.sprintf "Expected JSON %s but got %s"
          (Yojson.Safe.to_string expected)
          (Yojson.Safe.to_string actual))

  (** Assert JSON path value *)
  let json_path path expected resp =
    match Test_response.json resp with
    | None -> fail "Expected JSON response body"
    | Some json ->
      match Json_path.get path json with
      | None -> fail (Printf.sprintf "JSON path '%s' not found" path)
      | Some actual ->
        if actual <> expected then
          fail (Printf.sprintf "Expected JSON path '%s' to be %s but got %s"
            path
            (Yojson.Safe.to_string expected)
            (Yojson.Safe.to_string actual))

  (** Assert JSON path exists *)
  let json_path_exists path resp =
    match Test_response.json resp with
    | None -> fail "Expected JSON response body"
    | Some json ->
      if not (Json_path.exists path json) then
        fail (Printf.sprintf "Expected JSON path '%s' to exist" path)

  (** Assert JSON path is string *)
  let json_path_string path expected resp =
    json_path path (`String expected) resp

  (** Assert JSON path is int *)
  let json_path_int path expected resp =
    json_path path (`Int expected) resp

  (** Assert JSON path is bool *)
  let json_path_bool path expected resp =
    json_path path (`Bool expected) resp

  (** Assert redirect to *)
  let redirect_to expected resp =
    if not (Test_response.is_redirect resp) then
      fail (Printf.sprintf "Expected redirect but got %d"
        (Test_response.status_code resp));
    match Test_response.header "Location" resp with
    | None -> fail "Expected Location header"
    | Some location ->
      if location <> expected then
        fail (Printf.sprintf "Expected redirect to '%s' but got '%s'"
          expected location)

  (** Assert content type *)
  let content_type expected resp =
    match Test_response.header "Content-Type" resp with
    | None -> fail "Expected Content-Type header"
    | Some ct ->
      let actual = String.split_on_char ';' ct |> List.hd |> String.trim in
      if actual <> expected then
        fail (Printf.sprintf "Expected Content-Type '%s' but got '%s'"
          expected actual)

  (** Assert JSON content type *)
  let is_json resp =
    content_type "application/json" resp

  (** Assert HTML content type *)
  let is_html resp =
    content_type "text/html" resp
end

(** {1 Mock Server} *)

module Mock = struct
  (** Mock endpoint *)
  type endpoint = {
    meth : Http.Method.t option;  (* None = any method *)
    path : string;
    response : Test_response.t;
    mutable call_count : int;
  }

  (** Mock server *)
  type t = {
    mutable endpoints : endpoint list;
    mutable unmatched_requests : Test_request.t list;
  }

  (** Create mock server *)
  let create () = { endpoints = []; unmatched_requests = [] }

  (** Add mock endpoint *)
  let on ?(meth = None) ~path ~status ?(headers = []) ?(body = "") t =
    let endpoint = {
      meth;
      path;
      response = { Test_response.status; headers; body };
      call_count = 0;
    } in
    t.endpoints <- endpoint :: t.endpoints;
    t

  (** Add mock endpoint with JSON response *)
  let on_json ?(meth = None) ~path ~status ?(headers = []) json t =
    let body = Yojson.Safe.to_string json in
    let headers = ("Content-Type", "application/json") :: headers in
    on ~meth ~path ~status ~headers ~body t

  (** Handle request *)
  let handle t req =
    let matching = List.find_opt (fun ep ->
      ep.path = req.Test_request.path &&
      (Option.is_none ep.meth || ep.meth = Some req.meth)
    ) t.endpoints in
    match matching with
    | Some ep ->
      ep.call_count <- ep.call_count + 1;
      ep.response
    | None ->
      t.unmatched_requests <- req :: t.unmatched_requests;
      { Test_response.status = `Not_found; headers = []; body = "Not Found" }

  (** Get call count for endpoint *)
  let call_count ~path t =
    match List.find_opt (fun ep -> ep.path = path) t.endpoints with
    | Some ep -> ep.call_count
    | None -> 0

  (** Get unmatched requests *)
  let unmatched t = t.unmatched_requests

  (** Reset mock server *)
  let reset t =
    List.iter (fun ep -> ep.call_count <- 0) t.endpoints;
    t.unmatched_requests <- []

  (** Assert endpoint was called *)
  let assert_called ~path t =
    if call_count ~path t = 0 then
      Assert.fail (Printf.sprintf "Expected endpoint '%s' to be called" path)

  (** Assert endpoint was called n times *)
  let assert_called_times ~path ~times t =
    let actual = call_count ~path t in
    if actual <> times then
      Assert.fail (Printf.sprintf "Expected endpoint '%s' to be called %d times but was called %d times"
        path times actual)

  (** Assert no unmatched requests *)
  let assert_no_unmatched t =
    if t.unmatched_requests <> [] then
      Assert.fail (Printf.sprintf "Found %d unmatched requests"
        (List.length t.unmatched_requests))
end

(** {1 Test Utilities} *)

(** Run test and return result *)
let run_test name f =
  try
    f ();
    Printf.printf "✓ %s\n" name;
    true
  with
  | Assert.Assertion_error msg ->
    Printf.printf "✗ %s: %s\n" name msg;
    false
  | exn ->
    Printf.printf "✗ %s: Exception: %s\n" name (Printexc.to_string exn);
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
