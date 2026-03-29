(** Test Assertions

    Extracted from Testing.Assert. *)

exception Assertion_error of string

(** Assertion failed *)
let fail msg = raise (Assertion_error msg)

(** Assert equal *)
let equal ?(msg = "") expected actual =
  if expected <> actual
  then
    fail
      (Printf.sprintf
         "%sExpected %s but got %s"
         (if msg = "" then "" else msg ^ ": ")
         (Yojson.Safe.to_string expected)
         (Yojson.Safe.to_string actual))
;;

(** Assert true *)
let is_true ?(msg = "Expected true") value = if not value then fail msg

(** Assert false *)
let is_false ?(msg = "Expected false") value = if value then fail msg

(** Assert status *)
let status expected resp =
  if Testing_response.status resp <> expected
  then
    fail
      (Printf.sprintf
         "Expected status %d but got %d"
         (Http.Status.to_int expected)
         (Testing_response.status_code resp))
;;

(** Assert status code *)
let status_code expected resp =
  if Testing_response.status_code resp <> expected
  then
    fail
      (Printf.sprintf
         "Expected status %d but got %d"
         expected
         (Testing_response.status_code resp))
;;

(** Assert success (2xx) *)
let success resp =
  if not (Testing_response.is_success resp)
  then
    fail
      (Printf.sprintf
         "Expected success (2xx) but got %d"
         (Testing_response.status_code resp))
;;

(** Assert header exists *)
let header_exists name resp =
  if Option.is_none (Testing_response.header name resp)
  then fail (Printf.sprintf "Expected header '%s' to exist" name)
;;

(** Assert header value *)
let header name expected resp =
  match Testing_response.header name resp with
  | None -> fail (Printf.sprintf "Expected header '%s' to exist" name)
  | Some actual ->
    if actual <> expected
    then
      fail
        (Printf.sprintf
           "Expected header '%s' to be '%s' but got '%s'"
           name
           expected
           actual)
;;

(** Assert header contains *)
let header_contains name substring resp =
  match Testing_response.header name resp with
  | None -> fail (Printf.sprintf "Expected header '%s' to exist" name)
  | Some actual ->
    if
      not
        (String.length actual >= String.length substring
         &&
         try
           ignore (Str.search_forward (Str.regexp_string substring) actual 0);
           true
         with
         | Not_found -> false)
    then
      fail
        (Printf.sprintf
           "Expected header '%s' to contain '%s' but got '%s'"
           name
           substring
           actual)
;;

(** Assert body equals *)
let body expected resp =
  let actual = Testing_response.body resp in
  if actual <> expected
  then fail (Printf.sprintf "Expected body '%s' but got '%s'" expected actual)
;;

(** Assert body contains *)
let body_contains substring resp =
  let actual = Testing_response.body resp in
  if
    not
      (try
         ignore (Str.search_forward (Str.regexp_string substring) actual 0);
         true
       with
       | Not_found -> false)
  then fail (Printf.sprintf "Expected body to contain '%s'" substring)
;;

(** Assert JSON body *)
let json expected resp =
  match Testing_response.json resp with
  | None -> fail "Expected JSON response body"
  | Some actual ->
    if actual <> expected
    then
      fail
        (Printf.sprintf
           "Expected JSON %s but got %s"
           (Yojson.Safe.to_string expected)
           (Yojson.Safe.to_string actual))
;;

(** Assert JSON path value *)
let json_path path expected resp =
  match Testing_response.json resp with
  | None -> fail "Expected JSON response body"
  | Some json ->
    (match Testing_json_path.get path json with
     | None -> fail (Printf.sprintf "JSON path '%s' not found" path)
     | Some actual ->
       if actual <> expected
       then
         fail
           (Printf.sprintf
              "Expected JSON path '%s' to be %s but got %s"
              path
              (Yojson.Safe.to_string expected)
              (Yojson.Safe.to_string actual)))
;;

(** Assert JSON path exists *)
let json_path_exists path resp =
  match Testing_response.json resp with
  | None -> fail "Expected JSON response body"
  | Some json ->
    if not (Testing_json_path.exists path json)
    then fail (Printf.sprintf "Expected JSON path '%s' to exist" path)
;;

(** Assert JSON path is string *)
let json_path_string path expected resp = json_path path (`String expected) resp

(** Assert JSON path is int *)
let json_path_int path expected resp = json_path path (`Int expected) resp

(** Assert JSON path is bool *)
let json_path_bool path expected resp = json_path path (`Bool expected) resp

(** Assert redirect to *)
let redirect_to expected resp =
  if not (Testing_response.is_redirect resp)
  then
    fail
      (Printf.sprintf "Expected redirect but got %d" (Testing_response.status_code resp));
  match Testing_response.header "Location" resp with
  | None -> fail "Expected Location header"
  | Some location ->
    if location <> expected
    then fail (Printf.sprintf "Expected redirect to '%s' but got '%s'" expected location)
;;

(** Assert content type *)
let content_type expected resp =
  match Testing_response.header "Content-Type" resp with
  | None -> fail "Expected Content-Type header"
  | Some ct ->
    let actual = String.split_on_char ';' ct |> List.hd |> String.trim in
    if actual <> expected
    then fail (Printf.sprintf "Expected Content-Type '%s' but got '%s'" expected actual)
;;

(** Assert JSON content type *)
let is_json resp = content_type "application/json" resp

(** Assert HTML content type *)
let is_html resp = content_type "text/html" resp
