(** Test Request Builder

    Extracted from Testing.Test_request. *)

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
