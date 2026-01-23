# Kirin API Reference

Complete API documentation for the Kirin OCaml web framework.

## Table of Contents

- [Core Types](#core-types)
- [Server](#server)
- [Request](#request)
- [Response](#response)
- [Router](#router)
- [Middleware](#middleware)
- [Static Files](#static-files)
- [Cookies](#cookies)
- [Templates](#templates)
- [WebSocket](#websocket)
- [Server-Sent Events](#server-sent-events)
- [Multipart Forms](#multipart-forms)
- [Compression](#compression)
- [Rate Limiting](#rate-limiting)
- [ETag](#etag)

---

## Core Types

```ocaml
(** Handler: Direct-style function from request to response *)
type handler = Request.t -> Response.t

(** Middleware: Handler transformer *)
type middleware = handler -> handler

(** Route definition *)
type route = Router.route
```

---

## Server

### `Kirin.start`

Start the HTTP server.

```ocaml
val start : ?port:int -> handler -> unit
```

**Parameters:**
- `?port` - Port number (default: 8000)
- `handler` - Request handler (typically a router)

**Example:**
```ocaml
let () = Kirin.start ~port:3000
  @@ Kirin.logger
  @@ Kirin.router [...]
```

### `Kirin.run`

Run server with custom configuration.

```ocaml
val run : ?config:config -> sw:Eio.Switch.t -> env:Eio_main.stdenv -> handler -> unit
```

### Configuration Types

```ocaml
type config = {
  port : int;      (* Default: 8000 *)
  host : string;   (* Default: "0.0.0.0" *)
  backlog : int;   (* Default: 128 *)
}

val default_config : config
```

---

## Request

### Request Type

```ocaml
module Request : sig
  type t = {
    meth : Http.Method.t;
    uri : Uri.t;
    headers : Http.Header.t;
    body : string;
    params : (string * string) list;
    raw : Http.Request.t;
  }
end
```

### Path Parameters

```ocaml
(** Get path parameter, raises if not found *)
val param : string -> Request.t -> string

(** Get optional path parameter *)
val param_opt : string -> Request.t -> string option
```

**Example:**
```ocaml
Kirin.get "/users/:id" (fun req ->
  let id = Kirin.param "id" req in
  let name = Kirin.param_opt "name" req in
  (* ... *))
```

### Query Parameters

```ocaml
(** Get query parameter, raises if not found *)
val query : string -> Request.t -> string

(** Get optional query parameter *)
val query_opt : string -> Request.t -> string option
```

**Example:**
```ocaml
(* GET /search?q=hello&limit=10 *)
let q = Kirin.query "q" req         (* "hello" *)
let limit = Kirin.query_opt "limit" req  (* Some "10" *)
```

### Headers

```ocaml
(** Get header value *)
val header : string -> Request.t -> string option
```

**Example:**
```ocaml
let auth = Kirin.header "Authorization" req
let content_type = Kirin.header "Content-Type" req
```

### Body

```ocaml
(** Get raw body as string *)
val body : Request.t -> string

(** Parse body as JSON *)
val json_body : Request.t -> (Yojson.Safe.t, [`Json_parse_error of string]) result

(** Parse body as form data *)
val form_body : Request.t -> (string * string list) list
```

**Example:**
```ocaml
let raw = Kirin.body req

match Kirin.json_body req with
| Ok json -> (* ... *)
| Error (`Json_parse_error msg) -> Kirin.bad_request ()

let form = Kirin.form_body req
let email = List.assoc "email" form |> List.hd
```

---

## Response

### Response Type

```ocaml
module Response : sig
  type t = {
    status : Http.Status.t;
    headers : Http.Header.t;
    body : string;
  }
end
```

### Basic Responses

```ocaml
(** Plain text response *)
val text : ?status:Http.Status.t -> string -> Response.t

(** HTML response *)
val html : ?status:Http.Status.t -> ?doctype:bool -> string -> Response.t

(** JSON response from Yojson.Safe.t *)
val json : ?status:Http.Status.t -> Yojson.Safe.t -> Response.t

(** JSON response from string *)
val json_string : ?status:Http.Status.t -> string -> Response.t

(** Empty response with status *)
val empty : Http.Status.t -> Response.t
```

**Examples:**
```ocaml
Kirin.text "Hello, World!"
Kirin.html "<h1>Welcome</h1>"
Kirin.html ~doctype:true "<html>...</html>"
Kirin.json (`Assoc [("status", `String "ok")])
Kirin.json_string {|{"ready": true}|}
Kirin.empty `No_content
```

### Error Responses

```ocaml
(** 404 Not Found *)
val not_found : ?body:string -> unit -> Response.t

(** 400 Bad Request *)
val bad_request : ?body:string -> unit -> Response.t

(** 500 Internal Server Error *)
val server_error : ?body:string -> unit -> Response.t
```

### Redirects

```ocaml
(** Redirect (302 Found) *)
val redirect : ?status:Http.Status.t -> string -> Response.t

(** Permanent redirect (301 Moved Permanently) *)
val redirect_permanent : string -> Response.t
```

**Example:**
```ocaml
Kirin.redirect "/dashboard"
Kirin.redirect ~status:`See_other "/login"
Kirin.redirect_permanent "/new-url"
```

### Response Modifiers

```ocaml
(** Add header to response *)
val with_header : string -> string -> Response.t -> Response.t

(** Set response status *)
val with_status : Http.Status.t -> Response.t -> Response.t
```

**Example:**
```ocaml
Kirin.json data
|> Kirin.with_header "X-Request-ID" "abc123"
|> Kirin.with_status `Created
```

### HTMX Support

```ocaml
(** HTMX-specific response with swap headers *)
val htmx : ?status:Http.Status.t -> ?target:string -> ?swap:string -> string -> Response.t
```

**Example:**
```ocaml
Kirin.htmx ~target:"#content" ~swap:"innerHTML" "<p>Updated!</p>"
(* Sets HX-Retarget and HX-Reswap headers *)
```

### Response Inspection

```ocaml
(** Get response body *)
val response_body : Response.t -> string

(** Get response status code *)
val response_status : Response.t -> int

(** Get response header *)
val response_header : string -> Response.t -> string option
```

---

## Router

### Route Constructors

```ocaml
val get : string -> handler -> route
val post : string -> handler -> route
val put : string -> handler -> route
val patch : string -> handler -> route
val delete : string -> handler -> route
val head : string -> handler -> route
val options : string -> handler -> route
```

### Router

```ocaml
(** Create router from routes *)
val router : route list -> handler

(** Dispatch request to routes *)
val dispatch : route list -> Request.t -> Response.t
```

### Scoped Routes

```ocaml
(** Create scoped routes with prefix and middlewares *)
val scope : string -> middleware list -> route list -> route list
```

**Example:**
```ocaml
let api_routes = Kirin.scope "/api/v1" [auth_middleware; json_middleware] [
  Kirin.get "/users" list_users;
  Kirin.get "/users/:id" get_user;
  Kirin.post "/users" create_user;
]

let routes = Kirin.router (
  [Kirin.get "/" home] @ api_routes
)
```

### Route Patterns

| Pattern | Example | Matches |
|---------|---------|---------|
| Static | `/users` | Exact match |
| Param | `/users/:id` | `/users/123`, `/users/abc` |
| Multi-param | `/users/:id/posts/:pid` | `/users/1/posts/5` |
| Wildcard | `/files/*` | `/files/any/path/here` |

---

## Middleware

### Built-in Middleware

```ocaml
(** Logger - logs requests and responses *)
val logger : middleware

(** Timing - adds X-Response-Time header *)
val timing : middleware

(** CORS - Cross-Origin Resource Sharing *)
val cors : ?config:cors_config -> unit -> middleware

(** Error catcher *)
val catch : (exn -> Request.t -> Response.t) -> middleware

(** Compose middlewares into pipeline *)
val pipeline : middleware list -> middleware
```

### CORS Configuration

```ocaml
type cors_config = {
  origins : string list;      (* Allowed origins, ["*"] for any *)
  methods : string list;      (* Allowed HTTP methods *)
  headers : string list;      (* Allowed headers *)
  credentials : bool;         (* Allow credentials *)
  max_age : int option;       (* Preflight cache duration *)
}

val default_cors_config : cors_config
```

**Example:**
```ocaml
let cors_config = {
  Kirin.default_cors_config with
  origins = ["https://example.com"];
  credentials = true;
}

let () = Kirin.start ~port:8000
  @@ Kirin.cors ~config:cors_config ()
  @@ routes
```

### Custom Middleware

```ocaml
let my_middleware : Kirin.middleware = fun handler req ->
  (* Before handler *)
  let resp = handler req in
  (* After handler *)
  resp
```

**Example - Authentication:**
```ocaml
let auth_required : Kirin.middleware = fun handler req ->
  match Kirin.header "Authorization" req with
  | Some token when verify_token token -> handler req
  | _ -> Kirin.Response.make ~status:`Unauthorized "Unauthorized"
```

**Example - Request Logging:**
```ocaml
let log_requests : Kirin.middleware = fun handler req ->
  let start = Unix.gettimeofday () in
  let resp = handler req in
  let elapsed = Unix.gettimeofday () -. start in
  Printf.eprintf "%s %s %.3fms\n"
    (Http.Method.to_string (Request.meth req))
    (Request.path req)
    (elapsed *. 1000.0);
  resp
```

---

## Static Files

```ocaml
(** Static file middleware *)
val static : string -> dir:string -> middleware
```

**Parameters:**
- `string` - URL prefix (e.g., "/assets")
- `~dir` - Local directory path (e.g., "./public")

**Example:**
```ocaml
let () = Kirin.start ~port:8000
  @@ Kirin.static "/assets" ~dir:"./public"
  @@ Kirin.static "/uploads" ~dir:"./uploads"
  @@ routes

(* GET /assets/css/style.css -> ./public/css/style.css *)
(* GET /uploads/image.png -> ./uploads/image.png *)
```

**Features:**
- Automatic MIME type detection
- Directory index (serves `index.html` for directories)
- Cache headers (`Cache-Control: public, max-age=3600`)
- Security: Prevents directory traversal attacks
- HEAD request support

---

## Cookies

### Reading Cookies

```ocaml
(** Get cookie value *)
val cookie : string -> Request.t -> string option

(** Get all cookies *)
val cookies : Request.t -> (string * string) list

(** Get signed cookie (HMAC-verified) *)
val cookie_signed : string -> Request.t -> string option
```

### Setting Cookies

```ocaml
(** Set cookie on response *)
val set_cookie : ?attrs:Cookie.attributes -> string -> string -> Response.t -> Response.t

(** Delete cookie *)
val delete_cookie : ?path:string -> string -> Response.t -> Response.t

(** Set signed cookie *)
val set_cookie_signed : ?attrs:Cookie.attributes -> string -> string -> Response.t -> Response.t

(** Set cookie signing secret (call at startup) *)
val set_cookie_secret : string -> unit
```

### Cookie Attributes

```ocaml
type attributes = {
  max_age : int option;
  expires : string option;
  domain : string option;
  path : string option;           (* Default: "/" *)
  secure : bool;                  (* Default: false *)
  http_only : bool;               (* Default: true *)
  same_site : [`Strict | `Lax | `None] option;  (* Default: `Lax *)
}
```

**Example:**
```ocaml
(* Set secret at startup *)
let () = Kirin.set_cookie_secret "your-32-character-minimum-secret"

let login_handler req =
  let user_id = authenticate req in
  Kirin.html "Welcome!"
  |> Kirin.set_cookie_signed "session" user_id
  |> Kirin.set_cookie "preferences" "dark_mode"

let logout_handler _req =
  Kirin.redirect "/"
  |> Kirin.delete_cookie "session"

let protected_handler req =
  match Kirin.cookie_signed "session" req with
  | Some user_id -> serve_content user_id
  | None -> Kirin.redirect "/login"
```

---

## Templates

### Context

```ocaml
(** Template context type *)
type template_context = Yojson.Safe.t

(** Empty context *)
val template_context_empty : template_context

(** Create context from string pairs *)
val template_context : (string * string) list -> template_context

(** Create context with various types *)
val template_context_of : (string * Yojson.Safe.t) list -> template_context
```

### Rendering

```ocaml
(** Render template with context *)
val template_render : template_context -> string -> string

(** Render template to HTML response *)
val template_html : ?partials:(string -> string option) -> template_context -> string -> Response.t

(** Simple string interpolation *)
val template_interpolate : string -> (string * string) list -> string

(** HTML escape string *)
val html_escape : string -> string
```

### Template Syntax

| Syntax | Description |
|--------|-------------|
| `{{var}}` | HTML-escaped interpolation |
| `{{{var}}}` | Raw interpolation (no escaping) |
| `{{#if key}}...{{/if key}}` | Conditional |
| `{{#if key}}...{{else}}...{{/if key}}` | Conditional with else |
| `{{#unless key}}...{{/unless key}}` | Inverted conditional |
| `{{#each key}}...{{/each key}}` | Iteration |
| `{{user.name}}` | Dot notation |
| `{{! comment }}` | Comment (ignored) |
| `{{> partial}}` | Partial include |

### Iteration Variables

Inside `{{#each}}` blocks:
- `{{this}}` - Current item (for simple values)
- `{{@index}}` - Current index (0-based)
- `{{@first}}` - True if first item
- `{{@last}}` - True if last item

**Example:**
```ocaml
let template = {|
<ul>
{{#each users}}
  <li class="{{#if @first}}first{{/if @first}}">
    {{@index}}. {{name}} ({{email}})
  </li>
{{/each users}}
</ul>
|}

let ctx = Kirin.template_context_of [
  ("users", `List [
    `Assoc [("name", `String "Alice"); ("email", `String "alice@example.com")];
    `Assoc [("name", `String "Bob"); ("email", `String "bob@example.com")];
  ])
]

let resp = Kirin.template_html ctx template
```

---

## WebSocket

### Types

```ocaml
(** WebSocket opcodes *)
type ws_opcode =
  | Continuation
  | Text
  | Binary
  | Close
  | Ping
  | Pong

(** WebSocket frame *)
type ws_frame = {
  fin : bool;
  opcode : ws_opcode;
  payload : string;
}

(** Close codes *)
type ws_close_code =
  | Normal
  | GoingAway
  | ProtocolError
  | UnsupportedData
  | InvalidPayload
  | PolicyViolation
  | MessageTooBig
  | InternalError
```

### Detection and Upgrade

```ocaml
(** Check if request is WebSocket upgrade *)
val is_websocket_upgrade : Request.t -> bool

(** Create upgrade response *)
val websocket_upgrade : Request.t -> (Response.t, string) result

(** WebSocket middleware *)
val websocket : path:string -> handler:Websocket.handler -> middleware
```

### Frame Creation

```ocaml
(** Create text frame *)
val ws_text : ?fin:bool -> string -> ws_frame

(** Create binary frame *)
val ws_binary : ?fin:bool -> string -> ws_frame

(** Create ping frame *)
val ws_ping : ?payload:string -> unit -> ws_frame

(** Create pong frame *)
val ws_pong : payload:string -> ws_frame

(** Create close frame *)
val ws_close : ?code:ws_close_code -> ?reason:string -> unit -> ws_frame
```

### Frame Encoding/Decoding

```ocaml
(** Encode frame for sending *)
val ws_encode : ws_frame -> string

(** Decode received frame *)
val ws_decode : string -> (ws_frame * int, string) result
```

### WebSocket Handler

```ocaml
type handler = {
  on_open : unit -> unit;
  on_message : frame -> frame option;
  on_close : close_code option -> string -> unit;
  on_error : string -> unit;
}

val ws_echo_handler : handler  (* Default echo handler *)
```

**Example:**
```ocaml
let chat_handler = {
  Kirin.Websocket.on_open = (fun () ->
    print_endline "New connection");
  on_message = (fun frame ->
    match frame.opcode with
    | Kirin.Text ->
      broadcast_message frame.payload;
      None  (* Don't echo back *)
    | Kirin.Ping ->
      Some (Kirin.ws_pong ~payload:frame.payload)
    | _ -> None);
  on_close = (fun _ reason ->
    print_endline ("Disconnected: " ^ reason));
  on_error = (fun msg ->
    print_endline ("Error: " ^ msg));
}
```

---

## Server-Sent Events

### Event Type

```ocaml
type sse_event = {
  event : string option;  (* Event type *)
  data : string;          (* Event data *)
  id : string option;     (* Event ID *)
  retry : int option;     (* Reconnect interval (ms) *)
}
```

### Event Creation

```ocaml
(** Simple data event *)
val sse_data : string -> sse_event

(** Event with type *)
val sse_event : event_type:string -> string -> sse_event

(** Add ID *)
val sse_with_id : string -> sse_event -> sse_event

(** Add retry interval *)
val sse_with_retry : int -> sse_event -> sse_event

(** Encode event to SSE format *)
val sse_encode : sse_event -> string

(** Keep-alive ping *)
val sse_ping : unit -> string

(** Get Last-Event-ID header *)
val sse_last_id : Request.t -> string option
```

### Response

```ocaml
(** Create SSE response with events *)
val sse_response : sse_event list -> Response.t

(** SSE middleware *)
val sse : path:string -> on_events:(unit -> sse_event list) -> middleware
```

**Example:**
```ocaml
let notifications_handler _req =
  let events = [
    Kirin.sse_data "Connected to notifications";
    Kirin.sse_event ~event_type:"notification" "You have 3 new messages"
    |> Kirin.sse_with_id "notif-001"
    |> Kirin.sse_with_retry 3000;
  ] in
  Kirin.sse_response events

(* Or use middleware *)
let () = Kirin.start ~port:8000
  @@ Kirin.sse ~path:"/notifications" ~on_events:get_notifications
  @@ routes
```

**Client JavaScript:**
```javascript
const events = new EventSource('/notifications');

events.onmessage = (e) => console.log('Message:', e.data);
events.addEventListener('notification', (e) => {
  console.log('Notification:', e.data);
  console.log('ID:', e.lastEventId);
});
events.onerror = () => console.log('Connection lost, reconnecting...');
```

---

## Multipart Forms

### Types

```ocaml
type part = {
  name : string;
  filename : string option;
  content_type : string option;
  content : string;
}

type t = { parts : part list }
```

### Parsing

```ocaml
(** Parse multipart from request *)
val multipart : Request.t -> t option

(** Get field value *)
val multipart_field : string -> t -> string option

(** Get uploaded file *)
val multipart_file : string -> t -> part option

(** Get all files *)
val multipart_files : t -> part list

(** Get all fields (non-file parts) *)
val multipart_fields : t -> (string * string) list
```

**Example:**
```ocaml
let upload_handler req =
  match Kirin.multipart req with
  | Some form ->
    let title = Kirin.multipart_field "title" form in
    let files = Kirin.multipart_files form in
    List.iter (fun file ->
      Printf.printf "File: %s (%d bytes)\n"
        (Option.value ~default:"unnamed" file.filename)
        (String.length file.content);
      (* Save file... *)
    ) files;
    Kirin.json (`Assoc [
      ("title", `String (Option.value ~default:"" title));
      ("files", `Int (List.length files));
    ])
  | None ->
    Kirin.bad_request ~body:"Expected multipart form data" ()
```

**HTML Form:**
```html
<form action="/upload" method="POST" enctype="multipart/form-data">
  <input type="text" name="title">
  <input type="file" name="document">
  <button type="submit">Upload</button>
</form>
```

---

## Compression

```ocaml
(** Compression middleware (gzip/deflate) *)
val compress : middleware

(** Compress string using gzip *)
val compress_gzip : string -> string

(** Compress string using deflate *)
val compress_deflate : string -> string
```

**Features:**
- Automatic Accept-Encoding detection
- Skips already-compressed content (images, video, etc.)
- Only compresses when it reduces size
- Adds `Content-Encoding` and `Vary` headers

**Example:**
```ocaml
let () = Kirin.start ~port:8000
  @@ Kirin.compress  (* Add before routes for response compression *)
  @@ routes
```

---

## Rate Limiting

### Configuration

```ocaml
type rate_limit_config = {
  requests_per_second : float;
  burst_size : int;
}

val default_rate_limit_config : rate_limit_config
(* { requests_per_second = 10.0; burst_size = 20 } *)
```

### Middleware

```ocaml
(** Rate limiting middleware *)
val rate_limit : ?config:rate_limit_config -> ?get_key:(Request.t -> string) -> middleware
```

**Parameters:**
- `?config` - Rate limit configuration
- `?get_key` - Function to extract client identifier (default: X-Forwarded-For or X-Real-IP)

**Example:**
```ocaml
let strict_limit = {
  Kirin.requests_per_second = 1.0;
  burst_size = 5;
}

let () = Kirin.start ~port:8000
  @@ Kirin.rate_limit ~config:strict_limit
  @@ routes
```

**Response Headers:**
- `X-RateLimit-Limit` - Maximum requests
- `X-RateLimit-Remaining` - Remaining requests
- `X-RateLimit-Reset` - Seconds until reset
- `Retry-After` - Seconds to wait (on 429)

---

## ETag

```ocaml
(** ETag type *)
type t =
  | Strong of string
  | Weak of string

(** ETag middleware *)
val etag : middleware

(** Generate ETag from content *)
val generate_etag : ?weak:bool -> string -> Etag.t

(** Add ETag header to response *)
val with_etag : Etag.t -> Response.t -> Response.t
```

**Example:**
```ocaml
let () = Kirin.start ~port:8000
  @@ Kirin.etag  (* Automatic ETag generation and 304 responses *)
  @@ routes
```

**Features:**
- Automatic ETag generation using MD5
- If-None-Match handling (304 Not Modified)
- Strong and weak ETag support

---

## Internal Modules

For advanced use cases, internal modules are exposed:

```ocaml
module Request = Request
module Response = Response
module Router = Router
module Middleware = Middleware
module Static = Static
module Cookie = Cookie
module Etag = Etag
module Multipart = Multipart
module Compress = Compress
module Ratelimit = Ratelimit
module Websocket = Websocket
module Sse = Sse
module Template = Template
```

**Example:**
```ocaml
(* Direct access to internal functions *)
let mime = Kirin.Static.get_mime_type "file.json"
let safe = Kirin.Static.is_safe_path "../etc/passwd"
let etag = Kirin.Etag.generate "content"
```
