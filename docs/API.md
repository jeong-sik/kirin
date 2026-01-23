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
- [React Integration](#react-integration)

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

---

## High-Performance Modules

### Streaming

Large data handling without memory overflow. Process data incrementally using chunks.

```ocaml
(** Chunk for streaming *)
type chunk =
  | Data of string
  | End

(** Streaming source - lazy sequence of chunks *)
type source = unit -> chunk

(** Streaming sink - consumes chunks *)
type 'a sink = chunk -> 'a

(** Streaming transform - modifies chunks *)
type transform = chunk -> chunk
```

#### File Streaming

```ocaml
(** Stream from file *)
val file_source : string -> source

(** Stream to file *)
val file_sink : string -> unit sink

(** Copy file using streaming *)
val copy_file : src:string -> dst:string -> unit
```

#### Transformations

```ocaml
(** Map over chunks *)
val map : (string -> string) -> transform

(** Filter chunks *)
val filter : (string -> bool) -> transform

(** Take n bytes *)
val take : int -> transform

(** Drop n bytes *)
val drop : int -> transform

(** Compose transforms *)
val ( >> ) : transform -> transform -> transform
```

**Example:**
```ocaml
(* Process large CSV file line by line *)
let uppercase_lines =
  Kirin.Streaming.file_source "input.csv"
  |> Kirin.Streaming.lines
  |> Kirin.Streaming.map String.uppercase_ascii
  |> Kirin.Streaming.to_sink (Kirin.Streaming.file_sink "output.csv")

(* Streaming HTTP response *)
let stream_handler _req =
  let source = Kirin.Streaming.file_source "large_file.bin" in
  Kirin.Streaming.stream_response source
```

---

### Pool

Generic connection pooling with health checks and automatic resource management.

```ocaml
(** Pool configuration *)
type config = {
  min_size : int;           (** Minimum connections to keep (default: 1) *)
  max_size : int;           (** Maximum connections allowed (default: 10) *)
  idle_timeout : float;     (** Seconds before idle connection is closed (default: 300.0) *)
  max_wait_time : float;    (** Max seconds to wait for connection (default: 30.0) *)
  health_check_interval : float;  (** Seconds between health checks (default: 60.0) *)
}

(** Pool statistics *)
type stats = {
  total_connections : int;
  active_connections : int;
  idle_connections : int;
  waiting_requests : int;
  total_acquisitions : int;
  total_timeouts : int;
  total_errors : int;
}

(** Pool type *)
type 'a t
```

#### Pool Operations

```ocaml
(** Create pool with labeled arguments *)
val create :
  ?min_size:int ->
  ?max_size:int ->
  ?idle_timeout:float ->
  ?max_wait_time:float ->
  ?health_check_interval:float ->
  ~create:(unit -> 'a) ->
  ~destroy:('a -> unit) ->
  ?validate:('a -> bool) ->
  unit -> 'a t

(** Use resource with automatic release *)
val use : 'a t -> ('a -> 'b) -> 'b

(** Get pool statistics *)
val stats : 'a t -> stats

(** Drain and shutdown pool *)
val drain : 'a t -> unit
```

**Example:**
```ocaml
(* Database connection pool *)
let db_pool = Kirin.Pool.create
  ~max_size:10
  ~create:(fun () -> Db.connect ~host:"localhost" ~db:"myapp")
  ~destroy:Db.close
  ~validate:Db.ping
  ()

let query_handler req =
  Kirin.Pool.use db_pool (fun conn ->
    let results = Db.query conn "SELECT * FROM users" in
    Kirin.json (users_to_json results)
  )
```

---

### Backpressure

Flow control to prevent fast producers from overwhelming slow consumers.

```ocaml
(** Backpressure strategy *)
type strategy =
  | Block        (** Block producer until consumer catches up *)
  | Drop_oldest  (** Drop oldest items when buffer full *)
  | Drop_newest  (** Drop newest items when buffer full *)
  | Error        (** Raise exception when buffer full *)
```

#### Buffer

```ocaml
(** Bounded buffer with backpressure *)
module Buffer : sig
  type 'a t

  val create : ?strategy:strategy -> capacity:int -> unit -> 'a t
  val push : 'a t -> 'a -> bool  (** Returns false if dropped *)
  val pop : 'a t -> 'a option
  val size : 'a t -> int
  val is_full : 'a t -> bool
end
```

#### Channel

```ocaml
(** Async channel with backpressure *)
module Channel : sig
  type 'a t

  val create : ?capacity:int -> unit -> 'a t
  val send : 'a t -> 'a -> unit
  val receive : 'a t -> 'a
  val try_receive : 'a t -> 'a option
  val close : 'a t -> unit
end
```

#### Rate Limiter

```ocaml
(** Token bucket rate limiter *)
module RateLimiter : sig
  type t

  val create : ?rate:float -> ?burst:int -> unit -> t
  val acquire : t -> unit     (** Block until token available *)
  val try_acquire : t -> bool (** Non-blocking, returns false if no token *)
  val acquire_n : t -> int -> unit
end
```

**Example:**
```ocaml
(* Rate-limited API calls *)
let api_limiter = Kirin.Backpressure.RateLimiter.create
  ~rate:10.0  (* 10 requests/second *)
  ~burst:20   (* Allow burst of 20 *)
  ()

let api_handler req =
  if Kirin.Backpressure.RateLimiter.try_acquire api_limiter then
    process_request req
  else
    Kirin.Response.make ~status:`Too_many_requests "Rate limited"

(* Bounded work queue *)
let work_buffer = Kirin.Backpressure.Buffer.create
  ~strategy:Drop_oldest ~capacity:1000 ()

let submit_work item =
  if not (Kirin.Backpressure.Buffer.push work_buffer item) then
    Printf.eprintf "Work dropped due to backpressure\n"
```

---

### Cache

LRU (Least Recently Used) cache with optional TTL support.

```ocaml
(** Cache type *)
type ('k, 'v) t

(** Cache configuration *)
type config = {
  max_size : int;        (** Maximum entries *)
  default_ttl : float option;  (** Default TTL in seconds *)
}
```

#### Operations

```ocaml
(** Create cache *)
val create : ?config:config -> unit -> ('k, 'v) t

(** Get value *)
val get : ('k, 'v) t -> 'k -> 'v option

(** Set value with optional TTL *)
val set : ?ttl:float -> ('k, 'v) t -> 'k -> 'v -> unit

(** Get or compute and cache *)
val get_or_set : ?ttl:float -> ('k, 'v) t -> 'k -> (unit -> 'v) -> 'v

(** Remove value *)
val remove : ('k, 'v) t -> 'k -> unit

(** Clear all entries *)
val clear : ('k, 'v) t -> unit

(** Cache statistics *)
val stats : ('k, 'v) t -> stats
```

**Example:**
```ocaml
(* User cache with 5-minute TTL *)
let user_cache = Kirin.Cache.create ~config:{
  max_size = 10000;
  default_ttl = Some 300.0;  (* 5 minutes *)
} ()

let get_user id =
  Kirin.Cache.get_or_set user_cache id (fun () ->
    Db.fetch_user id  (* Only called on cache miss *)
  )

(* Response memoization *)
let expensive_handler req =
  let cache_key = Kirin.Request.path req in
  Kirin.Cache.get_or_set ~ttl:60.0 response_cache cache_key (fun () ->
    compute_expensive_response req
  )
```

---

### Jobs

Background job queue with priority levels and retry support.

```ocaml
(** Job priority *)
type priority =
  | Critical  (** Highest priority *)
  | High
  | Normal
  | Low       (** Lowest priority *)

(** Job status *)
type 'a status =
  | Pending
  | Running
  | Completed of 'a
  | Failed of exn
```

#### Queue Operations

```ocaml
(** Create job queue *)
val create : ?workers:int -> ?max_queue_size:int -> unit -> 'a t

(** Start processing jobs *)
val start : 'a t -> unit

(** Stop processing *)
val stop : 'a t -> unit

(** Submit job *)
val submit : ?priority:priority -> ?max_retries:int -> 'a t -> (unit -> 'a) -> job_id

(** Get job status *)
val status : 'a t -> job_id -> 'a status

(** Wait for job completion *)
val wait : 'a t -> job_id -> 'a status

(** Cancel pending job *)
val cancel : 'a t -> job_id -> bool

(** Queue statistics *)
val stats : 'a t -> stats
```

**Example:**
```ocaml
(* Email sending queue *)
let email_queue = Kirin.Jobs.create ~workers:4 ()
let () = Kirin.Jobs.start email_queue

let send_welcome_email user =
  let job_id = Kirin.Jobs.submit ~priority:High email_queue (fun () ->
    Email.send ~to_:user.email ~subject:"Welcome!" ~body:"..."
  ) in
  Printf.printf "Email job queued: %s\n" job_id

(* Async job with status check *)
let export_handler req =
  let job_id = Kirin.Jobs.submit ~priority:Low export_queue (fun () ->
    generate_large_report ()
  ) in
  Kirin.json (`Assoc [("job_id", `String job_id); ("status", `String "pending")])

let status_handler req =
  let job_id = Kirin.query "id" req in
  match Kirin.Jobs.status export_queue job_id with
  | Completed url -> Kirin.json (`Assoc [("status", `String "done"); ("url", `String url)])
  | Running -> Kirin.json (`Assoc [("status", `String "processing")])
  | Pending -> Kirin.json (`Assoc [("status", `String "queued")])
  | Failed exn -> Kirin.json (`Assoc [("status", `String "failed"); ("error", `String (Printexc.to_string exn))])
```

---

### Parallel

OCaml 5 Domain-based parallel computation for CPU-bound tasks.
True parallelism without GIL limitations.

```ocaml
(** Get recommended domain count based on CPUs *)
val recommended_domains : unit -> int
```

#### Parallel Collections

```ocaml
(** Parallel map *)
val map : ?domains:int -> ('a -> 'b) -> 'a list -> 'b list

(** Parallel iter *)
val iter : ?domains:int -> ('a -> unit) -> 'a list -> unit

(** Parallel filter *)
val filter : ?domains:int -> ('a -> bool) -> 'a list -> 'a list

(** Parallel reduce *)
val reduce : ?domains:int -> ('a -> 'a -> 'a) -> 'a -> 'a list -> 'a

(** Parallel map with index *)
val mapi : ?domains:int -> (int -> 'a -> 'b) -> 'a list -> 'b list
```

#### Fork-Join

```ocaml
(** Run two computations in parallel *)
val both : (unit -> 'a) -> (unit -> 'b) -> 'a * 'b

(** Run three computations in parallel *)
val triple : (unit -> 'a) -> (unit -> 'b) -> (unit -> 'c) -> 'a * 'b * 'c

(** Run all computations in parallel *)
val all : (unit -> 'a) list -> 'a list
```

#### Domain Pool

```ocaml
module Pool : sig
  type t

  val create : ?size:int -> unit -> t
  val map : t -> ('a -> 'b) -> 'a list -> 'b list
  val shutdown : t -> unit
end
```

**Example:**
```ocaml
(* Parallel image processing *)
let process_images images =
  Kirin.Parallel.map ~domains:8 process_single_image images

(* Parallel aggregation *)
let total = Kirin.Parallel.reduce ~domains:4 (+) 0 numbers

(* Fork-join for independent computations *)
let (user_data, order_data) = Kirin.Parallel.both
  (fun () -> fetch_user_data user_id)
  (fun () -> fetch_order_data user_id)

(* Chunked processing for fine-grained work *)
let results = Kirin.Parallel.map_chunked ~chunk_size:100 ~domains:4
  tiny_computation huge_list
```

---

## Performance Tips

### Streaming for Large Data
```ocaml
(* BAD: Loads entire file into memory *)
let content = In_channel.read_all "huge.csv"

(* GOOD: Stream processing *)
Kirin.Streaming.file_source "huge.csv"
|> Kirin.Streaming.lines
|> Kirin.Streaming.fold process_line initial
```

### Connection Pooling
```ocaml
(* BAD: New connection per request *)
let handler req =
  let conn = Db.connect () in
  let result = Db.query conn "..." in
  Db.close conn;
  result

(* GOOD: Use connection pool *)
let handler req =
  Kirin.Pool.with_resource db_pool (fun conn ->
    Db.query conn "..."
  )
```

### Caching Hot Data
```ocaml
(* Use get_or_set for transparent memoization *)
let get_config key =
  Kirin.Cache.get_or_set config_cache key (fun () ->
    load_config_from_db key
  )
```

### Parallel for CPU-Bound Work
```ocaml
(* Use Parallel for CPU-intensive operations *)
let results = Kirin.Parallel.map ~domains:8 expensive_computation items

(* Use Jobs for I/O-bound background work *)
Kirin.Jobs.submit queue (fun () -> send_email user)
```

---

## React Integration

The `Kirin_react` module provides three levels of React integration.

### Level 1: Static Serving

Serve Vite build output with SPA fallback.

```ocaml
open Kirin_react

let () =
  let routes = Vite.static_routes () @ [
    Kirin.get "/api/users" api_users;
  ] in
  Kirin.start ~port:3000 @@ Kirin.router routes
```

### Level 2: Hydration

Server HTML shell with `__INITIAL_DATA__` for SEO.

```ocaml
let handler req =
  let user = fetch_current_user req in
  let manifest = Manifest.load "dist/.vite/manifest.json" |> Result.get_ok in
  Kirin_react.hydrate_response
    ~title:"User Profile"
    ~meta:[("og:title", "Profile"); ("description", "User profile page")]
    ~initial_data:(Some (`Assoc [("user", user_to_json user)]))
    ~manifest
    ~entry:"index.html"
    ()
```

### Level 3: Full SSR

Server-side rendering with Node.js worker pool.

```ocaml
let () =
  let config = {
    Ssr.default_config with
    bundle = "dist/server/entry-server.js";
    workers = 4;
    timeout = 5.0;
  } in
  let engine = Kirin_react.create_ssr ~config () in
  at_exit (fun () -> Ssr.shutdown engine);

  let routes = [
    Kirin.get "/api/*" api_handler;
    Kirin.get "/*" (Kirin_react.ssr_handler engine);
  ] in
  Kirin.start ~port:3000 @@ Kirin.router routes
```

### Manifest Module

```ocaml
module Manifest : sig
  type entry = {
    file: string;
    src: string;
    is_entry: bool;
    css: string list;
    assets: string list;
    dynamic_imports: string list;
    imports: string list;
  }
  type t = (string * entry) list

  val load : string -> (t, string) result
  val resolve : t -> string -> string option
  val css_for : t -> string -> string list
  val preload_hints : ?base_url:string -> t -> string -> string
end
```

### Meta Module

```ocaml
module Meta : sig
  val title : string -> string
  val description : string -> string
  val og : name:string -> content:string -> string
  val twitter : name:string -> content:string -> string
  val canonical : string -> string
end
```

### SSR Configuration

```ocaml
type config = {
  bundle: string;           (* Path to Node.js SSR bundle *)
  workers: int;             (* Number of worker processes *)
  timeout: float;           (* Render timeout in seconds *)
  memory_limit_mb: int;     (* Memory limit per worker *)
  restart_after: int;       (* Restart after N requests *)
}

val default_config : config
(* bundle = "dist/server/entry-server.js"
   workers = 4
   timeout = 5.0
   memory_limit_mb = 200
   restart_after = 5000 *)
```

### SSR Statistics

```ocaml
type stats = {
  total_renders: int;
  errors: int;
  timeouts: int;
  avg_time_ms: float;
  cache_size: int;
  cache_hit_rate: float;
  active_workers: int;
  idle_workers: int;
}

val stats : Ssr.t -> stats
```

### Streaming SSR

```ocaml
module Streaming : sig
  module SSE : sig
    val response : engine:Ssr.t -> url:string -> ?props:Yojson.Safe.t -> unit -> Kirin.Response.t
  end
end
```
