(** Generic Worker Interface

    Abstract interface for SSR workers.
    Implementations: Node.js subprocess, QuickJS, Bun, etc.
*)

(** Worker status *)
type status =
  | Idle
  | Busy
  | Restarting
  | Dead

let status_to_string = function
  | Idle -> "idle"
  | Busy -> "busy"
  | Restarting -> "restarting"
  | Dead -> "dead"

(** Worker statistics *)
type stats = {
  requests_handled: int;
  errors: int;
  avg_response_time_ms: float;
  last_request_time: float option;
  memory_mb: int option;
  uptime_seconds: float;
}

let empty_stats = {
  requests_handled = 0;
  errors = 0;
  avg_response_time_ms = 0.0;
  last_request_time = None;
  memory_mb = None;
  uptime_seconds = 0.0;
}

(** Worker configuration *)
type config = {
  bundle_path: string;
  timeout_ms: int;
  memory_limit_mb: int;
  restart_after_requests: int;
  restart_after_errors: int;
  env: (string * string) list;
}

let default_config = {
  bundle_path = "";
  timeout_ms = 5000;
  memory_limit_mb = 200;
  restart_after_requests = 5000;
  restart_after_errors = 10;
  env = [];
}

(** Worker module type *)
module type WORKER = sig
  type t

  val create : config -> t
  val render : t -> url:string -> props:Yojson.Safe.t -> (string, string) result
  val health_check : t -> bool
  val stats : t -> stats
  val status : t -> status
  val restart : t -> unit
  val close : t -> unit
end

(** Worker events for monitoring *)
type event =
  | Started
  | Request_started of { id: int; url: string }
  | Request_completed of { id: int; duration_ms: float }
  | Request_failed of { id: int; error: string }
  | Memory_warning of { current_mb: int; limit_mb: int }
  | Restarting of { reason: string }
  | Stopped

type event_handler = event -> unit

(** No-op event handler *)
let null_handler : event_handler = fun _ -> ()

(** Logging event handler *)
let log_handler : event_handler = function
  | Started -> Printf.printf "[Worker] Started\n%!"
  | Request_started { id; url } ->
    Printf.printf "[Worker] Request %d: %s\n%!" id url
  | Request_completed { id; duration_ms } ->
    Printf.printf "[Worker] Request %d completed in %.1fms\n%!" id duration_ms
  | Request_failed { id; error } ->
    Printf.printf "[Worker] Request %d failed: %s\n%!" id error
  | Memory_warning { current_mb; limit_mb } ->
    Printf.printf "[Worker] Memory warning: %dMB / %dMB\n%!" current_mb limit_mb
  | Restarting { reason } ->
    Printf.printf "[Worker] Restarting: %s\n%!" reason
  | Stopped ->
    Printf.printf "[Worker] Stopped\n%!"

(** Worker pool interface *)
module type POOL = sig
  type t
  type worker

  val create :
    size:int ->
    config:config ->
    ?on_event:event_handler ->
    unit -> t

  val render : t -> url:string -> props:Yojson.Safe.t -> (string, string) result
  val render_any : t -> url:string -> props:Yojson.Safe.t -> (string, string) result
  val health : t -> bool
  val stats : t -> stats list
  val shutdown : t -> unit
end

(** In-memory render result cache *)
module Render_cache = struct
  type entry = {
    html: string;
    created_at: float;
    ttl_seconds: float;
  }

  type t = (string, entry) Hashtbl.t

  let create () : t = Hashtbl.create 256

  let cache_key ~url ~props =
    let props_str = Yojson.Safe.to_string props in
    url ^ "|" ^ props_str

  let get cache ~url ~props =
    let key = cache_key ~url ~props in
    match Hashtbl.find_opt cache key with
    | Some entry ->
      let now = Unix.gettimeofday () in
      if now -. entry.created_at < entry.ttl_seconds then
        Some entry.html
      else begin
        Hashtbl.remove cache key;
        None
      end
    | None -> None

  let set cache ~url ~props ~html ~ttl_seconds =
    let key = cache_key ~url ~props in
    let entry = {
      html;
      created_at = Unix.gettimeofday ();
      ttl_seconds;
    } in
    Hashtbl.replace cache key entry

  let clear cache = Hashtbl.clear cache

  let size cache = Hashtbl.length cache
end

(** Placeholder render function for testing *)
let placeholder_render ~url ~props =
  let props_json = Yojson.Safe.to_string props in
  Printf.sprintf {|<div id="root" data-url="%s" data-props='%s'>Loading...</div>|}
    url props_json
