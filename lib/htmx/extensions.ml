(** HTMX Extensions Helpers

    Generate attributes for common HTMX extensions.
    See: https://htmx.org/extensions/ *)

(** {1 Extension Loading} *)

(** Load extension via hx-ext attribute *)
let use extensions =
  ("hx-ext", String.concat ", " extensions)

(** Load single extension *)
let use_one ext = ("hx-ext", ext)

(** {1 Preload Extension} *)
(** https://htmx.org/extensions/preload/ *)

module Preload = struct
  (** Enable preload on element *)
  let enable = use_one "preload"

  (** Preload on hover *)
  let preload_on_hover = ("preload", "mousedown")

  (** Preload immediately *)
  let preload_init = ("preload", "init")

  (** Custom preload images *)
  let preload_images = ("preload-images", "true")
end

(** {1 WebSocket Extension} *)
(** https://htmx.org/extensions/web-sockets/ *)

module Ws = struct
  (** Enable WebSocket extension *)
  let enable = use_one "ws"

  (** Connect to WebSocket endpoint *)
  let connect url = ("ws-connect", url)

  (** Send message on event *)
  let send = ("ws-send", "")

  (** Full WebSocket setup *)
  let setup url = [
    use_one "ws";
    ("ws-connect", url);
  ]
end

(** {1 SSE Extension} *)
(** https://htmx.org/extensions/server-sent-events/ *)

module Sse = struct
  (** Enable SSE extension *)
  let enable = use_one "sse"

  (** Connect to SSE endpoint *)
  let connect url = ("sse-connect", url)

  (** Swap on specific event *)
  let swap event = ("sse-swap", event)

  (** Full SSE setup *)
  let setup url event = [
    use_one "sse";
    ("sse-connect", url);
    ("sse-swap", event);
  ]
end

(** {1 Response Targets Extension} *)
(** https://htmx.org/extensions/response-targets/ *)

module ResponseTargets = struct
  (** Enable response-targets extension *)
  let enable = use_one "response-targets"

  (** Target for specific status code *)
  let target_status code selector =
    (Printf.sprintf "hx-target-%d" code, selector)

  (** Target for 4xx errors *)
  let target_4xx selector = ("hx-target-4*", selector)

  (** Target for 5xx errors *)
  let target_5xx selector = ("hx-target-5*", selector)

  (** Target for any error *)
  let target_error selector = ("hx-target-error", selector)

  (** Common error handling setup *)
  let error_handling ~error_container = [
    enable;
    target_4xx error_container;
    target_5xx error_container;
  ]
end

(** {1 Loading States Extension} *)
(** https://htmx.org/extensions/loading-states/ *)

module LoadingStates = struct
  (** Enable loading-states extension *)
  let enable = use_one "loading-states"

  (** Add class during loading *)
  let add_class cls = ("data-loading-class", cls)

  (** Remove class during loading *)
  let remove_class cls = ("data-loading-class-remove", cls)

  (** Disable element during loading *)
  let disable = ("data-loading-disable", "")

  (** Show element during loading *)
  let show = ("data-loading", "")

  (** Set aria-busy during loading *)
  let aria_busy = ("data-loading-aria-busy", "")

  (** Delay before showing loading state *)
  let delay ms = ("data-loading-delay", string_of_int ms)

  (** Loading state with spinner *)
  let with_spinner ~class_ ~delay_ms = [
    enable;
    add_class class_;
    delay delay_ms;
  ]
end

(** {1 Class Tools Extension} *)
(** https://htmx.org/extensions/class-tools/ *)

module ClassTools = struct
  (** Enable class-tools extension *)
  let enable = use_one "class-tools"

  (** Add class after delay *)
  let classes_add cls delay =
    ("classes", Printf.sprintf "add %s:%s" cls delay)

  (** Remove class after delay *)
  let classes_remove cls delay =
    ("classes", Printf.sprintf "remove %s:%s" cls delay)

  (** Toggle class after delay *)
  let classes_toggle cls delay =
    ("classes", Printf.sprintf "toggle %s:%s" cls delay)

  (** Chain multiple class operations *)
  let classes operations =
    let ops = operations |> List.map (fun (op, cls, delay) ->
      Printf.sprintf "%s %s:%s" op cls delay
    ) in
    ("classes", String.concat ", " ops)
end

(** {1 JSON Encoding Extension} *)
(** https://htmx.org/extensions/json-enc/ *)

module JsonEnc = struct
  (** Enable json-enc extension - sends form data as JSON *)
  let enable = use_one "json-enc"
end

(** {1 Path Deps Extension} *)
(** https://htmx.org/extensions/path-deps/ *)

module PathDeps = struct
  (** Enable path-deps extension *)
  let enable = use_one "path-deps"

  (** Declare path dependency *)
  let path_deps paths = ("hx-path-deps", String.concat "," paths)
end

(** {1 Restored Extension} *)
(** https://htmx.org/extensions/restored/ *)

module Restored = struct
  (** Enable restored extension - fires htmx:restored on back button *)
  let enable = use_one "restored"
end

(** {1 Multi-Swap Extension} *)
(** https://htmx.org/extensions/multi-swap/ *)

module MultiSwap = struct
  (** Enable multi-swap extension *)
  let enable = use_one "multi-swap"
end

(** {1 Morphdom Extension} *)
(** https://htmx.org/extensions/morphdom-swap/ *)

module Morphdom = struct
  (** Enable morphdom-swap extension for DOM morphing *)
  let enable = use_one "morphdom-swap"
end

(** {1 Alpine Morph Extension} *)
(** https://htmx.org/extensions/alpine-morph/ *)

module AlpineMorph = struct
  (** Enable alpine-morph for Alpine.js compatibility *)
  let enable = use_one "alpine-morph"
end

(** {1 Debug Extension} *)
(** https://htmx.org/extensions/debug/ *)

module Debug = struct
  (** Enable debug extension - logs all events to console *)
  let enable = use_one "debug"
end

(** {1 Helper Functions} *)

(** Combine multiple attribute lists *)
let combine attr_lists =
  List.flatten attr_lists

(** Render attributes to HTML string *)
let attrs_to_string attrs =
  attrs
  |> List.map (fun (k, v) ->
       if v = "" then k else Printf.sprintf "%s=\"%s\"" k v)
  |> String.concat " "
