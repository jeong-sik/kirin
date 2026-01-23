(** Route Preloading

    Preload route data for faster navigation. *)

(** {1 Preload Types} *)

(** Preload strategy *)
type strategy =
  | Intent    (* Preload on hover/focus *)
  | Viewport  (* Preload when link enters viewport *)
  | Render    (* Preload immediately when link renders *)
  | None      (* No preloading *)

let strategy_to_string = function
  | Intent -> "intent"
  | Viewport -> "viewport"
  | Render -> "render"
  | None -> "none"

(** Preload state *)
type state =
  | Idle
  | Loading
  | Loaded
  | Error of string

(** {1 Preload Hints} *)

(** Generate Link preload hint *)
let link_hint ~href ~strategy =
  Printf.sprintf "<link rel=\"prefetch\" href=\"%s\" data-preload=\"%s\">"
    href (strategy_to_string strategy)

(** Generate modulepreload for JS *)
let module_preload ~src =
  Printf.sprintf "<link rel=\"modulepreload\" href=\"%s\">" src

(** Generate preconnect hint *)
let preconnect ~origin =
  Printf.sprintf "<link rel=\"preconnect\" href=\"%s\">" origin

(** Generate dns-prefetch hint *)
let dns_prefetch ~hostname =
  Printf.sprintf "<link rel=\"dns-prefetch\" href=\"//%s\">" hostname

(** {1 Preload Script} *)

(** Generate preload JavaScript *)
let preload_script () = {|
<script>
(function() {
  const preloadCache = new Map();

  function preloadRoute(href) {
    if (preloadCache.has(href)) return;

    preloadCache.set(href, 'loading');

    fetch(href, {
      headers: { 'X-Preload': '1' }
    })
    .then(res => res.json())
    .then(data => {
      preloadCache.set(href, { data, timestamp: Date.now() });
    })
    .catch(() => {
      preloadCache.delete(href);
    });
  }

  // Intent-based preloading
  document.addEventListener('mouseover', (e) => {
    const link = e.target.closest('a[data-preload="intent"]');
    if (link && link.href) {
      preloadRoute(link.href);
    }
  });

  // Viewport-based preloading with IntersectionObserver
  if ('IntersectionObserver' in window) {
    const observer = new IntersectionObserver((entries) => {
      entries.forEach(entry => {
        if (entry.isIntersecting) {
          const link = entry.target;
          if (link.href) {
            preloadRoute(link.href);
            observer.unobserve(link);
          }
        }
      });
    }, { rootMargin: '50px' });

    document.querySelectorAll('a[data-preload="viewport"]').forEach(link => {
      observer.observe(link);
    });
  }

  // Expose for manual preloading
  window.__preloadRoute = preloadRoute;
  window.__preloadCache = preloadCache;
})();
</script>
|}

(** {1 Preload Link Attributes} *)

(** Generate data attributes for preload link *)
let link_attrs ~strategy ~prefetch_intent =
  let attrs = [
    ("data-preload", strategy_to_string strategy);
  ] in
  let attrs = if prefetch_intent then
    ("data-prefetch-intent", "true") :: attrs
  else attrs in
  String.concat " " (List.map (fun (k, v) ->
    Printf.sprintf "%s=\"%s\"" k v
  ) attrs)

(** {1 Resource Hints} *)

(** Hint priority *)
type priority = High | Medium | Low | Auto

let priority_to_string = function
  | High -> "high"
  | Medium -> "medium"
  | Low -> "low"
  | Auto -> "auto"

(** Generate fetchpriority attribute *)
let fetch_priority p =
  Printf.sprintf "fetchpriority=\"%s\"" (priority_to_string p)

(** Generate all preload hints for a route *)
let route_hints ~loader_url ~js_modules ~css_files ~priority =
  let loader_hint = Printf.sprintf
    "<link rel=\"preload\" href=\"%s\" as=\"fetch\" crossorigin %s>"
    loader_url (fetch_priority priority) in
  let module_hints = List.map (fun src -> module_preload ~src) js_modules in
  let css_hints = List.map (fun href ->
    Printf.sprintf "<link rel=\"preload\" href=\"%s\" as=\"style\">" href
  ) css_files in
  String.concat "\n" (loader_hint :: module_hints @ css_hints)

(** {1 Navigation Preload} *)

(** Check if request is a preload request *)
let is_preload_request headers =
  List.exists (fun (k, v) ->
    String.lowercase_ascii k = "x-preload" && v = "1"
  ) headers

(** Generate response for preload request *)
let preload_response data =
  Yojson.Safe.to_string (`Assoc [
    ("preloaded", `Bool true);
    ("data", data);
    ("timestamp", `Int (int_of_float (Unix.time ())));
  ])
