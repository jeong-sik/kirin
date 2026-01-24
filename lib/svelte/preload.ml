(** Svelte Route Preloading

    SvelteKit-style link preloading with data-sveltekit attributes. *)

(** {1 Preload Strategies} *)

(** When to start preloading *)
type preload_strategy =
  | Hover      (* data-sveltekit-preload-data="hover" *)
  | Tap        (* data-sveltekit-preload-data="tap" *)
  | Viewport   (* Preload when link enters viewport *)
  | Eager      (* Preload immediately *)
  | Off        (* data-sveltekit-preload-data="off" *)

(** What to preload *)
type preload_type =
  | Code       (* Just JS/CSS *)
  | Data       (* Run load functions *)
  | CodeAndData

(** Navigation behavior *)
type nav_behavior =
  | Navigate     (* Normal navigation *)
  | NoScroll     (* data-sveltekit-noscroll *)
  | KeepFocus    (* data-sveltekit-keepfocus *)
  | ReplaceState (* data-sveltekit-replacestate *)

(** {1 Preload Configuration} *)

(** Preload options for a link *)
type preload_options = {
  strategy: preload_strategy;
  preload_type: preload_type;
  behaviors: nav_behavior list;
  reload: bool;  (* data-sveltekit-reload *)
}

(** Default preload options *)
let default_options = {
  strategy = Hover;
  preload_type = Data;
  behaviors = [];
  reload = false;
}

(** No preloading *)
let no_preload = {
  default_options with strategy = Off
}

(** Eager preload *)
let eager_preload = {
  default_options with strategy = Eager
}

(** {1 Attribute Generation} *)

(** Generate preload data attribute *)
let preload_data_attr = function
  | Hover -> ""  (* Default, no attr needed *)
  | Tap -> {|data-sveltekit-preload-data="tap"|}
  | Viewport -> {|data-sveltekit-preload-data="viewport"|}
  | Eager -> {|data-sveltekit-preload-data="eager"|}
  | Off -> {|data-sveltekit-preload-data="off"|}

(** Generate behavior attributes *)
let behavior_attrs behaviors =
  List.map (function
    | Navigate -> ""
    | NoScroll -> "data-sveltekit-noscroll"
    | KeepFocus -> "data-sveltekit-keepfocus"
    | ReplaceState -> "data-sveltekit-replacestate"
  ) behaviors
  |> List.filter (fun s -> s <> "")

(** Generate all attributes for link *)
let link_attrs options =
  let attrs = [] in
  let attrs = match options.strategy with
    | Hover -> attrs  (* Default *)
    | s -> preload_data_attr s :: attrs
  in
  let attrs = if options.reload then "data-sveltekit-reload" :: attrs else attrs in
  let attrs = attrs @ behavior_attrs options.behaviors in
  String.concat " " attrs

(** {1 Link Generation} *)

(** Generate link with preload options *)
let link ~href ?options text =
  let opts = Option.value ~default:default_options options in
  let attrs = link_attrs opts in
  if attrs = "" then
    Printf.sprintf {|<a href="%s">%s</a>|} href (Kirin.html_escape text)
  else
    Printf.sprintf {|<a href="%s" %s>%s</a>|} href attrs (Kirin.html_escape text)

(** Generate link with tap preload *)
let link_tap ~href text =
  link ~href ~options:{ default_options with strategy = Tap } text

(** Generate link with no preload *)
let link_no_preload ~href text =
  link ~href ~options:no_preload text

(** Generate external link (data-sveltekit-reload) *)
let link_external ~href text =
  link ~href ~options:{ default_options with reload = true } text

(** {1 Preload Hints} *)

(** Generate modulepreload link *)
let modulepreload_hint href =
  Printf.sprintf {|<link rel="modulepreload" href="%s">|} href

(** Generate preload link for CSS *)
let preload_css_hint href =
  Printf.sprintf {|<link rel="preload" href="%s" as="style">|} href

(** Generate prefetch link *)
let prefetch_hint href =
  Printf.sprintf {|<link rel="prefetch" href="%s">|} href

(** Generate prerender hint *)
let prerender_hint href =
  Printf.sprintf {|<link rel="prerender" href="%s">|} href

(** {1 Route-based Preload} *)

(** Preload hints for route *)
type route_hints = {
  js_modules: string list;
  css_files: string list;
  prefetch: string list;
}

(** Generate hints for route *)
let route_hints ~js ~css ?(prefetch=[]) () = {
  js_modules = js;
  css_files = css;
  prefetch;
}

(** Render all hints for route *)
let render_hints hints =
  let js = List.map modulepreload_hint hints.js_modules in
  let css = List.map preload_css_hint hints.css_files in
  let pf = List.map prefetch_hint hints.prefetch in
  String.concat "\n" (js @ css @ pf)

(** {1 Viewport Preloading} *)

(** Generate intersection observer script for viewport preloading *)
let viewport_observer_script =
  {|<script>
(function() {
  const observer = new IntersectionObserver((entries) => {
    entries.forEach(entry => {
      if (entry.isIntersecting) {
        const link = entry.target;
        const href = link.getAttribute('href');
        if (href && href.startsWith('/')) {
          // Preload route
          const preloadLink = document.createElement('link');
          preloadLink.rel = 'prefetch';
          preloadLink.href = href;
          document.head.appendChild(preloadLink);
        }
        observer.unobserve(link);
      }
    });
  }, { rootMargin: '50px' });

  document.querySelectorAll('a[data-sveltekit-preload-data="viewport"]').forEach(link => {
    observer.observe(link);
  });
})();
</script>|}

(** {1 Serialization} *)

(** Strategy to JSON *)
let strategy_to_json = function
  | Hover -> `String "hover"
  | Tap -> `String "tap"
  | Viewport -> `String "viewport"
  | Eager -> `String "eager"
  | Off -> `String "off"

(** Options to JSON *)
let options_to_json options =
  `Assoc [
    ("strategy", strategy_to_json options.strategy);
    ("preloadType", `String (match options.preload_type with
      | Code -> "code"
      | Data -> "data"
      | CodeAndData -> "code+data"));
    ("reload", `Bool options.reload);
    ("behaviors", `List (List.map (fun b -> `String (match b with
      | Navigate -> "navigate"
      | NoScroll -> "noscroll"
      | KeepFocus -> "keepfocus"
      | ReplaceState -> "replacestate"
    )) options.behaviors));
  ]
