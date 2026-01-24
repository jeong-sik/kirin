(** Vue/Nuxt Route Preloading

    Nuxt-style route preloading and prefetching. *)

(** {1 Preload Types} *)

(** Preload mode *)
type preload_mode =
  | Intent       (* On mouse hover/touch start *)
  | Visible      (* When link is visible in viewport *)
  | Always       (* Always preload *)
  | Never        (* Never preload *)

(** Prefetch mode *)
type prefetch_mode =
  | OnVisible    (* Prefetch when visible *)
  | OnInteract   (* Prefetch on interaction *)
  | Disabled     (* No prefetch *)

(** {1 Link Options} *)

(** NuxtLink options *)
type link_options = {
  preload: preload_mode;
  prefetch: prefetch_mode;
  no_prefetch: bool;
  no_rel: bool;
  external_: bool;
  active_class: string;
  exact_active_class: string;
  replace: bool;
  target: string option;
}

(** Default link options *)
let default_options = {
  preload = Intent;
  prefetch = OnVisible;
  no_prefetch = false;
  no_rel = false;
  external_ = false;
  active_class = "router-link-active";
  exact_active_class = "router-link-exact-active";
  replace = false;
  target = None;
}

(** No preload/prefetch *)
let no_preload = {
  default_options with
  preload = Never;
  prefetch = Disabled;
  no_prefetch = true;
}

(** {1 Resource Hints} *)

(** Hint type *)
type hint_type =
  | Modulepreload   (* JS modules *)
  | Preload         (* CSS, fonts *)
  | Prefetch        (* Future navigation *)
  | Preconnect      (* Early connection *)
  | DnsPrefetch     (* DNS lookup *)

(** Resource hint *)
type hint = {
  hint_type: hint_type;
  href: string;
  as_: string option;     (* Resource type *)
  crossorigin: bool;
  media: string option;   (* Media query *)
}

(** Create modulepreload hint *)
let modulepreload href = {
  hint_type = Modulepreload;
  href;
  as_ = Some "script";
  crossorigin = true;
  media = None;
}

(** Create stylesheet preload hint *)
let preload_css href = {
  hint_type = Preload;
  href;
  as_ = Some "style";
  crossorigin = false;
  media = None;
}

(** Create prefetch hint *)
let prefetch href = {
  hint_type = Prefetch;
  href;
  as_ = None;
  crossorigin = false;
  media = None;
}

(** Create preconnect hint *)
let preconnect href = {
  hint_type = Preconnect;
  href;
  as_ = None;
  crossorigin = true;
  media = None;
}

(** {1 Link Generation} *)

(** Generate NuxtLink attributes *)
let link_attrs ~href ?(options=default_options) () =
  let base = [("href", href)] in
  let with_prefetch = if options.no_prefetch then
    ("data-no-prefetch", "true") :: base
  else base
  in
  let with_replace = if options.replace then
    ("data-replace", "true") :: with_prefetch
  else with_prefetch
  in
  let with_target = match options.target with
    | Some t -> ("target", t) :: with_replace
    | None -> with_replace
  in
  let with_external = if options.external_ then
    ("rel", if options.no_rel then "" else "noopener noreferrer") :: with_target
  else with_target
  in
  with_external

(** Generate link tag *)
let link ~href ?(options=default_options) text =
  let classes = [options.active_class] in
  let class_attr = String.concat " " classes in
  if options.no_prefetch then
    Printf.sprintf {|<NuxtLink to="%s" class="%s" no-prefetch>%s</NuxtLink>|}
      href class_attr text
  else
    Printf.sprintf {|<NuxtLink to="%s" class="%s">%s</NuxtLink>|}
      href class_attr text

(** {1 Hint Rendering} *)

(** Render hint to HTML *)
let render_hint hint =
  let rel = match hint.hint_type with
    | Modulepreload -> "modulepreload"
    | Preload -> "preload"
    | Prefetch -> "prefetch"
    | Preconnect -> "preconnect"
    | DnsPrefetch -> "dns-prefetch"
  in
  let as_attr = match hint.as_ with
    | Some a -> Printf.sprintf " as=\"%s\"" a
    | None -> ""
  in
  let crossorigin = if hint.crossorigin then " crossorigin" else "" in
  let media_attr = match hint.media with
    | Some m -> Printf.sprintf " media=\"%s\"" m
    | None -> ""
  in
  Printf.sprintf {|<link rel="%s" href="%s"%s%s%s />|}
    rel hint.href as_attr crossorigin media_attr

(** Render multiple hints *)
let render_hints hints =
  hints |> List.map render_hint |> String.concat "\n"

(** {1 Route Hints} *)

(** Generate hints for route entry *)
let hints_for_route (entry : Manifest.route_entry) =
  let component_hint = modulepreload entry.component in
  [component_hint]

(** Generate all hints for manifest *)
let all_hints manifest =
  manifest.Manifest.routes
  |> List.concat_map hints_for_route

(** {1 Payload Prefetch} *)

(** Payload prefetch options *)
type payload_options = {
  enabled: bool;
  limit: int;        (* Max concurrent prefetches *)
  delay: int;        (* Delay in ms before prefetch *)
  threshold: float;  (* Intersection observer threshold *)
}

(** Default payload options *)
let default_payload_options = {
  enabled = true;
  limit = 3;
  delay = 50;
  threshold = 0.0;
}

(** Generate payload prefetch script *)
let payload_script ~route_path ~options =
  if not options.enabled then ""
  else
    Printf.sprintf {|<script>
window.__nuxt_prefetch = window.__nuxt_prefetch || [];
window.__nuxt_prefetch.push({
  route: '%s',
  delay: %d,
  threshold: %f
});
</script>|}
      route_path options.delay options.threshold

(** {1 Serialization} *)

(** Preload mode to string *)
let preload_mode_to_string = function
  | Intent -> "intent"
  | Visible -> "visible"
  | Always -> "always"
  | Never -> "never"

(** Prefetch mode to string *)
let prefetch_mode_to_string = function
  | OnVisible -> "onVisible"
  | OnInteract -> "onInteract"
  | Disabled -> "disabled"

(** Link options to JSON *)
let options_to_json opts =
  `Assoc [
    ("preload", `String (preload_mode_to_string opts.preload));
    ("prefetch", `String (prefetch_mode_to_string opts.prefetch));
    ("noPrefetch", `Bool opts.no_prefetch);
    ("replace", `Bool opts.replace);
    ("activeClass", `String opts.active_class);
  ]

(** Hint to JSON *)
let hint_to_json hint =
  let type_str = match hint.hint_type with
    | Modulepreload -> "modulepreload"
    | Preload -> "preload"
    | Prefetch -> "prefetch"
    | Preconnect -> "preconnect"
    | DnsPrefetch -> "dns-prefetch"
  in
  `Assoc [
    ("type", `String type_str);
    ("href", `String hint.href);
    ("as", match hint.as_ with Some a -> `String a | None -> `Null);
    ("crossorigin", `Bool hint.crossorigin);
  ]
