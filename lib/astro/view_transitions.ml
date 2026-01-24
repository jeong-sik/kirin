(** Astro View Transitions

    Native View Transitions API support for smooth page navigation. *)

(** {1 Transition Types} *)

(** Animation type *)
type animation =
  | Fade
  | Slide
  | None_
  | Initial
  | Custom of string

(** Transition direction *)
type direction =
  | Forward
  | Backward

(** Transition element *)
type element = {
  name: string;
  animation: animation;
  persist: bool;
}

(** View transition configuration *)
type config = {
  enabled: bool;
  fallback: bool;  (* Use fallback for unsupported browsers *)
  animation: animation;
  elements: element list;
}

(** {1 Configuration} *)

(** Default configuration *)
let default_config = {
  enabled = true;
  fallback = true;
  animation = Fade;
  elements = [];
}

(** Enable view transitions *)
let enable config = { config with enabled = true }

(** Disable view transitions *)
let disable config = { config with enabled = false }

(** Enable fallback *)
let with_fallback config = { config with fallback = true }

(** Set default animation *)
let with_animation animation config = { config with animation }

(** {1 Element Transitions} *)

(** Create transition element *)
let element ~name ?(animation=Fade) ?(persist=false) () = {
  name;
  animation;
  persist;
}

(** Add element to config *)
let add_element elem config =
  { config with elements = elem :: config.elements }

(** Persist element across navigations *)
let persist name =
  element ~name ~persist:true ()

(** {1 Animation Helpers} *)

(** Animation to string *)
let animation_to_string = function
  | Fade -> "fade"
  | Slide -> "slide"
  | None_ -> "none"
  | Initial -> "initial"
  | Custom name -> name

(** Animation from string *)
let animation_of_string = function
  | "fade" -> Fade
  | "slide" -> Slide
  | "none" -> None_
  | "initial" -> Initial
  | name -> Custom name

(** {1 HTML Attributes} *)

(** Generate transition:name attribute *)
let transition_name name =
  Printf.sprintf {|transition:name="%s"|} name

(** Generate transition:animate attribute *)
let transition_animate animation =
  Printf.sprintf {|transition:animate="%s"|} (animation_to_string animation)

(** Generate transition:persist attribute *)
let transition_persist = "transition:persist"

(** Generate all transition attributes *)
let transition_attrs elem =
  let attrs = [transition_name elem.name] in
  let attrs = if elem.animation <> Fade then
    transition_animate elem.animation :: attrs
  else attrs in
  let attrs = if elem.persist then
    transition_persist :: attrs
  else attrs in
  String.concat " " (List.rev attrs)

(** {1 Script Generation} *)

(** Generate ViewTransitions script *)
let generate_script config =
  if not config.enabled then ""
  else
  Printf.sprintf {|<script>
(function() {
  if (!document.startViewTransition) {
    %s
    return;
  }

  // Handle navigation
  document.addEventListener('click', (e) => {
    const link = e.target.closest('a');
    if (!link) return;
    if (link.origin !== location.origin) return;
    if (link.pathname === location.pathname) return;

    e.preventDefault();

    document.startViewTransition(async () => {
      const response = await fetch(link.href);
      const html = await response.text();
      const doc = new DOMParser().parseFromString(html, 'text/html');

      // Update head
      document.title = doc.title;

      // Update body with transition
      document.body.innerHTML = doc.body.innerHTML;

      // Update URL
      history.pushState({}, '', link.href);
    });
  });

  // Handle back/forward
  window.addEventListener('popstate', () => {
    document.startViewTransition(async () => {
      const response = await fetch(location.href);
      const html = await response.text();
      const doc = new DOMParser().parseFromString(html, 'text/html');
      document.title = doc.title;
      document.body.innerHTML = doc.body.innerHTML;
    });
  });
})();
</script>|}
    (if config.fallback then
      "// Fallback: use regular navigation\nreturn;"
    else
      "")

(** Generate CSS for animations *)
let generate_css config =
  let base = {|::view-transition-old(root),
::view-transition-new(root) {
  animation-duration: 0.3s;
}|} in

  let element_css = List.map (fun elem ->
    let name = elem.name in
    let anim = animation_to_string elem.animation in
    Printf.sprintf {|::view-transition-old(%s),
::view-transition-new(%s) {
  animation: %s 0.3s ease-in-out;
}|} name name anim
  ) config.elements in

  String.concat "\n\n" (base :: element_css)

(** {1 Component} *)

(** Generate ViewTransitions component import *)
let component_import =
  "import { ViewTransitions } from 'astro:transitions';"

(** Generate ViewTransitions component usage *)
let component_usage =
  "<ViewTransitions />"

(** Full head content for view transitions *)
let head_content config =
  if not config.enabled then ""
  else Printf.sprintf {|%s
<style>
%s
</style>|}
    component_usage
    (generate_css config)
