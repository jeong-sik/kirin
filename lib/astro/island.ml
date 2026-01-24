(** Astro Islands

    Partial hydration for interactive components.

    Islands are the core concept in Astro - most of the page is static HTML,
    with "islands" of interactivity that hydrate independently. *)

(** {1 Island Types} *)

(** Client directive - when to hydrate the island *)
type client_directive =
  | ClientLoad       (* Hydrate immediately on page load *)
  | ClientIdle       (* Hydrate when browser is idle *)
  | ClientVisible    (* Hydrate when component enters viewport *)
  | ClientMedia of string  (* Hydrate when media query matches *)
  | ClientOnly of string   (* Skip SSR, render only on client (framework name) *)

(** Island framework *)
type framework =
  | React
  | Vue
  | Svelte
  | Solid
  | Preact
  | Lit
  | Alpine
  | Custom of string

(** Island definition *)
type t = {
  id: string;
  component: string;
  framework: framework;
  directive: client_directive;
  props: (string * Yojson.Safe.t) list;
  slots: (string * string) list;  (* Named slots with HTML content *)
}

(** {1 Island Creation} *)

let next_id = ref 0

let gen_id () =
  incr next_id;
  Printf.sprintf "astro-island-%d" !next_id

(** Create an island *)
let create ~component ~framework ?(directive=ClientLoad) ?(props=[]) ?(slots=[]) () = {
  id = gen_id ();
  component;
  framework;
  directive;
  props;
  slots;
}

(** Create React island *)
let react ~component ?directive ?props ?slots () =
  create ~component ~framework:React ?directive ?props ?slots ()

(** Create Vue island *)
let vue ~component ?directive ?props ?slots () =
  create ~component ~framework:Vue ?directive ?props ?slots ()

(** Create Svelte island *)
let svelte ~component ?directive ?props ?slots () =
  create ~component ~framework:Svelte ?directive ?props ?slots ()

(** Create Solid island *)
let solid ~component ?directive ?props ?slots () =
  create ~component ~framework:Solid ?directive ?props ?slots ()

(** Create Preact island *)
let preact ~component ?directive ?props ?slots () =
  create ~component ~framework:Preact ?directive ?props ?slots ()

(** {1 Directive Helpers} *)

(** Load immediately *)
let load island = { island with directive = ClientLoad }

(** Load when idle *)
let idle island = { island with directive = ClientIdle }

(** Load when visible *)
let visible island = { island with directive = ClientVisible }

(** Load on media query *)
let media query island = { island with directive = ClientMedia query }

(** Client-only (no SSR) *)
let only framework island = { island with directive = ClientOnly framework }

(** {1 Serialization} *)

(** Framework to string *)
let framework_to_string = function
  | React -> "react"
  | Vue -> "vue"
  | Svelte -> "svelte"
  | Solid -> "solid"
  | Preact -> "preact"
  | Lit -> "lit"
  | Alpine -> "alpine"
  | Custom name -> name

(** Framework from string *)
let framework_of_string = function
  | "react" -> React
  | "vue" -> Vue
  | "svelte" -> Svelte
  | "solid" -> Solid
  | "preact" -> Preact
  | "lit" -> Lit
  | "alpine" -> Alpine
  | name -> Custom name

(** Directive to string *)
let directive_to_string = function
  | ClientLoad -> "load"
  | ClientIdle -> "idle"
  | ClientVisible -> "visible"
  | ClientMedia q -> "media:" ^ q
  | ClientOnly f -> "only:" ^ f

(** Directive from string *)
let directive_of_string s =
  if s = "load" then ClientLoad
  else if s = "idle" then ClientIdle
  else if s = "visible" then ClientVisible
  else if String.length s > 6 && String.sub s 0 6 = "media:" then
    ClientMedia (String.sub s 6 (String.length s - 6))
  else if String.length s > 5 && String.sub s 0 5 = "only:" then
    ClientOnly (String.sub s 5 (String.length s - 5))
  else ClientLoad

(** Island to JSON *)
let to_json island =
  `Assoc [
    ("id", `String island.id);
    ("component", `String island.component);
    ("framework", `String (framework_to_string island.framework));
    ("directive", `String (directive_to_string island.directive));
    ("props", `Assoc island.props);
    ("slots", `Assoc (List.map (fun (k, v) -> (k, `String v)) island.slots));
  ]

(** {1 HTML Rendering} *)

(** Render island wrapper *)
let render_wrapper island inner_html =
  let props_json = Yojson.Safe.to_string (`Assoc island.props) in
  let directive_attr = match island.directive with
    | ClientLoad -> "client:load"
    | ClientIdle -> "client:idle"
    | ClientVisible -> "client:visible"
    | ClientMedia q -> Printf.sprintf "client:media=\"%s\"" q
    | ClientOnly _ -> "client:only"
  in
  Printf.sprintf
    {|<astro-island uid="%s" component-url="/_astro/%s.js" component-export="%s" renderer-url="/_astro/%s.js" props="%s" %s>%s</astro-island>|}
    island.id
    island.component
    island.component
    (framework_to_string island.framework)
    (String.escaped props_json)
    directive_attr
    inner_html

(** Render island script *)
let render_script island =
  Printf.sprintf
    {|<script type="module">
import { createIsland } from '/_astro/client.js';
createIsland('%s', '%s', '%s', %s);
</script>|}
    island.id
    island.component
    (framework_to_string island.framework)
    (Yojson.Safe.to_string (`Assoc island.props))

(** {1 Hydration Script} *)

(** Generate hydration observer script *)
let hydration_script () =
  {|<script>
(function() {
  const islands = document.querySelectorAll('astro-island');

  const hydrateOnLoad = (island) => {
    import(island.getAttribute('renderer-url')).then(mod => {
      mod.default(island);
    });
  };

  const hydrateOnIdle = (island) => {
    if ('requestIdleCallback' in window) {
      requestIdleCallback(() => hydrateOnLoad(island));
    } else {
      setTimeout(() => hydrateOnLoad(island), 200);
    }
  };

  const hydrateOnVisible = (island) => {
    const observer = new IntersectionObserver((entries) => {
      entries.forEach(entry => {
        if (entry.isIntersecting) {
          observer.disconnect();
          hydrateOnLoad(island);
        }
      });
    });
    observer.observe(island);
  };

  const hydrateOnMedia = (island, query) => {
    const mql = window.matchMedia(query);
    const check = () => {
      if (mql.matches) {
        hydrateOnLoad(island);
      }
    };
    mql.addListener(check);
    check();
  };

  islands.forEach(island => {
    if (island.hasAttribute('client:load')) {
      hydrateOnLoad(island);
    } else if (island.hasAttribute('client:idle')) {
      hydrateOnIdle(island);
    } else if (island.hasAttribute('client:visible')) {
      hydrateOnVisible(island);
    } else if (island.hasAttribute('client:media')) {
      hydrateOnMedia(island, island.getAttribute('client:media'));
    } else if (island.hasAttribute('client:only')) {
      hydrateOnLoad(island);
    }
  });
})();
</script>|}
