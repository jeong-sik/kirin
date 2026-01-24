(** Astro Integrations

    Support for multiple UI frameworks in a single project. *)

(** {1 Integration Types} *)

(** Integration configuration *)
type config = {
  name: string;
  renderer: string;
  client_script: string;
  server_script: string;
  deps: string list;
  dev_deps: string list;
}

(** Integration *)
type t = {
  config: config;
  enabled: bool;
  include_patterns: string list;
  exclude_patterns: string list;
}

(** {1 Built-in Integrations} *)

(** React integration *)
let react = {
  config = {
    name = "react";
    renderer = "@astrojs/react";
    client_script = "import { createElement } from 'react'; import { hydrateRoot } from 'react-dom/client';";
    server_script = "import { renderToString } from 'react-dom/server';";
    deps = ["react"; "react-dom"];
    dev_deps = ["@types/react"; "@types/react-dom"];
  };
  enabled = true;
  include_patterns = ["**/*.jsx"; "**/*.tsx"];
  exclude_patterns = [];
}

(** Vue integration *)
let vue = {
  config = {
    name = "vue";
    renderer = "@astrojs/vue";
    client_script = "import { createApp } from 'vue';";
    server_script = "import { renderToString } from 'vue/server-renderer';";
    deps = ["vue"];
    dev_deps = [];
  };
  enabled = true;
  include_patterns = ["**/*.vue"];
  exclude_patterns = [];
}

(** Svelte integration *)
let svelte = {
  config = {
    name = "svelte";
    renderer = "@astrojs/svelte";
    client_script = "import { mount } from 'svelte';";
    server_script = "import { render } from 'svelte/server';";
    deps = ["svelte"];
    dev_deps = ["@sveltejs/vite-plugin-svelte"];
  };
  enabled = true;
  include_patterns = ["**/*.svelte"];
  exclude_patterns = [];
}

(** Solid integration *)
let solid = {
  config = {
    name = "solid";
    renderer = "@astrojs/solid-js";
    client_script = "import { hydrate } from 'solid-js/web';";
    server_script = "import { renderToString } from 'solid-js/web';";
    deps = ["solid-js"];
    dev_deps = ["vite-plugin-solid"];
  };
  enabled = true;
  include_patterns = ["**/*.jsx"; "**/*.tsx"];
  exclude_patterns = [];
}

(** Preact integration *)
let preact = {
  config = {
    name = "preact";
    renderer = "@astrojs/preact";
    client_script = "import { hydrate } from 'preact';";
    server_script = "import { render } from 'preact-render-to-string';";
    deps = ["preact"];
    dev_deps = ["@preact/preset-vite"];
  };
  enabled = true;
  include_patterns = ["**/*.jsx"; "**/*.tsx"];
  exclude_patterns = [];
}

(** Lit integration *)
let lit = {
  config = {
    name = "lit";
    renderer = "@astrojs/lit";
    client_script = "import '@lit-labs/ssr-client/lit-element-hydrate-support.js';";
    server_script = "import { render } from '@lit-labs/ssr';";
    deps = ["lit"; "@lit-labs/ssr"];
    dev_deps = [];
  };
  enabled = true;
  include_patterns = ["**/*.ts"; "**/*.js"];
  exclude_patterns = [];
}

(** Alpine integration *)
let alpine = {
  config = {
    name = "alpine";
    renderer = "@astrojs/alpinejs";
    client_script = "import Alpine from 'alpinejs'; window.Alpine = Alpine; Alpine.start();";
    server_script = "";  (* Alpine is client-only *)
    deps = ["alpinejs"];
    dev_deps = [];
  };
  enabled = true;
  include_patterns = [];
  exclude_patterns = [];
}

(** {1 Integration Management} *)

(** Enable integration *)
let enable integration = { integration with enabled = true }

(** Disable integration *)
let disable integration = { integration with enabled = false }

(** Add include pattern *)
let include_pattern pattern integration =
  { integration with include_patterns = pattern :: integration.include_patterns }

(** Add exclude pattern *)
let exclude_pattern pattern integration =
  { integration with exclude_patterns = pattern :: integration.exclude_patterns }

(** {1 Component Detection} *)

(** Detect framework from file extension *)
let detect_framework filename =
  if Filename.check_suffix filename ".vue" then Some "vue"
  else if Filename.check_suffix filename ".svelte" then Some "svelte"
  else if Filename.check_suffix filename ".jsx" || Filename.check_suffix filename ".tsx" then
    (* Could be React, Solid, or Preact - need additional heuristics *)
    Some "react"  (* Default to React *)
  else
    None

(** Check if file matches integration *)
let matches_integration integration filename =
  let matches_include = integration.include_patterns = [] ||
    List.exists (fun p ->
      (* Simple glob matching *)
      let ext = Filename.extension filename in
      String.length p > 2 && String.sub p (String.length p - String.length ext) (String.length ext) = ext
    ) integration.include_patterns
  in
  let matches_exclude = List.exists (fun p ->
    let ext = Filename.extension filename in
    String.length p > 2 && String.sub p (String.length p - String.length ext) (String.length ext) = ext
  ) integration.exclude_patterns in
  integration.enabled && matches_include && not matches_exclude

(** {1 Rendering} *)

(** Get client script for integration *)
let get_client_script integration =
  integration.config.client_script

(** Get server script for integration *)
let get_server_script integration =
  integration.config.server_script

(** {1 Configuration Generation} *)

(** Generate astro.config.mjs integrations array *)
let generate_config integrations =
  let enabled = List.filter (fun i -> i.enabled) integrations in
  let imports = List.map (fun i ->
    Printf.sprintf "import %s from '%s';" i.config.name i.config.renderer
  ) enabled in
  let items = List.map (fun i -> Printf.sprintf "%s()" i.config.name) enabled in
  Printf.sprintf {|// Auto-generated integrations config
%s

export const integrations = [
  %s
];|} (String.concat "\n" imports) (String.concat ",\n  " items)

(** Generate package.json dependencies *)
let generate_deps integrations =
  let enabled = List.filter (fun i -> i.enabled) integrations in
  let deps = List.concat_map (fun i -> i.config.deps) enabled in
  let dev_deps = List.concat_map (fun i -> i.config.dev_deps) enabled in
  (deps, dev_deps)
