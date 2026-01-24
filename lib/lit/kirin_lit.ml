(** Kirin Lit Integration

    Unified API for Lit/Web Components support in Kirin.
    Provides Custom Elements, Shadow DOM, and SSR capabilities. *)

(** Re-export modules *)
module Element = Element
module Template = Template
module Shadow_dom = Shadow_dom
module Ssr = Ssr

(** {1 Quick Start} *)

(** Create a simple custom element *)
let element ~tag_name ~class_name ?(props = []) ?(styles = []) template =
  Element.create ~tag_name ~class_name ~properties:props ~styles template

(** Create a property *)
let prop = Element.string_prop
let number = Element.number_prop
let boolean = Element.boolean_prop
let object_ = Element.object_prop
let array = Element.array_prop
let state = Element.state_prop

(** {1 Shadow DOM Helpers} *)

(** Create shadow root with styles *)
let shadow ?(mode = Shadow_dom.Open) ?(styles = []) content =
  Shadow_dom.create ~mode ~styles content

(** Create declarative shadow DOM HTML *)
let declarative_shadow = Shadow_dom.to_declarative_shadow_dom

(** Render component with shadow DOM *)
let render_component = Shadow_dom.render_component

(** Include polyfill for older browsers *)
let with_polyfill = Shadow_dom.with_polyfill

(** {1 Template Helpers} *)

(** Create template from parts *)
let template = Template.create
let text = Template.text
let property = Template.prop
let computed = Template.computed
let event = Template.event
let slot = Template.slot
let when_ = Template.when_
let repeat = Template.repeat

(** Render template to Lit html`` literal *)
let to_html = Template.to_html_literal

(** Render template statically with data *)
let render_static = Template.render_static

(** {1 Common Directives} *)

let class_map = Template.class_map
let style_map = Template.style_map
let if_defined = Template.if_defined
let live = Template.live
let ref_ = Template.ref_directive
let cache = Template.cache

(** {1 SSR Engine} *)

(** Create SSR engine with default config *)
let create_ssr ?(config = Ssr.default_config) () =
  Ssr.create config

(** Render component to HTML *)
let ssr = Ssr.render

(** Render with fallback *)
let ssr_with_fallback = Ssr.render_with_fallback

(** Render multiple components *)
let ssr_page = Ssr.render_page

(** {1 Kirin Handlers} *)

(** Create handler for component *)
let handler = Ssr.handler

(** Create handler with dynamic props *)
let handler_with_props = Ssr.handler_with_props

(** {1 Code Generation} *)

(** Generate TypeScript class *)
let to_typescript = Element.to_typescript

(** Generate JavaScript class *)
let to_javascript = Element.to_javascript

(** {1 Full Example} *)

(**
{[
   (* Define a custom element *)
   let my_card = Kirin_lit.(element
     ~tag_name:"my-card"
     ~class_name:"MyCard"
     ~props:[
       prop "title" ~reflect:true;
       number "count" ~default:(Some 0.0);
       boolean "active" ~default:(Some false);
     ]
     ~styles:[
       ":host { display: block; padding: 16px; }";
       ".title { font-size: 1.5em; }";
     ]
     {|<div class="title">${this.title}</div>
       <p>Count: ${this.count}</p>
       <slot></slot>|}
   )

   (* Generate TypeScript *)
   let ts_code = Kirin_lit.to_typescript my_card

   (* SSR with Declarative Shadow DOM *)
   let engine = Kirin_lit.create_ssr ()

   let routes = [
     Kirin.get "/card" (Kirin_lit.handler engine ~tag_name:"my-card");
   ]
]}
*)

(** {1 Serialization} *)

(** Element to JSON *)
let element_to_json = Element.to_json

(** Template to JSON *)
let template_to_json = Template.to_json

(** Shadow DOM to JSON *)
let shadow_to_json = Shadow_dom.to_json

(** SSR config to JSON *)
let config_to_json = Ssr.config_to_json

(** SSR stats to JSON *)
let stats_to_json = Ssr.stats_to_json
