(** Kirin Alpine.js Integration

    Unified API for Alpine.js support in Kirin.
    Provides directives, components, stores, and SSR. *)

(** Re-export modules *)
module Directive = Directive
module Component = Component
module Store = Store
module Ssr = Ssr

(** {1 Quick Directives} *)

(** x-data directive *)
let data = Directive.data

(** x-init directive *)
let init = Directive.init

(** x-show directive *)
let show = Directive.show

(** x-if directive *)
let if_ = Directive.if_

(** x-bind directive *)
let bind = Directive.bind

(** x-on directive *)
let on = Directive.on

(** x-text directive *)
let text = Directive.text

(** x-html directive *)
let html = Directive.html

(** x-model directive *)
let model = Directive.model

(** x-for directive *)
let for_ = Directive.for_

(** x-ref directive *)
let ref_ = Directive.ref_

(** x-cloak directive *)
let cloak = Directive.cloak

(** {1 Event Modifiers} *)

let prevent = Directive.Prevent
let stop = Directive.Stop
let outside = Directive.Outside
let window = Directive.Window
let document = Directive.Document
let once = Directive.Once
let debounce = fun ms -> Directive.Debounce ms
let throttle = fun ms -> Directive.Throttle ms
let self = Directive.Self
let passive = Directive.Passive
let capture = Directive.Capture

(** {1 Component Helpers} *)

(** Create component *)
let component = Component.create

(** Add property to component *)
let prop = Component.with_property

(** Property types *)
let string_prop = Component.string_prop
let number_prop = Component.number_prop
let boolean_prop = Component.boolean_prop
let array_prop = Component.array_prop
let object_prop = Component.object_prop

(** Add method to component *)
let method_ = Component.with_method

(** {1 Store Helpers} *)

(** Create store *)
let store = Store.create

(** Store from object *)
let store_from_object = Store.from_object

(** Store accessor *)
let store_accessor = Store.accessor

(** {1 SSR Helpers} *)

(** Default SSR config *)
let default_config = Ssr.default_config

(** Create SSR state *)
let ssr_state = Ssr.empty_state

(** Head scripts *)
let head_scripts = Ssr.head_scripts

(** Init script *)
let init_script = Ssr.init_script

(** Render page *)
let render_page = Ssr.render_page

(** Kirin response *)
let response = Ssr.response

(** Kirin handler *)
let handler = Ssr.handler

(** {1 Element Rendering} *)

(** Render element with directives *)
let element = Ssr.render_element

(** Render with x-cloak *)
let cloaked = Ssr.render_cloaked

(** {1 Full Example} *)

(**
{[
   (* Define a counter component *)
   let counter = Kirin_alpine.(component "counter"
     |> prop ~default:(`Int 0) `Number "count"
     |> method_ "increment" [] "this.count++"
     |> method_ "decrement" [] "this.count--"
   )

   (* Define a global store *)
   let app_store = Kirin_alpine.(store "app"
     |> Store.with_property "user" `Null
     |> Store.with_property "theme" (`String "light")
   )

   (* Create SSR state *)
   let state = Kirin_alpine.(ssr_state
     |> Ssr.with_component counter
     |> Ssr.with_store app_store
     |> Ssr.with_data "serverTime" (`String "2024-01-15T12:00:00Z")
   )

   (* Create Kirin handler *)
   let routes = [
     Kirin.get "/" (fun _req ->
       let body = {|
         <div x-data="counter">
           <button @click="decrement">-</button>
           <span x-text="count"></span>
           <button @click="increment">+</button>
         </div>
       |} in
       Kirin_alpine.response ~config:default_config ~state ~body
     )
   ]
]}
*)

(** {1 Serialization} *)

(** Directive to JSON *)
let directive_to_json = Directive.to_json

(** Component to JSON *)
let component_to_json = Component.to_json

(** Store to JSON *)
let store_to_json = Store.to_json

(** Config to JSON *)
let config_to_json = Ssr.config_to_json

(** State to JSON *)
let state_to_json = Ssr.state_to_json
