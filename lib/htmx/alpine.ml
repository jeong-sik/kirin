(** Alpine.js Integration Helpers

    Generate Alpine.js directives for reactive behavior.
    See: https://alpinejs.dev/ *)

(** {1 Data Binding} *)

(** Create x-data attribute with JSON object *)
let data fields =
  let pairs = List.map (fun (k, v) -> Printf.sprintf "%s: %s" k v) fields in
  Printf.sprintf "{ %s }" (String.concat ", " pairs)

(** x-data with empty object *)
let data_empty = "{}"

(** {1 Reactive Directives} *)

(** x-show: conditionally show element *)
let show expr = ("x-show", expr)

(** x-if: conditionally render element (must be on template) *)
let if_ expr = ("x-if", expr)

(** x-for: loop over items *)
let for_ item_expr = ("x-for", item_expr)

(** x-text: set text content *)
let text expr = ("x-text", expr)

(** x-html: set inner HTML *)
let html expr = ("x-html", expr)

(** {1 Event Handlers} *)

(** x-on:event or @event shorthand *)
let on event action = (Printf.sprintf "x-on:%s" event, action)

(** @click shorthand *)
let on_click action = ("@click", action)

(** @submit shorthand *)
let on_submit action = ("@submit", action)

(** @keyup shorthand *)
let on_keyup action = ("@keyup", action)

(** @change shorthand *)
let on_change action = ("@change", action)

(** @input shorthand *)
let on_input action = ("@input", action)

(** {1 Two-Way Binding} *)

(** x-model: two-way data binding *)
let model name = ("x-model", name)

(** x-model with modifier *)
let model_debounce name ms =
  (Printf.sprintf "x-model.debounce.%dms" ms, name)

(** x-model.lazy *)
let model_lazy name = ("x-model.lazy", name)

(** x-model.number *)
let model_number name = ("x-model.number", name)

(** {1 Attribute Binding} *)

(** x-bind:attr or :attr shorthand *)
let bind attr expr = (Printf.sprintf ":%s" attr, expr)

(** :class binding *)
let bind_class expr = (":class", expr)

(** :style binding *)
let bind_style expr = (":style", expr)

(** :disabled binding *)
let bind_disabled expr = (":disabled", expr)

(** {1 Lifecycle Hooks} *)

(** x-init: run on element init *)
let init expr = ("x-init", expr)

(** x-effect: reactive side effect *)
let x_effect expr = ("x-effect", expr)

(** {1 References} *)

(** x-ref: element reference *)
let ref_ name = ("x-ref", name)

(** Access ref in expression *)
let refs name = Printf.sprintf "$refs.%s" name

(** {1 Transitions} *)

(** x-transition: default transition *)
let transition = ("x-transition", "")

(** x-transition with named stages *)
let transition_named ~enter ~leave =
  [
    ("x-transition:enter", enter);
    ("x-transition:leave", leave);
  ]

(** Full transition config *)
let transition_full ~enter ~enter_start ~enter_end ~leave ~leave_start ~leave_end =
  [
    ("x-transition:enter", enter);
    ("x-transition:enter-start", enter_start);
    ("x-transition:enter-end", enter_end);
    ("x-transition:leave", leave);
    ("x-transition:leave-start", leave_start);
    ("x-transition:leave-end", leave_end);
  ]

(** Fade transition preset *)
let transition_fade =
  transition_full
    ~enter:"transition ease-out duration-300"
    ~enter_start:"opacity-0"
    ~enter_end:"opacity-100"
    ~leave:"transition ease-in duration-200"
    ~leave_start:"opacity-100"
    ~leave_end:"opacity-0"

(** Scale transition preset *)
let transition_scale =
  transition_full
    ~enter:"transition ease-out duration-300"
    ~enter_start:"opacity-0 transform scale-95"
    ~enter_end:"opacity-100 transform scale-100"
    ~leave:"transition ease-in duration-200"
    ~leave_start:"opacity-100 transform scale-100"
    ~leave_end:"opacity-0 transform scale-95"

(** {1 Special Directives} *)

(** x-cloak: hide until Alpine loads *)
let cloak = ("x-cloak", "")

(** x-ignore: skip Alpine processing *)
let ignore = ("x-ignore", "")

(** x-teleport: move element elsewhere *)
let teleport selector = ("x-teleport", selector)

(** {1 Magic Properties} *)

(** $el: current element *)
let el = "$el"

(** $refs: element references *)
let all_refs = "$refs"

(** $store: global store *)
let store name = Printf.sprintf "$store.%s" name

(** $watch: watch for changes *)
let watch prop callback = Printf.sprintf "$watch('%s', %s)" prop callback

(** $nextTick: after DOM update *)
let next_tick callback = Printf.sprintf "$nextTick(%s)" callback

(** $dispatch: dispatch custom event *)
let dispatch event = Printf.sprintf "$dispatch('%s')" event

(** $dispatch with detail *)
let dispatch_detail event detail =
  Printf.sprintf "$dispatch('%s', %s)" event detail

(** {1 Stores} *)

(** Define a global store (for script tag) *)
let define_store name data =
  Printf.sprintf "Alpine.store('%s', %s)" name data

(** {1 Render Helpers} *)

(** Render attributes as HTML string *)
let attrs_to_string attrs =
  attrs
  |> List.map (fun (k, v) ->
       if v = "" then k else Printf.sprintf "%s=\"%s\"" k v)
  |> String.concat " "

(** Create a component div with x-data *)
let component ?(id : string option) ?(class_ : string option) data_obj children =
  let attrs = [] in
  let attrs = match id with Some i -> Printf.sprintf "id=\"%s\"" i :: attrs | None -> attrs in
  let attrs = match class_ with Some c -> Printf.sprintf "class=\"%s\"" c :: attrs | None -> attrs in
  let attrs = Printf.sprintf "x-data=\"%s\"" data_obj :: attrs in
  Printf.sprintf "<div %s>%s</div>" (String.concat " " attrs) children
