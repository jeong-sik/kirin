(** Kirin Browser - Client-side Framework (Phase 7)

    Browser-side companion to Kirin server framework.
    Provides DOM manipulation, History API routing, Fetch API, and SSR hydration.

    {b Quick Start:}
    {[
      open Kirin_browser

      let () =
        let root = get_by_id "app" in
        on_click root (fun _ -> log "Clicked!")
    ]}
*)

open Js_of_ocaml

(** {1 Core Types} *)

(** DOM Element type *)
type element = Dom_html.element Js.t

(** DOM Event type *)
type event = Dom_html.event Js.t

(** Location information *)
type location = {
  pathname : string;
  search : string;
  hash : string;
}

(** {1 Console} *)

(** Log to console *)
let log msg : unit =
  ignore (Js.Unsafe.meth_call Js.Unsafe.global##.console "log"
    [| Js.Unsafe.inject (Js.string msg) |])

(** Log error *)
let error msg : unit =
  ignore (Js.Unsafe.meth_call Js.Unsafe.global##.console "error"
    [| Js.Unsafe.inject (Js.string msg) |])

(** Log warning *)
let warn msg : unit =
  ignore (Js.Unsafe.meth_call Js.Unsafe.global##.console "warn"
    [| Js.Unsafe.inject (Js.string msg) |])

(** {1 DOM Access} *)

(** Get element by ID *)
let get_by_id id : element =
  Js.Opt.get 
    (Dom_html.document##getElementById (Js.string id))
    (fun () -> failwith ("Element not found: " ^ id))

(** Get element by ID (optional) *)
let get_by_id_opt id : element option =
  Js.Opt.to_option (Dom_html.document##getElementById (Js.string id))

(** Query selector *)
let query selector : element =
  Js.Opt.get
    (Dom_html.document##querySelector (Js.string selector))
    (fun () -> failwith ("No element matches: " ^ selector))

(** Query selector (optional) *)
let query_opt selector : element option =
  Js.Opt.to_option (Dom_html.document##querySelector (Js.string selector))

(** Get body element *)
let body () : element =
  (Dom_html.document##.body :> element)

(** {1 Element Manipulation} *)

(** Get inner HTML *)
let get_html (el : element) =
  Js.to_string el##.innerHTML

(** Set inner HTML *)
let set_html (el : element) html =
  el##.innerHTML := Js.string html

(** Get text content *)
let get_text (el : element) =
  Js.Opt.case el##.textContent
    (fun () -> "")
    Js.to_string

(** Set text content *)
let set_text (el : element) text =
  el##.textContent := Js.some (Js.string text)

(** Get attribute *)
let get_attr (el : element) name =
  Js.Opt.to_option (el##getAttribute (Js.string name))
  |> Option.map Js.to_string

(** Set attribute *)
let set_attr (el : element) name value =
  el##setAttribute (Js.string name) (Js.string value)

(** Add CSS class *)
let add_class (el : element) cls =
  el##.classList##add (Js.string cls)

(** Remove CSS class *)
let remove_class (el : element) cls =
  el##.classList##remove (Js.string cls)

(** Toggle CSS class *)
let toggle_class (el : element) cls =
  Js.to_bool (el##.classList##toggle (Js.string cls))

(** Has CSS class *)
let has_class (el : element) cls =
  Js.to_bool (el##.classList##contains (Js.string cls))

(** {1 Element Creation} *)

(** Create element *)
let create tag : element =
  (Dom_html.document##createElement (Js.string tag) :> element)

(** Append child *)
let append (parent : element) (child : element) =
  ignore (parent##appendChild (child :> Dom.node Js.t))

(** Remove element *)
let remove (el : element) =
  Js.Opt.iter el##.parentNode (fun parent ->
    ignore (parent##removeChild (el :> Dom.node Js.t)))

(** {1 Event Handling} *)

(** Add event listener *)
let on (el : element) event_type handler =
  let listener = Dom.handler (fun e -> handler e; Js._true) in
  ignore (Dom.addEventListener el (Dom.Event.make event_type) listener Js._false)

(** Click handler *)
let on_click (el : element) handler =
  on el "click" handler

(** Input handler *)  
let on_input (el : element) handler =
  on el "input" handler

(** Submit handler (prevents default) *)
let on_submit (el : element) handler =
  on el "submit" (fun e ->
    Dom.preventDefault e;
    handler e)

(** {1 Form Helpers} *)

(** Get input value *)
let get_value (el : element) =
  Js.to_string ((Js.Unsafe.coerce el)##.value)

(** Set input value *)
let set_value (el : element) value =
  (Js.Unsafe.coerce el)##.value := Js.string value

(** {1 History/Routing} *)

(** Get current location *)
let location () =
  let loc = Dom_html.window##.location in
  {
    pathname = Js.to_string loc##.pathname;
    search = Js.to_string loc##.search;
    hash = Js.to_string loc##.hash;
  }

(** Get current path *)
let path () = (location ()).pathname

(** Navigate to path (push state) *)
let navigate path =
  Dom_html.window##.history##pushState Js.null (Js.string "") (Js.some (Js.string path))

(** Replace current path *)
let replace path =
  Dom_html.window##.history##replaceState Js.null (Js.string "") (Js.some (Js.string path))

(** Go back *)
let back () = Dom_html.window##.history##back

(** Go forward *)
let forward () = Dom_html.window##.history##forward

(** Navigation callbacks *)
let navigate_callbacks : (location -> unit) list ref = ref []

(** Handle popstate *)
let handle_popstate _ =
  let loc = location () in
  List.iter (fun cb -> cb loc) !navigate_callbacks;
  Js._true

(** Initialize popstate listener *)
let init_popstate =
  let initialized = ref false in
  fun () ->
    if not !initialized then begin
      initialized := true;
      ignore (Dom.addEventListener Dom_html.window (Dom.Event.make "popstate")
        (Dom.handler handle_popstate) Js._false)
    end

(** Register navigation callback *)
let on_navigate callback =
  init_popstate ();
  navigate_callbacks := callback :: !navigate_callbacks

(** {1 Fetch API} *)

(** HTTP methods *)
type http_method = GET | POST | PUT | DELETE | PATCH

let method_string = function
  | GET -> "GET" | POST -> "POST" | PUT -> "PUT" 
  | DELETE -> "DELETE" | PATCH -> "PATCH"

(** Fetch response *)
type response = {
  ok : bool;
  status : int;
  _resp : Js.Unsafe.any;
}

(** Fetch URL *)
let fetch ?(meth = GET) ?(headers = []) ?body url callback =
  let headers_obj = Js.Unsafe.obj [||] in
  List.iter (fun (k, v) ->
    Js.Unsafe.set headers_obj (Js.string k) (Js.string v)
  ) headers;
  
  let init = Js.Unsafe.obj [|
    ("method", Js.Unsafe.inject (Js.string (method_string meth)));
    ("headers", Js.Unsafe.inject headers_obj);
  |] in
  (match body with
   | Some b -> Js.Unsafe.set init (Js.string "body") (Js.string b)
   | None -> ());
  
  let promise = Js.Unsafe.fun_call (Js.Unsafe.js_expr "fetch")
    [| Js.Unsafe.inject (Js.string url); Js.Unsafe.inject init |] in
  
  ignore (Js.Unsafe.meth_call promise "then" [|
    Js.Unsafe.inject (Js.wrap_callback (fun resp ->
      let r = {
        ok = Js.to_bool (Js.Unsafe.get resp (Js.string "ok"));
        status = Js.Unsafe.get resp (Js.string "status");
        _resp = resp;
      } in
      callback r))
  |])

(** Get response as text *)
let response_text resp callback =
  let promise = Js.Unsafe.meth_call resp._resp "text" [||] in
  ignore (Js.Unsafe.meth_call promise "then" [|
    Js.Unsafe.inject (Js.wrap_callback (fun text_js ->
      callback (Js.to_string text_js)))
  |])

(** Get response as JSON *)
let response_json resp callback =
  let promise = Js.Unsafe.meth_call resp._resp "text" [||] in
  ignore (Js.Unsafe.meth_call promise "then" [|
    Js.Unsafe.inject (Js.wrap_callback (fun text_js ->
      let text = Js.to_string text_js in
      try callback (Some (Yojson.Safe.from_string text))
      with _ -> callback None))
  |])

(** Convenience: GET request *)
let get url callback = fetch ~meth:GET url callback

(** Convenience: POST JSON *)
let post_json url body callback =
  fetch ~meth:POST 
    ~headers:[("Content-Type", "application/json")]
    ~body:(Yojson.Safe.to_string body)
    url callback

(** {1 Timers} *)

(** Set timeout (returns cancel function) *)
let set_timeout ms f =
  let id = Dom_html.window##setTimeout 
    (Js.wrap_callback f) (Js.float (float_of_int ms)) in
  fun () -> Dom_html.window##clearTimeout id

(** Set interval (returns cancel function) *)
let set_interval ms f =
  let id = Dom_html.window##setInterval 
    (Js.wrap_callback f) (Js.float (float_of_int ms)) in
  fun () -> Dom_html.window##clearInterval id

(** Request animation frame *)
let request_animation_frame f =
  ignore (Dom_html.window##requestAnimationFrame
    (Js.wrap_callback (fun _ -> f ())))

(** {1 Local Storage} *)

(** Get localStorage object (may not exist in all contexts) *)
let get_storage () =
  Js.Optdef.to_option Dom_html.window##.localStorage

(** Get from localStorage *)
let storage_get key =
  match get_storage () with
  | None -> None
  | Some storage ->
      Js.Opt.to_option (storage##getItem (Js.string key))
      |> Option.map Js.to_string

(** Set in localStorage *)
let storage_set key value =
  match get_storage () with
  | None -> ()
  | Some storage ->
      storage##setItem (Js.string key) (Js.string value)

(** Remove from localStorage *)
let storage_remove key =
  match get_storage () with
  | None -> ()
  | Some storage ->
      storage##removeItem (Js.string key)

(** {1 Hydration} *)

(** Data attribute prefix *)
let data_prefix = "data-kirin-"

(** Get kirin data attribute *)
let get_data (el : element) key =
  get_attr el (data_prefix ^ key)

(** Set kirin data attribute *)
let set_data (el : element) key value =
  set_attr el (data_prefix ^ key) value

(** Component registry *)
let components : (string * (element -> string -> unit)) list ref = ref []

(** Register component *)
let register_component name handler =
  components := (name, handler) :: !components

(** Find components in element *)
let find_components (root : element) =
  let selector = "[" ^ data_prefix ^ "component]" in
  let nodes = root##querySelectorAll (Js.string selector) in
  let len = nodes##.length in
  let rec collect i acc =
    if i < 0 then acc
    else
      match Js.Opt.to_option (nodes##item i) with
      | None -> collect (i - 1) acc
      | Some node -> collect (i - 1) ((Js.Unsafe.coerce node : element) :: acc)
  in
  collect (len - 1) []

(** Hydrate element *)
let hydrate_element (el : element) =
  match get_data el "component" with
  | None -> ()
  | Some name ->
      match List.assoc_opt name !components with
      | None -> warn (Printf.sprintf "[Kirin] Unknown component: %s" name)
      | Some handler ->
          let state = Option.value ~default:"" (get_data el "state") in
          handler el state;
          set_data el "hydrated" "true"

(** Hydrate root element and all components within *)
let hydrate (root : element) =
  let comps = find_components root in
  List.iter hydrate_element comps;
  hydrate_element root;
  log (Printf.sprintf "[Kirin] Hydrated %d components" (List.length comps))

(** {1 App Start} *)

(** Start Kirin browser app *)
let start ?(root_id = "app") ?(routing = true) () =
  let root = get_by_id root_id in
  if routing then init_popstate ();
  hydrate root;
  root
