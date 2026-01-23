(** Kirin Browser - History API Bindings

    OCaml bindings for the browser History API.
    Enables SPA-style client-side navigation.

    {b Example:}
    {[
      (* Navigate to a new URL *)
      History.push_state "/users/123";

      (* Listen for navigation events *)
      History.on_popstate (fun event ->
        render_page event.path
      );
    ]}
*)

open Js_of_ocaml

(** {1 Types} *)

(** Navigation event *)
type nav_event = {
  path : string;
  state : Js.Unsafe.any option;
}

(** {1 Location Helpers} *)

module Location = struct
  (** Get current pathname *)
  let pathname () : string =
    Js.to_string Dom_html.window##.location##.pathname

  (** Get current search (query string) *)
  let search () : string =
    Js.to_string Dom_html.window##.location##.search

  (** Get current hash *)
  let hash () : string =
    Js.to_string Dom_html.window##.location##.hash

  (** Get full URL *)
  let href () : string =
    Js.to_string Dom_html.window##.location##.href

  (** Get origin (protocol + host) *)
  let origin () : string =
    Js.to_string (Js.Unsafe.get Dom_html.window##.location (Js.string "origin"))

  (** Parse query string into key-value pairs *)
  let query_params () : (string * string) list =
    let search = search () in
    if String.length search <= 1 then []
    else
      let query = String.sub search 1 (String.length search - 1) in
      String.split_on_char '&' query
      |> List.filter_map (fun pair ->
        match String.split_on_char '=' pair with
        | [k; v] -> Some (k, v)
        | [k] -> Some (k, "")
        | _ -> None)
end

(** {1 History API} *)

(** Push a new state to history (navigates without reload) *)
let push_state ?(state = Js.Unsafe.inject Js.null) ?(title = "") path =
  Dom_html.window##.history##pushState state (Js.string title) (Js.Opt.return (Js.string path))

(** Replace current state in history *)
let replace_state ?(state = Js.Unsafe.inject Js.null) ?(title = "") path =
  Dom_html.window##.history##replaceState state (Js.string title) (Js.Opt.return (Js.string path))

(** Go back in history *)
let back () =
  Dom_html.window##.history##back

(** Go forward in history *)
let forward () =
  Dom_html.window##.history##forward

(** Go to specific position in history *)
let go n =
  Dom_html.window##.history##go n

(** Get history length *)
let length () : int =
  Dom_html.window##.history##.length

(** {1 Event Listeners} *)

(** Listener handle for removal *)
type listener_id = Dom_html.event_listener_id

(** Listen for popstate events (browser back/forward) *)
let on_popstate (callback : nav_event -> unit) : listener_id =
  Dom_html.addEventListener
    Dom_html.window
    Dom_html.Event.popstate
    (Dom_html.handler (fun event ->
      let state =
        let s = Js.Unsafe.get event (Js.string "state") in
        if Js.Opt.test (Js.Opt.return s) && not (Js.Unsafe.equals s Js.null) then
          Some s
        else
          None
      in
      callback { path = Location.pathname (); state };
      Js._true))
    Js._false

(** Remove a popstate listener *)
let remove_listener (id : listener_id) =
  Dom_html.removeEventListener id

(** {1 Navigation Helpers} *)

(** Navigate to a path (push state + trigger callback) *)
let navigate ?(callback : (string -> unit) option) path =
  push_state path;
  match callback with
  | Some cb -> cb path
  | None -> ()

(** Check if History API is supported *)
let is_supported () : bool =
  Js.Optdef.test (Js.Unsafe.get Dom_html.window (Js.string "history"))
  && Js.Optdef.test (Js.Unsafe.get Dom_html.window##.history (Js.string "pushState"))

(** {1 Hash-based Routing (fallback)} *)

module Hash = struct
  (** Get current hash (without #) *)
  let get () : string =
    let hash = Location.hash () in
    if String.length hash > 0 && hash.[0] = '#' then
      String.sub hash 1 (String.length hash - 1)
    else
      hash

  (** Set hash *)
  let set path =
    Dom_html.window##.location##.hash := Js.string ("#" ^ path)

  (** Listen for hash changes *)
  let on_change (callback : string -> unit) : listener_id =
    Dom_html.addEventListener
      Dom_html.window
      (Dom_html.Event.make "hashchange")
      (Dom_html.handler (fun _ ->
        callback (get ());
        Js._true))
      Js._false
end
