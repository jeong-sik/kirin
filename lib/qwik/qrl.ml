(** Qwik QRL (Qwik URL)

    Serialized references to functions for lazy loading.

    QRL is the key to Qwik's resumability - instead of hydrating all event
    handlers, they're serialized as URLs that load on-demand. *)

(** {1 QRL Types} *)

(** QRL capture type - what the QRL closes over *)
type capture =
  | NoCapture                    (* No closure variables *)
  | Capture of string list       (* List of captured variable names *)
  | CaptureRef of int list       (* References to container state *)

(** QRL definition *)
type t = {
  chunk: string;                 (* JS chunk containing the function *)
  symbol: string;                (* Export name in the chunk *)
  capture: capture;              (* Captured variables *)
  dev_name: string option;       (* Development-only name for debugging *)
}

(** {1 QRL Construction} *)

(** Create a QRL *)
let create ~chunk ~symbol ?(capture=NoCapture) ?dev_name () = {
  chunk;
  symbol;
  capture;
  dev_name;
}

(** Create QRL from chunk and symbol *)
let of_string ~chunk ~symbol =
  create ~chunk ~symbol ()

(** Create QRL with captures *)
let with_captures ~chunk ~symbol captures =
  create ~chunk ~symbol ~capture:(Capture captures) ()

(** Create QRL with reference captures *)
let with_refs ~chunk ~symbol refs =
  create ~chunk ~symbol ~capture:(CaptureRef refs) ()

(** {1 QRL Serialization} *)

(** Serialize QRL to string (for HTML attributes) *)
let to_string qrl =
  let base = Printf.sprintf "%s#%s" qrl.chunk qrl.symbol in
  match qrl.capture with
  | NoCapture -> base
  | Capture vars ->
    Printf.sprintf "%s[%s]" base (String.concat " " vars)
  | CaptureRef refs ->
    Printf.sprintf "%s[%s]" base (String.concat " " (List.map string_of_int refs))

(** Parse QRL from string *)
let of_serialized s =
  (* Parse "chunk#symbol" or "chunk#symbol[captures]" *)
  let parse_base s =
    match String.split_on_char '#' s with
    | [chunk; rest] ->
      (* Check for captures *)
      if String.contains rest '[' then
        let idx = String.index rest '[' in
        let symbol = String.sub rest 0 idx in
        let captures_str = String.sub rest (idx + 1) (String.length rest - idx - 2) in
        let captures = String.split_on_char ' ' captures_str in
        (* Try to parse as refs *)
        (try
          let refs = List.map int_of_string captures in
          Some { chunk; symbol; capture = CaptureRef refs; dev_name = None }
        with _ ->
          Some { chunk; symbol; capture = Capture captures; dev_name = None })
      else
        Some { chunk; symbol = rest; capture = NoCapture; dev_name = None }
    | _ -> None
  in
  parse_base s

(** {1 QRL Attributes} *)

(** Generate on:click QRL attribute *)
let on_click qrl =
  Printf.sprintf {|on:click="%s"|} (to_string qrl)

(** Generate on:input QRL attribute *)
let on_input qrl =
  Printf.sprintf {|on:input="%s"|} (to_string qrl)

(** Generate on:submit QRL attribute *)
let on_submit qrl =
  Printf.sprintf {|on:submit="%s"|} (to_string qrl)

(** Generate custom event QRL attribute *)
let on_event ~event qrl =
  Printf.sprintf {|on:%s="%s"|} event (to_string qrl)

(** Generate window event QRL attribute *)
let on_window ~event qrl =
  Printf.sprintf {|on-window:%s="%s"|} event (to_string qrl)

(** Generate document event QRL attribute *)
let on_document ~event qrl =
  Printf.sprintf {|on-document:%s="%s"|} event (to_string qrl)

(** {1 QRL Prefetching} *)

(** Prefetch strategy *)
type prefetch_strategy =
  | PrefetchEvent     (* Prefetch on event (hover/focus) *)
  | PrefetchVisible   (* Prefetch when visible *)
  | PrefetchLoad      (* Prefetch on page load *)
  | PrefetchNone      (* No prefetching *)

(** Generate prefetch hint *)
let prefetch_hint ?(strategy=PrefetchEvent) qrl =
  let strategy_str = match strategy with
    | PrefetchEvent -> "event"
    | PrefetchVisible -> "visible"
    | PrefetchLoad -> "load"
    | PrefetchNone -> "none"
  in
  Printf.sprintf {|<link rel="modulepreload" href="/%s" data-qwik-prefetch="%s" />|}
    qrl.chunk strategy_str

(** {1 Component QRL} *)

(** Component QRL (for lazy loading components) *)
type component_qrl = {
  qrl: t;
  styles: string list;      (* Associated style chunks *)
  slot_projection: bool;    (* Whether component uses slots *)
}

(** Create component QRL *)
let component ~chunk ~symbol ?(styles=[]) ?(slot_projection=false) () = {
  qrl = create ~chunk ~symbol ();
  styles;
  slot_projection;
}

(** {1 Serialization} *)

(** QRL to JSON *)
let to_json qrl =
  let capture_json = match qrl.capture with
    | NoCapture -> `Null
    | Capture vars -> `List (List.map (fun v -> `String v) vars)
    | CaptureRef refs -> `List (List.map (fun r -> `Int r) refs)
  in
  `Assoc [
    ("chunk", `String qrl.chunk);
    ("symbol", `String qrl.symbol);
    ("capture", capture_json);
    ("devName", match qrl.dev_name with Some n -> `String n | None -> `Null);
  ]

(** QRL from JSON *)
let of_json json =
  let open Yojson.Safe.Util in
  let chunk = json |> member "chunk" |> to_string in
  let symbol = json |> member "symbol" |> to_string in
  let dev_name = json |> member "devName" |> to_string_option in
  let capture = match json |> member "capture" with
    | `Null -> NoCapture
    | `List items ->
      (try
        CaptureRef (List.map to_int items)
      with _ ->
        Capture (List.map to_string items))
    | _ -> NoCapture
  in
  { chunk; symbol; capture; dev_name }
