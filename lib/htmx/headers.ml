(** HTMX Response Headers

    Comprehensive support for all HTMX response headers.
    See: https://htmx.org/reference/#response_headers *)

open Kirin

(** {1 Core Response Headers} *)

(** Trigger client-side events *)
let trigger events resp =
  let value = String.concat ", " events in
  Response.with_header "HX-Trigger" value resp

(** Trigger events after settle *)
let trigger_after_settle events resp =
  let value = String.concat ", " events in
  Response.with_header "HX-Trigger-After-Settle" value resp

(** Trigger events after swap *)
let trigger_after_swap events resp =
  let value = String.concat ", " events in
  Response.with_header "HX-Trigger-After-Swap" value resp

(** Trigger with JSON data *)
let trigger_with_data event_data resp =
  let json = `Assoc event_data |> Yojson.Safe.to_string in
  Response.with_header "HX-Trigger" json resp

(** {1 Navigation Headers} *)

(** Push URL into browser history *)
let push_url url resp =
  Response.with_header "HX-Push-Url" url resp

(** Replace current URL without history *)
let replace_url url resp =
  Response.with_header "HX-Replace-Url" url resp

(** Full page redirect *)
let redirect url resp =
  Response.with_header "HX-Redirect" url resp

(** Refresh the page *)
let refresh resp =
  Response.with_header "HX-Refresh" "true" resp

(** {1 Content Control Headers} *)

(** Override the target element *)
let retarget selector resp =
  Response.with_header "HX-Retarget" selector resp

(** Override the swap method *)
let reswap method_ resp =
  Response.with_header "HX-Reswap" method_ resp

(** Override the select filter *)
let reselect selector resp =
  Response.with_header "HX-Reselect" selector resp

(** {1 Swap Methods} *)

type swap =
  | Inner_html
  | Outer_html
  | Before_begin
  | After_begin
  | Before_end
  | After_end
  | Delete
  | None

let swap_to_string = function
  | Inner_html -> "innerHTML"
  | Outer_html -> "outerHTML"
  | Before_begin -> "beforebegin"
  | After_begin -> "afterbegin"
  | Before_end -> "beforeend"
  | After_end -> "afterend"
  | Delete -> "delete"
  | None -> "none"

(** Set swap method with type safety *)
let reswap_typed swap resp =
  Response.with_header "HX-Reswap" (swap_to_string swap) resp

(** Reswap with modifiers (e.g., "innerHTML swap:1s settle:500ms") *)
let reswap_with_modifiers swap ?(swap_delay : float option) ?(settle_delay : float option)
    ?(scroll : string option) ?(show : string option) () resp =
  let base = swap_to_string swap in
  let mods = [] in
  let mods = match swap_delay with
    | Some d -> Printf.sprintf "swap:%.0fms" (d *. 1000.) :: mods
    | None -> mods
  in
  let mods = match settle_delay with
    | Some d -> Printf.sprintf "settle:%.0fms" (d *. 1000.) :: mods
    | None -> mods
  in
  let mods = match scroll with
    | Some s -> Printf.sprintf "scroll:%s" s :: mods
    | None -> mods
  in
  let mods = match show with
    | Some s -> Printf.sprintf "show:%s" s :: mods
    | None -> mods
  in
  let value = match mods with
    | [] -> base
    | _ -> base ^ " " ^ String.concat " " (List.rev mods)
  in
  Response.with_header "HX-Reswap" value resp

(** {1 Request Control} *)

(** Set request location with context *)
let location ?(target : string option) ?(swap : swap option) ?(source : string option) path resp =
  let obj = [("path", `String path)] in
  let obj = match target with Some t -> ("target", `String t) :: obj | None -> obj in
  let obj = match swap with Some s -> ("swap", `String (swap_to_string s)) :: obj | None -> obj in
  let obj = match source with Some s -> ("source", `String s) :: obj | None -> obj in
  let json = `Assoc obj |> Yojson.Safe.to_string in
  Response.with_header "HX-Location" json resp

(** Simple location redirect *)
let location_simple path resp =
  Response.with_header "HX-Location" path resp

(** {1 Helper Functions} *)

(** Check if request is from HTMX *)
let is_htmx_request req =
  match Request.header "HX-Request" req with
  | Some "true" -> true
  | _ -> false

(** Check if this is a boosted request *)
let is_boosted req =
  match Request.header "HX-Boosted" req with
  | Some "true" -> true
  | _ -> false

(** Get the current URL from request *)
let current_url req =
  Request.header "HX-Current-URL" req

(** Get the triggering element's ID *)
let trigger_id req =
  Request.header "HX-Trigger" req

(** Get the triggering element's name *)
let trigger_name req =
  Request.header "HX-Trigger-Name" req

(** Get the target element's ID *)
let target_id req =
  Request.header "HX-Target" req

(** Get the prompt response *)
let prompt_response req =
  Request.header "HX-Prompt" req
