(** Out-of-Band (OOB) Swap Helpers

    Generate OOB swap elements for updating multiple parts of the page.
    See: https://htmx.org/attributes/hx-swap-oob/ *)

(** {1 OOB Swap Strategies} *)

type swap_strategy =
  | True          (** Use element ID, innerHTML *)
  | Inner_html    (** innerHTML swap *)
  | Outer_html    (** outerHTML swap *)
  | Before_begin  (** Insert before element *)
  | After_begin   (** Insert at start of element *)
  | Before_end    (** Insert at end of element *)
  | After_end     (** Insert after element *)
  | Delete        (** Delete the element *)
  | None          (** Don't swap, for events only *)

let strategy_to_string = function
  | True -> "true"
  | Inner_html -> "innerHTML"
  | Outer_html -> "outerHTML"
  | Before_begin -> "beforebegin"
  | After_begin -> "afterbegin"
  | Before_end -> "beforeend"
  | After_end -> "afterend"
  | Delete -> "delete"
  | None -> "none"

(** {1 OOB Element Generation} *)

(** Create an OOB swap attribute *)
let attr ?(strategy = True) ?(target : string option) () =
  match target with
  | Some t -> Printf.sprintf "hx-swap-oob=\"%s:%s\"" (strategy_to_string strategy) t
  | None -> Printf.sprintf "hx-swap-oob=\"%s\"" (strategy_to_string strategy)

(** Wrap content with OOB swap div *)
let div ?(strategy = True) ~id content =
  Printf.sprintf "<div id=\"%s\" hx-swap-oob=\"%s\">%s</div>" id (strategy_to_string strategy) content

(** Wrap content with OOB swap targeting another element *)
let div_target ~strategy ~target content =
  Printf.sprintf "<div hx-swap-oob=\"%s:%s\">%s</div>" (strategy_to_string strategy) target content

(** {1 Common OOB Patterns} *)

(** Update a notification area *)
let notification ~id ?(class_ = "notification") message =
  Printf.sprintf "<div id=\"%s\" class=\"%s\" hx-swap-oob=\"true\">%s</div>" id class_ message

(** Update a success message *)
let success_message ~id message =
  notification ~id ~class_:"notification success" message

(** Update an error message *)
let error_message ~id message =
  notification ~id ~class_:"notification error" message

(** Clear a container *)
let clear ~id =
  Printf.sprintf "<div id=\"%s\" hx-swap-oob=\"true\"></div>" id

(** Delete an element *)
let delete ~id =
  Printf.sprintf "<div id=\"%s\" hx-swap-oob=\"delete\"></div>" id

(** Update multiple elements at once *)
let multi elements =
  String.concat "\n" elements

(** {1 Form-Specific OOB} *)

(** Update form validation message *)
let field_error ~field_id message =
  Printf.sprintf "<span id=\"%s-error\" class=\"field-error\" hx-swap-oob=\"true\">%s</span>" field_id message

(** Clear field error *)
let clear_field_error ~field_id =
  Printf.sprintf "<span id=\"%s-error\" class=\"field-error\" hx-swap-oob=\"true\"></span>" field_id

(** Update field with validation state *)
let field_with_state ~field_id ~state content =
  Printf.sprintf "<div id=\"%s-wrapper\" class=\"field-wrapper %s\" hx-swap-oob=\"true\">%s</div>"
    field_id state content

(** {1 Counter/Badge Updates} *)

(** Update a counter badge *)
let counter ~id count =
  Printf.sprintf "<span id=\"%s\" class=\"counter\" hx-swap-oob=\"true\">%d</span>" id count

(** Update cart badge *)
let cart_badge count =
  counter ~id:"cart-count" count

(** Update notification badge *)
let notification_badge count =
  counter ~id:"notification-count" count

(** {1 List Operations} *)

(** Append item to list *)
let append_to_list ~list_id item_html =
  Printf.sprintf "<div hx-swap-oob=\"beforeend:#%s\">%s</div>" list_id item_html

(** Prepend item to list *)
let prepend_to_list ~list_id item_html =
  Printf.sprintf "<div hx-swap-oob=\"afterbegin:#%s\">%s</div>" list_id item_html

(** Replace list item *)
let replace_item ~item_id new_html =
  Printf.sprintf "<div id=\"%s\" hx-swap-oob=\"outerHTML\">%s</div>" item_id new_html

(** Remove list item *)
let remove_item ~item_id =
  Printf.sprintf "<div id=\"%s\" hx-swap-oob=\"delete\"></div>" item_id

(** {1 Response Builder} *)

(** Build OOB response with main content and OOB updates *)
let response ~main_content oob_updates =
  main_content ^ "\n" ^ multi oob_updates

(** Build response with only OOB updates (no main content) *)
let response_oob_only oob_updates =
  multi oob_updates
