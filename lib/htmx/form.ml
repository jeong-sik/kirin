(** HTMX Form Helpers

    Generate form elements with HTMX attributes and validation. *)

(** {1 Form Attributes} *)

(** Create hx-post attribute *)
let post url = ("hx-post", url)

(** Create hx-put attribute *)
let put url = ("hx-put", url)

(** Create hx-patch attribute *)
let patch url = ("hx-patch", url)

(** Create hx-delete attribute *)
let delete url = ("hx-delete", url)

(** Set target element *)
let target selector = ("hx-target", selector)

(** Set swap method *)
let swap method_ = ("hx-swap", method_)

(** Include values from other elements *)
let include_ selector = ("hx-include", selector)

(** Additional parameters *)
let vals json = ("hx-vals", json)

(** Disable element during request *)
let disable_during = ("hx-disabled-elt", "this")

(** Indicator element *)
let indicator selector = ("hx-indicator", selector)

(** {1 Triggers} *)

(** Custom trigger *)
let trigger event = ("hx-trigger", event)

(** Trigger on submit (default for forms) *)
let trigger_submit = ("hx-trigger", "submit")

(** Trigger with modifiers *)
let trigger_with_mods event mods =
  ("hx-trigger", Printf.sprintf "%s %s" event (String.concat " " mods))

(** Debounce trigger *)
let trigger_debounce event ms =
  ("hx-trigger", Printf.sprintf "%s delay:%dms" event ms)

(** Throttle trigger *)
let trigger_throttle event ms =
  ("hx-trigger", Printf.sprintf "%s throttle:%dms" event ms)

(** Changed trigger (only when value changes) *)
let trigger_changed event =
  ("hx-trigger", Printf.sprintf "%s changed" event)

(** {1 Validation Attributes} *)

(** HTML5 required *)
let required = ("required", "")

(** HTML5 pattern *)
let pattern regex = ("pattern", regex)

(** Min length *)
let minlength n = ("minlength", string_of_int n)

(** Max length *)
let maxlength n = ("maxlength", string_of_int n)

(** Min value *)
let min n = ("min", string_of_int n)

(** Max value *)
let max n = ("max", string_of_int n)

(** Step value *)
let step n = ("step", string_of_int n)

(** {1 Input Types} *)

type input_type =
  | Text | Password | Email | Number | Tel | Url
  | Date | Time | Datetime_local | Month | Week
  | Color | Range | File | Hidden | Search
  | Checkbox | Radio

let input_type_to_string = function
  | Text -> "text"
  | Password -> "password"
  | Email -> "email"
  | Number -> "number"
  | Tel -> "tel"
  | Url -> "url"
  | Date -> "date"
  | Time -> "time"
  | Datetime_local -> "datetime-local"
  | Month -> "month"
  | Week -> "week"
  | Color -> "color"
  | Range -> "range"
  | File -> "file"
  | Hidden -> "hidden"
  | Search -> "search"
  | Checkbox -> "checkbox"
  | Radio -> "radio"

(** {1 Field Generation} *)

(** Generate input element *)
let input ~type_ ~name ?(id : string option) ?(value : string option)
    ?(placeholder : string option) ?(class_ : string option) attrs =
  let id = match id with Some i -> i | None -> name in
  let base_attrs = [
    ("type", input_type_to_string type_);
    ("name", name);
    ("id", id);
  ] in
  let base_attrs = match value with
    | Some v -> ("value", v) :: base_attrs
    | None -> base_attrs
  in
  let base_attrs = match placeholder with
    | Some p -> ("placeholder", p) :: base_attrs
    | None -> base_attrs
  in
  let base_attrs = match class_ with
    | Some c -> ("class", c) :: base_attrs
    | None -> base_attrs
  in
  let all_attrs = base_attrs @ attrs in
  let attr_str = all_attrs
    |> List.map (fun (k, v) ->
         if v = "" then k else Printf.sprintf "%s=\"%s\"" k v)
    |> String.concat " "
  in
  Printf.sprintf "<input %s />" attr_str

(** Text input *)
let text_input ~name ?id ?value ?placeholder ?class_ attrs =
  input ~type_:Text ~name ?id ?value ?placeholder ?class_ attrs

(** Password input *)
let password_input ~name ?id ?placeholder ?class_ attrs =
  input ~type_:Password ~name ?id ?placeholder ?class_ attrs

(** Email input *)
let email_input ~name ?id ?value ?placeholder ?class_ attrs =
  input ~type_:Email ~name ?id ?value ?placeholder ?class_ attrs

(** Number input *)
let number_input ~name ?id ?value ?placeholder ?class_ attrs =
  input ~type_:Number ~name ?id ?value ?placeholder ?class_ attrs

(** Hidden input *)
let hidden_input ~name ~value =
  Printf.sprintf "<input type=\"hidden\" name=\"%s\" value=\"%s\" />" name value

(** {1 Textarea} *)

(** Generate textarea element *)
let textarea ~name ?(id : string option) ?(rows = 4) ?(cols : int option)
    ?(placeholder : string option) ?(class_ : string option) ?(content = "") attrs =
  let id = match id with Some i -> i | None -> name in
  let base_attrs = [
    ("name", name);
    ("id", id);
    ("rows", string_of_int rows);
  ] in
  let base_attrs = match cols with
    | Some c -> ("cols", string_of_int c) :: base_attrs
    | None -> base_attrs
  in
  let base_attrs = match placeholder with
    | Some p -> ("placeholder", p) :: base_attrs
    | None -> base_attrs
  in
  let base_attrs = match class_ with
    | Some c -> ("class", c) :: base_attrs
    | None -> base_attrs
  in
  let all_attrs = base_attrs @ attrs in
  let attr_str = all_attrs
    |> List.map (fun (k, v) -> Printf.sprintf "%s=\"%s\"" k v)
    |> String.concat " "
  in
  Printf.sprintf "<textarea %s>%s</textarea>" attr_str content

(** {1 Select} *)

(** Generate option element *)
let option ~value ?(selected = false) label =
  if selected then
    Printf.sprintf "<option value=\"%s\" selected>%s</option>" value label
  else
    Printf.sprintf "<option value=\"%s\">%s</option>" value label

(** Generate select element *)
let select ~name ?(id : string option) ?(class_ : string option) options attrs =
  let id = match id with Some i -> i | None -> name in
  let base_attrs = [("name", name); ("id", id)] in
  let base_attrs = match class_ with
    | Some c -> ("class", c) :: base_attrs
    | None -> base_attrs
  in
  let all_attrs = base_attrs @ attrs in
  let attr_str = all_attrs
    |> List.map (fun (k, v) -> Printf.sprintf "%s=\"%s\"" k v)
    |> String.concat " "
  in
  let options_html = String.concat "\n  " options in
  Printf.sprintf "<select %s>\n  %s\n</select>" attr_str options_html

(** {1 Submit Button} *)

(** Submit button *)
let submit ?(class_ : string option) ?(disabled = false) label =
  let attrs = ["type", "submit"] in
  let attrs = match class_ with Some c -> ("class", c) :: attrs | None -> attrs in
  let attrs = if disabled then ("disabled", "") :: attrs else attrs in
  let attr_str = attrs
    |> List.map (fun (k, v) ->
         if v = "" then k else Printf.sprintf "%s=\"%s\"" k v)
    |> String.concat " "
  in
  Printf.sprintf "<button %s>%s</button>" attr_str label

(** Submit button with loading indicator *)
let submit_with_loading ~label ~loading_label ?(class_ = "") () =
  Printf.sprintf {|<button type="submit" class="%s"
  hx-disabled-elt="this"
  hx-indicator="this">
  <span class="normal-state">%s</span>
  <span class="loading-state htmx-indicator">%s</span>
</button>|} class_ label loading_label

(** {1 Form Wrapper} *)

(** Generate form element with HTMX *)
let form ~action ?(method_ = `Post) ?(id : string option) ?(class_ : string option)
    ?(target : string option) ?(swap = "innerHTML") ?(indicator : string option)
    content =
  let hx_method = match method_ with
    | `Post -> "hx-post"
    | `Put -> "hx-put"
    | `Patch -> "hx-patch"
    | `Delete -> "hx-delete"
  in
  let attrs = [(hx_method, action); ("hx-swap", swap)] in
  let attrs = match id with Some i -> ("id", i) :: attrs | None -> attrs in
  let attrs = match class_ with Some c -> ("class", c) :: attrs | None -> attrs in
  let attrs = match target with Some t -> ("hx-target", t) :: attrs | None -> attrs in
  let attrs = match indicator with Some i -> ("hx-indicator", i) :: attrs | None -> attrs in
  let attr_str = attrs
    |> List.map (fun (k, v) -> Printf.sprintf "%s=\"%s\"" k v)
    |> String.concat " "
  in
  Printf.sprintf "<form %s>\n%s\n</form>" attr_str content

(** {1 Field Groups} *)

(** Label + input wrapper *)
let field ~label ~name ?(error : string option) input_html =
  let error_html = match error with
    | Some e -> Printf.sprintf "<span id=\"%s-error\" class=\"field-error\">%s</span>" name e
    | None -> Printf.sprintf "<span id=\"%s-error\" class=\"field-error\"></span>" name
  in
  Printf.sprintf {|<div class="field" id="%s-wrapper">
  <label for="%s">%s</label>
  %s
  %s
</div>|} name name label input_html error_html

(** Inline validation trigger *)
let validate_on_blur ~url =
  trigger_with_mods "blur" ["hx-post"; url]
