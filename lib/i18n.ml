(** Internationalization (i18n) Support (Phase 15)

    Multi-language support with locale detection and translation loading.

    {b Quick Start:}
    {[
      (* Load translations *)
      let i18n = I18n.create ()
        |> I18n.add_translations "en" [
             ("greeting", "Hello, {{name}}!");
             ("items", "{{count}} item(s)");
           ]
        |> I18n.add_translations "ko" [
             ("greeting", "안녕하세요, {{name}}님!");
             ("items", "{{count}}개의 항목");
           ]

      (* Translate directly *)
      let msg = I18n.translate i18n ~locale:"ko" "greeting"
        ~args:[("name", "World")] ()

      (* Kirin integration *)
      let handler req =
        let accept_lang = Request.header req "Accept-Language" in
        let t = I18n.translator_for_header i18n accept_lang in
        let msg = t "greeting" ~args:[("name", "World")] () in
        Response.text msg
    ]}
*)

(** {1 Types} *)

(** Locale identifier (e.g., "en", "en-US", "ko", "ko-KR") *)
type locale = string

(** Translation key *)
type key = string

(** Translation value with optional pluralization *)
type translation =
  | Simple of string
  | Plural of {
      zero : string option;
      one : string;
      few : string option;  (* for languages like Polish *)
      many : string option; (* for languages like Russian *)
      other : string;
    }

(** Translation store *)
type translations = (key, translation) Hashtbl.t

(** Locale data *)
type locale_data = {
  translations : translations;
  fallback : locale option;
}

(** i18n instance *)
type t = {
  locales : (locale, locale_data) Hashtbl.t;
  default_locale : locale;
  mutable current_locale : locale;
}

(** Translator function type *)
type translator = key -> ?args:(string * string) list -> ?count:int -> string

(** {1 Plural Rules} *)

(** Plural category *)
type plural_category = Zero | One | Few | Many | Other

(** Get plural category for a count in a given locale *)
let plural_category locale count =
  (* Simplified CLDR plural rules *)
  let lang = String.sub locale 0 (min 2 (String.length locale)) in
  match lang with
  | "en" | "de" | "es" | "it" | "pt" | "nl" ->
    if count = 1 then One else Other
  | "fr" ->
    if count = 0 || count = 1 then One else Other
  | "ko" | "ja" | "zh" | "vi" | "th" ->
    Other  (* No plural distinction *)
  | "ru" | "uk" | "pl" | "cs" | "sk" ->
    let mod10 = count mod 10 in
    let mod100 = count mod 100 in
    if mod10 = 1 && mod100 <> 11 then One
    else if mod10 >= 2 && mod10 <= 4 && (mod100 < 10 || mod100 >= 20) then Few
    else Many
  | "ar" ->
    if count = 0 then Zero
    else if count = 1 then One
    else if count = 2 then Few  (* Arabic dual *)
    else if count mod 100 >= 3 && count mod 100 <= 10 then Few
    else if count mod 100 >= 11 then Many
    else Other
  | _ ->
    if count = 1 then One else Other

(** {1 Creation} *)

(** Create new i18n instance *)
let create ?(default_locale = "en") () =
  {
    locales = Hashtbl.create 16;
    default_locale;
    current_locale = default_locale;
  }

(** Add translations for a locale *)
let add_translations locale pairs t =
  let locale_data =
    match Hashtbl.find_opt t.locales locale with
    | Some ld -> ld
    | None -> { translations = Hashtbl.create 64; fallback = None }
  in
  List.iter (fun (key, value) ->
    Hashtbl.replace locale_data.translations key (Simple value)
  ) pairs;
  Hashtbl.replace t.locales locale locale_data;
  t

(** Add plural translations *)
let add_plural locale ~key ~one ~other ?zero ?few ?many t =
  let locale_data =
    match Hashtbl.find_opt t.locales locale with
    | Some ld -> ld
    | None -> { translations = Hashtbl.create 64; fallback = None }
  in
  Hashtbl.replace locale_data.translations key (Plural { zero; one; few; many; other });
  Hashtbl.replace t.locales locale locale_data;
  t

(** Set fallback locale *)
let set_fallback ~locale ~fallback t =
  match Hashtbl.find_opt t.locales locale with
  | Some ld ->
    Hashtbl.replace t.locales locale { ld with fallback = Some fallback };
    t
  | None ->
    Hashtbl.replace t.locales locale { translations = Hashtbl.create 64; fallback = Some fallback };
    t

(** {1 Loading from Files} *)

(** Load translations from JSON file *)
let load_json ~locale ~path t =
  let content = Fs_compat.load path in
  let json = Yojson.Safe.from_string content in
  let pairs = match json with
    | `Assoc pairs ->
      List.filter_map (fun (k, v) ->
        match v with
        | `String s -> Some (k, s)
        | _ -> None  (* Skip non-string values for now *)
      ) pairs
    | _ -> []
  in
  add_translations locale pairs t

(** Load from directory (one JSON file per locale) *)
let load_directory ~path t =
  let files = Sys.readdir path in
  Array.iter (fun filename ->
    if Filename.check_suffix filename ".json" then begin
      let locale = Filename.chop_suffix filename ".json" in
      let filepath = Filename.concat path filename in
      ignore (load_json ~locale ~path:filepath t)
    end
  ) files;
  t

(** {1 Translation} *)

(** Interpolate variables in a string *)
let interpolate template args =
  List.fold_left (fun acc (key, value) ->
    let pattern = "{{" ^ key ^ "}}" in
    let re = Str.regexp_string pattern in
    Str.global_replace re value acc
  ) template args

(** Get translation for a key in a locale *)
let get_translation t locale key =
  let rec find_in_locale loc =
    match Hashtbl.find_opt t.locales loc with
    | None -> None
    | Some ld ->
      match Hashtbl.find_opt ld.translations key with
      | Some tr -> Some tr
      | None ->
        match ld.fallback with
        | None -> None
        | Some fb -> find_in_locale fb
  in
  match find_in_locale locale with
  | Some tr -> Some tr
  | None ->
    if locale <> t.default_locale then
      find_in_locale t.default_locale
    else
      None

(** Translate a key *)
let translate t ?(locale = t.current_locale) ?(args = []) ?count key =
  match get_translation t locale key with
  | None -> key  (* Return key as fallback *)
  | Some (Simple value) -> interpolate value args
  | Some (Plural { zero; one; few; many; other }) ->
    let c = Option.value count ~default:1 in
    let template = match plural_category locale c with
      | Zero -> Option.value zero ~default:other
      | One -> one
      | Few -> Option.value few ~default:other
      | Many -> Option.value many ~default:other
      | Other -> other
    in
    let args = ("count", string_of_int c) :: args in
    interpolate template args

(** {1 Locale Detection} *)

(** Parse Accept-Language header *)
let parse_accept_language header =
  (* Parse "en-US,en;q=0.9,ko;q=0.8" format *)
  let parts = String.split_on_char ',' header in
  let parse_part part =
    let part = String.trim part in
    match String.split_on_char ';' part with
    | [] -> None
    | locale :: rest ->
      let quality = match rest with
        | [] -> 1.0
        | q :: _ ->
          let q = String.trim q in
          if String.length q > 2 && String.sub q 0 2 = "q=" then
            try float_of_string (String.sub q 2 (String.length q - 2))
            with _ -> 1.0
          else 1.0
      in
      Some (String.trim locale, quality)
  in
  let parsed = List.filter_map parse_part parts in
  let sorted = List.sort (fun (_, q1) (_, q2) -> compare q2 q1) parsed in
  List.map fst sorted

(** Detect best locale from Accept-Language header *)
let detect_locale t accept_language =
  let requested = parse_accept_language accept_language in
  let available = Hashtbl.fold (fun k _ acc -> k :: acc) t.locales [] in
  let normalize s =
    String.lowercase_ascii (String.sub s 0 (min 2 (String.length s)))
  in
  let find_match req =
    (* Try exact match first *)
    if List.mem req available then Some req
    else
      (* Try language-only match *)
      let lang = normalize req in
      List.find_opt (fun a -> normalize a = lang) available
  in
  match List.find_map find_match requested with
  | Some locale -> locale
  | None -> t.default_locale

(** {1 Request Integration} *)

(** Get translator for Accept-Language header value *)
let translator_for_header t accept_language_opt =
  let locale = match accept_language_opt with
    | None -> t.default_locale
    | Some header -> detect_locale t header
  in
  fun key ?(args = []) ?count () ->
    translate t ~locale ~args ?count key

(** Set locale from Accept-Language header *)
let set_locale_from_header t accept_language_opt =
  let locale = match accept_language_opt with
    | None -> t.default_locale
    | Some header -> detect_locale t header
  in
  t.current_locale <- locale;
  locale

(** Get current locale *)
let current_locale t = t.current_locale

(** Set current locale *)
let set_locale locale t =
  t.current_locale <- locale

(** List available locales *)
let available_locales t =
  Hashtbl.fold (fun k _ acc -> k :: acc) t.locales []

(** Check if locale exists *)
let has_locale locale t =
  Hashtbl.mem t.locales locale

(** {1 Utilities} *)

(** Format number according to locale (basic) *)
let format_number ?(locale = "en") n =
  let s = string_of_float n in
  let s = if String.get s (String.length s - 1) = '.' then
    String.sub s 0 (String.length s - 1)
  else s in
  let separator = match String.sub locale 0 (min 2 (String.length locale)) with
    | "de" | "fr" | "es" | "it" | "pt" | "ru" -> "."
    | _ -> ","
  in
  let decimal_sep = match String.sub locale 0 (min 2 (String.length locale)) with
    | "de" | "fr" | "es" | "it" | "pt" | "ru" -> ","
    | _ -> "."
  in
  (* Simple thousands separator insertion *)
  let parts = String.split_on_char '.' s in
  match parts with
  | [int_part] ->
    let len = String.length int_part in
    let buf = Buffer.create (len + len / 3) in
    String.iteri (fun i c ->
      if i > 0 && (len - i) mod 3 = 0 then Buffer.add_string buf separator;
      Buffer.add_char buf c
    ) int_part;
    Buffer.contents buf
  | [int_part; frac_part] ->
    let len = String.length int_part in
    let buf = Buffer.create (len + len / 3 + String.length frac_part + 1) in
    String.iteri (fun i c ->
      if i > 0 && (len - i) mod 3 = 0 then Buffer.add_string buf separator;
      Buffer.add_char buf c
    ) int_part;
    Buffer.add_string buf decimal_sep;
    Buffer.add_string buf frac_part;
    Buffer.contents buf
  | _ -> s

(** Format currency (basic) *)
let format_currency ?(locale = "en") ~currency amount =
  let formatted = format_number ~locale amount in
  match currency with
  | "USD" -> "$" ^ formatted
  | "EUR" -> formatted ^ " €"
  | "GBP" -> "£" ^ formatted
  | "JPY" -> "¥" ^ formatted
  | "KRW" -> "₩" ^ formatted
  | "CNY" -> "¥" ^ formatted
  | c -> formatted ^ " " ^ c

(** Format date (basic ISO format) *)
let format_date ?(locale = "en") ~year ~month ~day () =
  let lang = String.sub locale 0 (min 2 (String.length locale)) in
  match lang with
  | "en" -> Printf.sprintf "%02d/%02d/%04d" month day year
  | "ko" | "ja" | "zh" -> Printf.sprintf "%04d년 %02d월 %02d일" year month day
  | "de" | "ru" -> Printf.sprintf "%02d.%02d.%04d" day month year
  | _ -> Printf.sprintf "%04d-%02d-%02d" year month day  (* ISO default *)
