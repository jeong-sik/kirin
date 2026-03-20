(** Internationalization (i18n) Support

    Multi-language support with locale detection and translation loading.

    {b Features:}
    - Mustache-style variable interpolation ([\{\{name\}\}])
    - Plural rules (CLDR-based)
    - Accept-Language header parsing
    - JSON translation file loading *)

(** {1 Types} *)

(** Locale identifier (e.g., "en", "en-US", "ko", "ko-KR"). *)
type locale = string

(** Translation key. *)
type key = string

(** Translation value with optional pluralization. *)
type translation =
  | Simple of string
  | Plural of {
      zero : string option;
      one : string;
      few : string option;
      many : string option;
      other : string;
    }

(** i18n instance. Abstract to hide internal state. *)
type t

(** Translator function type. *)
type translator = key -> ?args:(string * string) list -> ?count:int -> string

(** {1 Plural Rules} *)

(** Plural category. *)
type plural_category = Zero | One | Few | Many | Other

(** [plural_category locale count] returns the plural category for a count
    in a given locale (simplified CLDR rules). *)
val plural_category : locale -> int -> plural_category

(** {1 Creation} *)

(** [create ?default_locale ()] creates a new i18n instance.
    @param default_locale Default locale (default: "en") *)
val create : ?default_locale:string -> unit -> t

(** [add_translations locale pairs t] adds string translations for a locale.
    Returns [t] for chaining. *)
val add_translations : locale -> (key * string) list -> t -> t

(** [add_plural locale ~key ~one ~other ?zero ?few ?many t] adds a plural translation.
    Returns [t] for chaining. *)
val add_plural :
  locale ->
  key:key ->
  one:string ->
  other:string ->
  ?zero:string ->
  ?few:string ->
  ?many:string ->
  t -> t

(** [set_fallback ~locale ~fallback t] sets a fallback locale.
    Returns [t] for chaining. *)
val set_fallback : locale:locale -> fallback:locale -> t -> t

(** {1 Loading from Files} *)

(** [load_json ~locale ~path t] loads translations from a JSON file.
    Returns [t] for chaining. *)
val load_json : locale:locale -> path:string -> t -> t

(** [load_directory ~path t] loads all JSON files from a directory
    (filename without .json extension is used as locale).
    Returns [t] for chaining. *)
val load_directory : path:string -> t -> t

(** {1 Translation} *)

(** [translate t ?locale ?args ?count key] translates a key.
    Returns the key itself if no translation is found. *)
val translate :
  t ->
  ?locale:locale ->
  ?args:(string * string) list ->
  ?count:int ->
  key ->
  string

(** {1 Locale Detection} *)

(** [parse_accept_language header] parses an Accept-Language header string.
    Returns a list of locales sorted by quality. *)
val parse_accept_language : string -> locale list

(** [detect_locale t accept_language] detects the best locale from
    an Accept-Language header value. *)
val detect_locale : t -> string -> locale

(** {1 Request Integration} *)

(** [translator_for_header t accept_language_opt] returns a translator function
    bound to the detected locale. *)
val translator_for_header :
  t ->
  string option ->
  (key -> ?args:(string * string) list -> ?count:int -> unit -> string)

(** [set_locale_from_header t accept_language_opt] sets the current locale
    from an Accept-Language header. Returns the detected locale. *)
val set_locale_from_header : t -> string option -> locale

(** [current_locale t] returns the current locale. *)
val current_locale : t -> locale

(** [set_locale locale t] sets the current locale. *)
val set_locale : locale -> t -> unit

(** [available_locales t] lists all available locales. *)
val available_locales : t -> locale list

(** [has_locale locale t] checks if a locale exists. *)
val has_locale : locale -> t -> bool

(** {1 Formatting Utilities} *)

(** [format_number ?locale n] formats a number with locale-appropriate separators. *)
val format_number : ?locale:string -> float -> string

(** [format_currency ?locale ~currency amount] formats a currency value. *)
val format_currency : ?locale:string -> currency:string -> float -> string

(** [format_date ?locale ~year ~month ~day ()] formats a date according to locale. *)
val format_date : ?locale:string -> year:int -> month:int -> day:int -> unit -> string
