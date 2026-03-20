(** OpenAPI UI Templates

    Pure HTML template functions for Swagger UI and ReDoc.
    No dependency on OpenAPI types -- takes only [spec_url] and [title] strings. *)

(** [swagger_ui_html ~spec_url ~title] generates Swagger UI HTML page. *)
val swagger_ui_html : spec_url:string -> title:string -> string

(** [redoc_html ~spec_url ~title] generates ReDoc HTML page. *)
val redoc_html : spec_url:string -> title:string -> string
