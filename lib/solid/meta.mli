type meta_tag =
  | Title of string
  | Description of string
  | Charset of string
  | Viewport of string
  | OgTitle of string
  | OgDescription of string
  | OgImage of string
  | OgUrl of string
  | OgType of string
  | TwitterCard of string
  | TwitterTitle of string
  | TwitterDescription of string
  | TwitterImage of string
  | Canonical of string
  | Robots of string
  | Custom of string * string

val render_tag : meta_tag -> string
val render_tags : meta_tag list -> string

type builder = { mutable tags : meta_tag list }

val create : unit -> builder
val title : string -> builder -> builder
val description : string -> builder -> builder
val charset : string -> builder -> builder
val viewport : string -> builder -> builder

val og
  :  title:string
  -> ?description:string
  -> ?image:string
  -> ?url:string
  -> ?og_type:string
  -> unit
  -> builder
  -> builder

val twitter
  :  card:string
  -> title:string
  -> ?description:string
  -> ?image:string
  -> unit
  -> builder
  -> builder

val canonical : string -> builder -> builder
val robots : string -> builder -> builder
val custom : name:string -> content:string -> builder -> builder
val build : builder -> string
val defaults : ?charset_value:string -> ?viewport_value:string -> unit -> builder

val page
  :  title:string
  -> ?description:string
  -> ?canonical_url:string
  -> ?og_image:string
  -> unit
  -> builder

val tag_to_json : meta_tag -> [> `Assoc of (string * [> `String of string ]) list ]

val to_json
  :  builder
  -> [> `List of [> `Assoc of (string * [> `String of string ]) list ] list ]
