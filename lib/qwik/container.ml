(** Qwik Container State

    Serialized application state for resumability.

    The container holds all serialized state needed to resume the application
    on the client without re-executing initialization code. *)

(** {1 Container Types} *)

(** Object reference (for deduplication) *)
type obj_ref = int

(** Serialized object *)
type sobj =
  | SNull
  | SBool of bool
  | SInt of int
  | SFloat of float
  | SString of string
  | SArray of sobj list
  | SObject of (string * sobj) list
  | SRef of obj_ref                    (* Reference to another object *)
  | SQrl of string                     (* QRL reference *)
  | SSignal of obj_ref                 (* Signal reference *)
  | SResource of obj_ref * string      (* Resource with state *)
  | SStore of obj_ref                  (* Store proxy reference *)

(** Container state *)
type t = {
  mutable objects: (obj_ref * sobj) list;  (* Serialized objects *)
  mutable qrls: (string * Qrl.t) list;     (* QRL definitions *)
  mutable next_ref: obj_ref;
  mutable paused: bool;                     (* Is container paused? *)
}

(** {1 Container Operations} *)

(** Create container *)
let create () = {
  objects = [];
  qrls = [];
  next_ref = 0;
  paused = false;
}

(** Allocate object reference *)
let alloc_ref container =
  let ref_ = container.next_ref in
  container.next_ref <- ref_ + 1;
  ref_

(** Add object to container *)
let add_object container sobj =
  let ref_ = alloc_ref container in
  container.objects <- (ref_, sobj) :: container.objects;
  ref_

(** Get object by reference *)
let get_object container ref_ =
  List.assoc_opt ref_ container.objects

(** Add QRL to container *)
let add_qrl container name qrl =
  container.qrls <- (name, qrl) :: container.qrls

(** Get QRL by name *)
let get_qrl container name =
  List.assoc_opt name container.qrls

(** {1 Pause/Resume} *)

(** Pause container (for SSR) *)
let pause container =
  container.paused <- true

(** Is container paused? *)
let is_paused container = container.paused

(** Resume container *)
let resume container =
  container.paused <- false

(** {1 Serialization} *)

(** Serialize sobj to JSON *)
let rec sobj_to_json = function
  | SNull -> `Null
  | SBool b -> `Bool b
  | SInt i -> `Int i
  | SFloat f -> `Float f
  | SString s -> `String s
  | SArray items -> `List (List.map sobj_to_json items)
  | SObject props -> `Assoc (List.map (fun (k, v) -> (k, sobj_to_json v)) props)
  | SRef r -> `Assoc [("$ref$", `Int r)]
  | SQrl q -> `Assoc [("$qrl$", `String q)]
  | SSignal r -> `Assoc [("$signal$", `Int r)]
  | SResource (r, state) -> `Assoc [("$resource$", `Int r); ("state", `String state)]
  | SStore r -> `Assoc [("$store$", `Int r)]

(** Parse sobj from JSON *)
let rec sobj_of_json (json : Yojson.Safe.t) =
  let open Yojson.Safe.Util in
  match json with
  | `Null -> SNull
  | `Bool b -> SBool b
  | `Int i -> SInt i
  | `Intlit s -> SInt (int_of_string s)  (* Handle Intlit from Yojson.Safe *)
  | `Float f -> SFloat f
  | `String s -> SString s
  | `List items -> SArray (List.map sobj_of_json items)
  | `Assoc props ->
    (* Check for special markers *)
    (match List.assoc_opt "$ref$" props with
    | Some (`Int r) -> SRef r
    | _ ->
    match List.assoc_opt "$qrl$" props with
    | Some (`String q) -> SQrl q
    | _ ->
    match List.assoc_opt "$signal$" props with
    | Some (`Int r) -> SSignal r
    | _ ->
    match List.assoc_opt "$resource$" props with
    | Some (`Int r) ->
      let state = try props |> List.assoc "$state$" |> to_string with _ -> "pending" in
      SResource (r, state)
    | _ ->
    match List.assoc_opt "$store$" props with
    | Some (`Int r) -> SStore r
    | _ ->
      SObject (List.map (fun (k, v) -> (k, sobj_of_json v)) props))

(** Container to JSON *)
let to_json container =
  let objects_json = container.objects
    |> List.map (fun (ref_, obj) -> (string_of_int ref_, sobj_to_json obj))
  in
  let qrls_json = container.qrls
    |> List.map (fun (name, qrl) -> (name, Qrl.to_json qrl))
  in
  `Assoc [
    ("objs", `Assoc objects_json);
    ("qrls", `Assoc qrls_json);
    ("ctx", `Assoc [
      ("paused", `Bool container.paused);
      ("refs", `Int container.next_ref);
    ]);
  ]

(** Container from JSON *)
let of_json json =
  let open Yojson.Safe.Util in
  let objs = json |> member "objs" |> to_assoc in
  let qrls = json |> member "qrls" |> to_assoc in
  let ctx = json |> member "ctx" in
  {
    objects = List.map (fun (k, v) -> (int_of_string k, sobj_of_json v)) objs;
    qrls = List.map (fun (k, v) -> (k, Qrl.of_json v)) qrls;
    next_ref = ctx |> member "refs" |> to_int;
    paused = ctx |> member "paused" |> to_bool;
  }

(** {1 HTML Serialization} *)

(** Serialize container for HTML embedding *)
let serialize container =
  let json = to_json container in
  let json_str = Yojson.Safe.to_string json in
  (* Escape for safe HTML embedding *)
  json_str
  |> Str.global_replace (Str.regexp "<") "\\u003c"
  |> Str.global_replace (Str.regexp ">") "\\u003e"
  |> Str.global_replace (Str.regexp "&") "\\u0026"

(** Generate container script tag *)
let script_tag container =
  let data = serialize container in
  Printf.sprintf {|<script type="qwik/json">%s</script>|} data

(** Generate prefetch script *)
let prefetch_script container =
  let qrl_chunks = List.map (fun (_, qrl) -> qrl.Qrl.chunk) container.qrls in
  let unique_chunks = List.sort_uniq String.compare qrl_chunks in
  let links = List.map (fun chunk ->
    Printf.sprintf {|<link rel="modulepreload" href="/%s" />|} chunk
  ) unique_chunks in
  String.concat "\n" links

(** {1 Context API} *)

(** Context ID *)
type ctx_id = string

(** Context value *)
type ctx_value = Yojson.Safe.t

(** Context store *)
type ctx_store = {
  mutable contexts: (ctx_id * ctx_value) list;
}

(** Create context store *)
let create_ctx_store () = {
  contexts = [];
}

(** Set context value *)
let set_ctx store id value =
  store.contexts <- (id, value) :: List.filter (fun (k, _) -> k <> id) store.contexts

(** Get context value *)
let get_ctx store id =
  List.assoc_opt id store.contexts

(** Context store to JSON *)
let ctx_store_to_json store =
  `Assoc store.contexts
