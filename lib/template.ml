(** Simple HTML Template Engine (Mustache-like syntax)

    Supports:
    - {{variable}} - Value interpolation (HTML escaped)
    - {{{variable}}} - Raw interpolation (no escaping)
    - {{#if key}}...{{/if}} - Conditional sections
    - {{#unless key}}...{{/unless}} - Inverted conditionals
    - {{#each key}}...{{/each}} - Iteration
    - {{> partial}} - Partial includes
    - {{! comment }} - Comments (ignored)
*)

(** Template context (using Yojson for flexibility) *)
type context = Yojson.Safe.t

(** Empty context *)
let empty_context = `Assoc []

(** Create context from key-value pairs *)
let context pairs =
  `Assoc (List.map (fun (k, v) -> (k, `String v)) pairs)

(** Create context with various value types *)
let context_of pairs =
  `Assoc pairs

(** Lookup value in context *)
let rec lookup ctx key =
  match ctx with
  | `Assoc pairs ->
    (* Support dot notation: "user.name" *)
    (match String.split_on_char '.' key with
     | [single] -> List.assoc_opt single pairs
     | first :: rest ->
       (match List.assoc_opt first pairs with
        | Some nested -> lookup nested (String.concat "." rest)
        | None -> None)
     | [] -> None)
  | _ -> None

(** Convert JSON value to string *)
let value_to_string = function
  | `String s -> s
  | `Int i -> string_of_int i
  | `Float f -> string_of_float f
  | `Bool b -> string_of_bool b
  | `Null -> ""
  | `List _ -> "[array]"
  | `Assoc _ -> "[object]"

(** Check if value is truthy *)
let is_truthy = function
  | `Null -> false
  | `Bool false -> false
  | `String "" -> false
  | `List [] -> false
  | `Int 0 -> false
  | _ -> true

(** HTML escape special characters *)
let html_escape s =
  let buf = Buffer.create (String.length s) in
  String.iter (function
    | '&' -> Buffer.add_string buf "&amp;"
    | '<' -> Buffer.add_string buf "&lt;"
    | '>' -> Buffer.add_string buf "&gt;"
    | '"' -> Buffer.add_string buf "&quot;"
    | '\'' -> Buffer.add_string buf "&#x27;"
    | c -> Buffer.add_char buf c
  ) s;
  Buffer.contents buf

(** Template AST *)
type node =
  | Text of string
  | Var of string * bool  (* bool = escape? *)
  | If of string * node list * node list option  (* condition, then, else *)
  | Unless of string * node list
  | Each of string * node list
  | Partial of string
  | Comment

(** Find closing tag position *)
let find_closing tag content start =
  let open_tag = "{{#" ^ tag in
  let close_tag = "{{/" ^ tag ^ "}}" in
  let close_len = String.length close_tag in
  let content_len = String.length content in

  let rec find depth pos =
    if pos >= content_len then None
    else if pos + close_len <= content_len &&
            String.sub content pos close_len = close_tag then
      if depth = 0 then Some pos
      else find (depth - 1) (pos + close_len)
    else if pos + String.length open_tag <= content_len &&
            String.sub content pos (String.length open_tag) = open_tag then
      find (depth + 1) (pos + 1)
    else
      find depth (pos + 1)
  in
  find 0 start

(** Parse template string into AST *)
let rec parse template =
  let len = String.length template in
  let nodes = ref [] in
  let pos = ref 0 in

  while !pos < len do
    (* Look for {{ *)
    match String.index_from_opt template !pos '{' with
    | None ->
      (* No more tags, rest is text *)
      nodes := Text (String.sub template !pos (len - !pos)) :: !nodes;
      pos := len
    | Some i when i + 1 < len && template.[i + 1] = '{' ->
      (* Found {{ *)
      if i > !pos then
        nodes := Text (String.sub template !pos (i - !pos)) :: !nodes;

      let tag_start = i + 2 in

      (* Check for {{{ (raw) *)
      if tag_start < len && template.[tag_start] = '{' then begin
        match String.index_from_opt template (tag_start + 1) '}' with
        | Some j when j + 2 < len && template.[j + 1] = '}' && template.[j + 2] = '}' ->
          let var_name = String.trim (String.sub template (tag_start + 1) (j - tag_start - 1)) in
          nodes := Var (var_name, false) :: !nodes;
          pos := j + 3
        | _ ->
          nodes := Text "{{" :: !nodes;
          pos := i + 2
      end
      (* Check for {{! (comment) *)
      else if tag_start < len && template.[tag_start] = '!' then begin
        match String.index_from_opt template tag_start '}' with
        | Some j when j + 1 < len && template.[j + 1] = '}' ->
          nodes := Comment :: !nodes;
          pos := j + 2
        | _ ->
          nodes := Text "{{" :: !nodes;
          pos := i + 2
      end
      (* Check for {{# (section) *)
      else if tag_start < len && template.[tag_start] = '#' then begin
        match String.index_from_opt template tag_start '}' with
        | Some j when j + 1 < len && template.[j + 1] = '}' ->
          let section = String.trim (String.sub template (tag_start + 1) (j - tag_start - 1)) in
          (* Parse section type and key *)
          let section_type, key =
            match String.split_on_char ' ' section with
            | ["if"; k] -> (`If, k)
            | ["unless"; k] -> (`Unless, k)
            | ["each"; k] -> (`Each, k)
            | [k] -> (`Section, k)  (* Plain section = if *)
            | _ -> (`Section, section)
          in
          let tag_name = match section_type with
            | `If -> "if " ^ key
            | `Unless -> "unless " ^ key
            | `Each -> "each " ^ key
            | `Section -> key
          in
          let content_start = j + 2 in
          (match find_closing tag_name template content_start with
           | Some close_pos ->
             let inner = String.sub template content_start (close_pos - content_start) in
             let close_tag_len = 5 + String.length tag_name in
             (match section_type with
              | `If | `Section ->
                (* Check for {{else}} - find the actual tag *)
                let else_tag = "{{else}}" in
                let else_len = String.length else_tag in
                let rec find_else i =
                  if i + else_len > String.length inner then None
                  else if String.sub inner i else_len = else_tag then Some i
                  else find_else (i + 1)
                in
                (match find_else 0 with
                 | Some ep ->
                   let then_part = String.sub inner 0 ep in
                   let else_part = String.sub inner (ep + else_len) (String.length inner - ep - else_len) in
                   nodes := If (key, parse then_part, Some (parse else_part)) :: !nodes
                 | None ->
                   nodes := If (key, parse inner, None) :: !nodes)
              | `Unless ->
                nodes := Unless (key, parse inner) :: !nodes
              | `Each ->
                nodes := Each (key, parse inner) :: !nodes);
             pos := close_pos + close_tag_len
           | None ->
             nodes := Text ("{{#" ^ section ^ "}}") :: !nodes;
             pos := j + 2)
        | _ ->
          nodes := Text "{{" :: !nodes;
          pos := i + 2
      end
      (* Check for {{> (partial) *)
      else if tag_start < len && template.[tag_start] = '>' then begin
        match String.index_from_opt template tag_start '}' with
        | Some j when j + 1 < len && template.[j + 1] = '}' ->
          let partial_name = String.trim (String.sub template (tag_start + 1) (j - tag_start - 1)) in
          nodes := Partial partial_name :: !nodes;
          pos := j + 2
        | _ ->
          nodes := Text "{{" :: !nodes;
          pos := i + 2
      end
      (* Regular variable *)
      else begin
        match String.index_from_opt template tag_start '}' with
        | Some j when j + 1 < len && template.[j + 1] = '}' ->
          let var_name = String.trim (String.sub template tag_start (j - tag_start)) in
          nodes := Var (var_name, true) :: !nodes;
          pos := j + 2
        | _ ->
          nodes := Text "{{" :: !nodes;
          pos := i + 2
      end
    | Some i ->
      (* Single { is just text *)
      nodes := Text (String.sub template !pos (i - !pos + 1)) :: !nodes;
      pos := i + 1
  done;

  List.rev !nodes

(** Partial resolver type *)
type partials = string -> string option

(** No partials *)
let no_partials _ = None

(** Render AST to string *)
let rec render_nodes ?(partials = no_partials) ctx nodes =
  let buf = Buffer.create 256 in
  List.iter (fun node ->
    Buffer.add_string buf (render_node ~partials ctx node)
  ) nodes;
  Buffer.contents buf

and render_node ?(partials = no_partials) ctx = function
  | Text s -> s
  | Comment -> ""
  | Var (key, escape) ->
    let value = match lookup ctx key with
      | Some v -> value_to_string v
      | None -> ""
    in
    if escape then html_escape value else value
  | If (key, then_nodes, else_nodes) ->
    let value = lookup ctx key in
    if Option.map is_truthy value = Some true then
      render_nodes ~partials ctx then_nodes
    else
      (match else_nodes with
       | Some nodes -> render_nodes ~partials ctx nodes
       | None -> "")
  | Unless (key, nodes) ->
    let value = lookup ctx key in
    if Option.map is_truthy value <> Some true then
      render_nodes ~partials ctx nodes
    else
      ""
  | Each (key, nodes) ->
    (match lookup ctx key with
     | Some (`List items) ->
       let results = List.mapi (fun i item ->
         (* Add @index, @first, @last to context *)
         let item_ctx = match item with
           | `Assoc pairs ->
             `Assoc (("@index", `Int i) ::
                     ("@first", `Bool (i = 0)) ::
                     ("@last", `Bool (i = List.length items - 1)) ::
                     pairs)
           | _ ->
             `Assoc [("this", item);
                     ("@index", `Int i);
                     ("@first", `Bool (i = 0));
                     ("@last", `Bool (i = List.length items - 1))]
         in
         render_nodes ~partials item_ctx nodes
       ) items in
       String.concat "" results
     | _ -> "")
  | Partial name ->
    (match partials name with
     | Some partial_template -> render ~partials ctx partial_template
     | None -> "")

(** Main render function *)
and render ?(partials = no_partials) ctx template =
  let nodes = parse template in
  render_nodes ~partials ctx nodes

(** Render template to response *)
let html ?(partials = no_partials) ctx template =
  let body = render ~partials ctx template in
  Response.html body

(** Load template from string *)
let from_string s = s

(** Simple string interpolation (no parsing, just replace) *)
let interpolate template pairs =
  List.fold_left (fun acc (key, value) ->
    let pattern = "{{" ^ key ^ "}}" in
    let rec replace s =
      match String.index_from_opt s 0 '{' with
      | None -> s
      | Some i ->
        let plen = String.length pattern in
        if i + plen <= String.length s &&
           String.sub s i plen = pattern then
          let before = String.sub s 0 i in
          let after = String.sub s (i + plen) (String.length s - i - plen) in
          replace (before ^ value ^ after)
        else
          let before = String.sub s 0 (i + 1) in
          let after = String.sub s (i + 1) (String.length s - i - 1) in
          before ^ replace after
    in
    replace acc
  ) template pairs
