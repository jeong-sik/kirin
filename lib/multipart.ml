(** Multipart form-data parsing for file uploads *)

(** A single part of multipart data *)
type part = {
  name : string;                    (* Form field name *)
  filename : string option;         (* Original filename (for files) *)
  content_type : string option;     (* MIME type (for files) *)
  content : string;                 (* Raw content *)
}

(** Multipart form data *)
type t = {
  parts : part list;
}

(* RFC 2046 §5.1.1 — boundary is 1..70 characters from a restricted
   character class.  Beyond the spec, three concrete misparses fall
   out of accepting anything:

   - empty boundary  → delimiter is just "--", which appears all
     over real bodies and turns the parser into garbage
   - whitespace-only / CR-LF in boundary → splitter aligns on bytes
     that occur naturally in part headers, again producing garbage
   - 70+ char boundary → spec-violating sender; tolerating it
     makes denial-of-service easier (the value still has to be
     searched linearly against the whole body)

   Reject these at [extract_boundary] so an attacker-shaped header
   never reaches [parse].  Returning [None] is the existing failure
   shape: [from_request] already maps it to a no-multipart request. *)
let is_valid_boundary_char c =
  (c >= 'a' && c <= 'z')
  || (c >= 'A' && c <= 'Z')
  || (c >= '0' && c <= '9')
  || c = '\'' || c = '(' || c = ')' || c = '+' || c = '_'
  || c = ',' || c = '-' || c = '.' || c = '/' || c = ':'
  || c = '=' || c = '?' || c = ' '

let boundary_is_rfc2046_valid s =
  let n = String.length s in
  if n < 1 || n > 70 then false
  else begin
    let ok = ref true in
    String.iter (fun c -> if not (is_valid_boundary_char c) then ok := false) s;
    !ok
  end

(* Strip a single layer of double-quotes if present (some senders
   wrap the boundary as [boundary="..."]).  Done before the
   character-class check so [boundary="abc"] passes. *)
let unquote s =
  let n = String.length s in
  if n >= 2 && s.[0] = '"' && s.[n - 1] = '"'
  then String.sub s 1 (n - 2)
  else s

(** Extract boundary from Content-Type header.
    Returns [None] when the boundary parameter is absent, empty,
    longer than RFC 2046's 70-char limit, or contains bytes outside
    the spec's allowed class. *)
let extract_boundary content_type =
  (* Content-Type: multipart/form-data; boundary=----WebKitFormBoundary7MA4YWxkTrZu0gW *)
  let parts = String.split_on_char ';' content_type in
  List.find_map (fun part ->
    let part = String.trim part in
    if String.length part > 9 && String.sub part 0 9 = "boundary=" then
      let raw = String.sub part 9 (String.length part - 9) in
      let b = unquote raw in
      if boundary_is_rfc2046_valid b then Some b else None
    else
      None
  ) parts

(** Parse Content-Disposition header *)
let parse_content_disposition header =
  (* form-data; name="field1"; filename="file.txt" *)
  let parts = String.split_on_char ';' header in
  let name = ref None in
  let filename = ref None in
  List.iter (fun part ->
    let part = String.trim part in
    if String.length part > 5 && String.sub part 0 5 = "name=" then begin
      let value = String.sub part 5 (String.length part - 5) in
      (* Remove quotes *)
      let value =
        if String.length value >= 2 && value.[0] = '"' && value.[String.length value - 1] = '"'
        then String.sub value 1 (String.length value - 2)
        else value
      in
      name := Some value
    end
    else if String.length part > 9 && String.sub part 0 9 = "filename=" then begin
      let value = String.sub part 9 (String.length part - 9) in
      (* Remove quotes *)
      let value =
        if String.length value >= 2 && value.[0] = '"' && value.[String.length value - 1] = '"'
        then String.sub value 1 (String.length value - 2)
        else value
      in
      filename := Some value
    end
  ) parts;
  (!name, !filename)

(** Find substring position *)
let find_substring haystack needle start =
  let needle_len = String.length needle in
  let haystack_len = String.length haystack in
  if needle_len = 0 then Some start
  else if needle_len > haystack_len - start then None
  else begin
    let rec search i =
      if i > haystack_len - needle_len then None
      else if String.sub haystack i needle_len = needle then Some i
      else search (i + 1)
    in
    search start
  end

(** Parse headers from part content *)
let parse_headers content =
  (* Headers are separated from body by \r\n\r\n *)
  match find_substring content "\r\n\r\n" 0 with
  | None -> ([], content)
  | Some idx ->
    let header_str = String.sub content 0 idx in
    let body = String.sub content (idx + 4) (String.length content - idx - 4) in
    let header_lines = String.split_on_char '\n' header_str in
    let headers = List.filter_map (fun line ->
      let line = String.trim line in
      match String.index_opt line ':' with
      | Some idx ->
        let name = String.lowercase_ascii (String.sub line 0 idx |> String.trim) in
        let value = String.sub line (idx + 1) (String.length line - idx - 1) |> String.trim in
        Some (name, value)
      | None -> None
    ) header_lines in
    (headers, body)

(** Parse a single part *)
let parse_part content =
  let (headers, body) = parse_headers content in
  let content_disposition =
    List.assoc_opt "content-disposition" headers
    |> Option.value ~default:""
  in
  let content_type = List.assoc_opt "content-type" headers in
  let (name_opt, filename) = parse_content_disposition content_disposition in
  match name_opt with
  | Some name -> Some { name; filename; content_type; content = body }
  | None -> None

(** Default cap on the number of parts the splitter will accumulate.
    A normal form has a handful; anything past the cap is almost
    certainly a denial-of-service shape (an attacker driving the
    parser into [O(N * body)] [String.sub] allocations by repeating
    boundaries).  Real forms never approach this. *)
let default_max_parts = 1000

(** Parse multipart form data.

    [?max_parts] caps the number of part fragments the splitter
    keeps.  Once the cap is hit, the entire result is discarded
    and an empty [{parts = []}] is returned — *not* a truncated
    partial parse — because a partial result is attacker-shaped
    and may be more dangerous than no result at all. *)
let parse ?(max_parts = default_max_parts) ~boundary body =
  let delimiter = "--" ^ boundary in
  let _end_delimiter = delimiter ^ "--" in

  let overflow = ref false in
  let take_or_overflow acc part_content =
    if List.length acc >= max_parts then begin
      overflow := true;
      acc
    end else
      part_content :: acc
  in

  (* Split by delimiter *)
  let rec split_parts acc start =
    if !overflow then acc
    else
      match find_substring body delimiter start with
      | None -> List.rev acc
      | Some idx ->
        if idx > start then begin
          (* Content before this delimiter *)
          let part_content = String.sub body start (idx - start) in
          (* Remove trailing \r\n *)
          let part_content =
            let len = String.length part_content in
            if len >= 2 && String.sub part_content (len - 2) 2 = "\r\n"
            then String.sub part_content 0 (len - 2)
            else part_content
          in
          if String.length part_content > 0 then
            split_parts (take_or_overflow acc part_content) (idx + String.length delimiter)
          else
            split_parts acc (idx + String.length delimiter)
        end else begin
          (* Skip to after delimiter *)
          let after_delim = idx + String.length delimiter in
          (* Check if this is end delimiter *)
          if after_delim + 2 <= String.length body &&
             String.sub body after_delim 2 = "--"
          then List.rev acc
          else begin
            (* Skip \r\n after delimiter *)
            let next_start =
              if after_delim + 2 <= String.length body &&
                 String.sub body after_delim 2 = "\r\n"
              then after_delim + 2
              else after_delim
            in
            split_parts acc next_start
          end
        end
  in

  let raw_parts = split_parts [] 0 in
  if !overflow then
    (* Refuse to expose a partial parse — log nothing here (the
       caller's middleware decides on telemetry) and return the
       empty multipart so the request looks like one with no
       parts. *)
    { parts = [] }
  else
    let parts = List.filter_map parse_part raw_parts in
    { parts }

(** Parse multipart from request *)
let from_request req =
  match Request.header "content-type" req with
  | Some ct when String.length ct > 30 &&
                 String.lowercase_ascii (String.sub ct 0 19) = "multipart/form-data" ->
    (match extract_boundary ct with
    | Some boundary ->
      let body = Request.body req in
      Some (parse ~boundary body)
    | None -> None)
  | _ -> None

(** Get all parts *)
let parts t = t.parts

(** Get a field value by name *)
let field name t =
  List.find_map (fun part ->
    if part.name = name && part.filename = None
    then Some part.content
    else None
  ) t.parts

(** Get a file by name *)
let file name t =
  List.find_map (fun part ->
    if part.name = name && part.filename <> None
    then Some part
    else None
  ) t.parts

(** Get all files *)
let files t =
  List.filter (fun part -> part.filename <> None) t.parts

(** Get all fields (non-file parts) *)
let fields t =
  List.filter_map (fun part ->
    if part.filename = None
    then Some (part.name, part.content)
    else None
  ) t.parts
