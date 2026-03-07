(** JSON Path Utilities

    Extracted from Testing.Json_path. *)

(** Get value at JSON path (e.g., "user.name" or "users[0].id") *)
let get path json =
  let segments = String.split_on_char '.' path in
  let rec traverse segments json =
    match segments, json with
    | [], v -> Some v
    | seg :: rest, `Assoc fields ->
      (* Check for array index notation: field[0] *)
      if String.contains seg '[' then begin
        let bracket_pos = String.index seg '[' in
        let field = String.sub seg 0 bracket_pos in
        let idx_str = String.sub seg (bracket_pos + 1) (String.length seg - bracket_pos - 2) in
        let idx = int_of_string idx_str in
        match List.assoc_opt field fields with
        | Some (`List items) when idx < List.length items ->
          traverse rest (List.nth items idx)
        | _ -> None
      end else begin
        match List.assoc_opt seg fields with
        | Some v -> traverse rest v
        | None -> None
      end
    | seg :: rest, `List items when String.length seg > 0 && seg.[0] = '[' ->
      (* Array index: [0] *)
      let idx_str = String.sub seg 1 (String.length seg - 2) in
      let idx = int_of_string idx_str in
      if idx < List.length items then traverse rest (List.nth items idx)
      else None
    | _ -> None
  in
  traverse segments json

(** Check if path exists *)
let exists path json = Option.is_some (get path json)

(** Get string at path *)
let get_string path json =
  match get path json with
  | Some (`String s) -> Some s
  | _ -> None

(** Get int at path *)
let get_int path json =
  match get path json with
  | Some (`Int i) -> Some i
  | _ -> None

(** Get bool at path *)
let get_bool path json =
  match get path json with
  | Some (`Bool b) -> Some b
  | _ -> None

(** Get list at path *)
let get_list path json =
  match get path json with
  | Some (`List l) -> Some l
  | _ -> None
