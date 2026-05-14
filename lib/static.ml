(** Static file serving middleware *)

(** Get MIME type from extension *)
let mime_of_ext ext =
  match String.lowercase_ascii ext with
  | ".html" -> "text/html; charset=utf-8"
  | ".css" -> "text/css; charset=utf-8"
  | ".js" -> "application/javascript; charset=utf-8"
  | ".json" -> "application/json; charset=utf-8"
  | ".png" -> "image/png"
  | ".jpg" | ".jpeg" -> "image/jpeg"
  | ".gif" -> "image/gif"
  | ".svg" -> "image/svg+xml"
  | ".txt" -> "text/plain; charset=utf-8"
  | ".woff" | ".woff2" -> "font/woff2"
  | ".ttf" -> "font/ttf"
  | ".eot" -> "application/vnd.ms-fontobject"
  | ".webp" -> "image/webp"
  | _ -> "application/octet-stream"

let get_mime_type path =
  let ext = Filename.extension path in
  mime_of_ext ext

(** Check for directory traversal attacks *)
let contains_dot_dot path =
  let parts = String.split_on_char '/' path in
  List.exists (fun p -> p = "..") parts

let is_safe_path path = not (contains_dot_dot path)

(** Resolve [relative_path] under [dir] and require the result to live
    inside [dir] after symlink canonicalization.

    [contains_dot_dot] only blocks literal [..] segments in the request
    path. It does not see filesystem-level symlinks: a benign-looking
    file [dir/innocent.txt] that is itself a symlink to [/etc/passwd]
    bypasses every textual check and is happily read by [load_binary],
    which opens with default symlink-follow semantics. realpath collapses
    the entire symlink chain to a single absolute path, so requiring the
    canonical candidate to sit under the canonical [dir] catches both
    the symlink case and any residual textual escape the parser missed.

    Returns [None] on missing files (so callers fall through to the next
    handler rather than leaking existence) or escape attempts. *)
let resolve_under ~dir relative_path =
  let candidate = Filename.concat dir relative_path in
  try
    let real_candidate = Unix.realpath candidate in
    let real_dir = Unix.realpath dir in
    let sep = Filename.dir_sep in
    let prefix = real_dir ^ sep in
    if real_candidate = real_dir then Some real_candidate
    else if String.length real_candidate > String.length prefix
            && String.sub real_candidate 0 (String.length prefix) = prefix
    then Some real_candidate
    else None
  with Unix.Unix_error _ -> None

(** Static file middleware *)
let static prefix ~dir =
  fun next req ->
    let path = Request.path req in
    if String.length path >= String.length prefix &&
       String.sub path 0 (String.length prefix) = prefix then
      let relative_path =
        String.sub path (String.length prefix) (String.length path - String.length prefix)
      in

      (* Remove leading slash to prevent Filename.concat treating it as absolute *)
      let relative_path =
        if String.length relative_path > 0 && relative_path.[0] = '/' then
          String.sub relative_path 1 (String.length relative_path - 1)
        else
          relative_path
      in

      if contains_dot_dot relative_path then
        Response.make ~status:`Forbidden (`String "Forbidden")
        |> Response.with_header "content-type" "text/plain"
      else
        match resolve_under ~dir relative_path with
        | None -> next req
        | Some file_path ->
          if Fs_compat.is_directory file_path then next req
          else
            let content = Fs_compat.load_binary file_path in
            let len = String.length content in
            let ext = Filename.extension file_path in
            let mime = mime_of_ext ext in
            Response.make ~status:`OK (`String content)
            |> Response.with_header "content-type" mime
            |> Response.with_header "content-length" (string_of_int len)
    else
      next req
