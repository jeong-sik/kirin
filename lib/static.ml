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
  | _ -> "application/octet-stream"

let get_mime_type path =
  let ext = Filename.extension path in
  mime_of_ext ext

(** Check for directory traversal attacks *)
let contains_dot_dot path =
  let parts = String.split_on_char '/' path in
  List.exists (fun p -> p = "..") parts

let is_safe_path path = not (contains_dot_dot path)

(** Static file middleware *)
let static prefix ~dir =
  fun next req ->
    let path = Request.path req in
    if String.length path >= String.length prefix &&
       String.sub path 0 (String.length prefix) = prefix then
      let relative_path = 
        String.sub path (String.length prefix) (String.length path - String.length prefix)
      in
      
      if contains_dot_dot relative_path then
        Response.make ~status:`Forbidden (`String "Forbidden")
        |> Response.with_header "content-type" "text/plain"
      else
        let file_path = Filename.concat dir relative_path in
        if Fs_compat.file_exists file_path && not (Fs_compat.is_directory file_path) then
          let content = Fs_compat.load_binary file_path in
          let len = String.length content in

          let ext = Filename.extension file_path in
          let mime = mime_of_ext ext in
          
          Response.make ~status:`OK (`String content)
          |> Response.with_header "content-type" mime
          |> Response.with_header "content-length" (string_of_int len)
        else
          next req
    else
      next req