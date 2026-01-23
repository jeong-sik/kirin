(** Static file serving middleware *)

(** MIME type mapping based on file extension *)
let mime_types = [
  (* Text *)
  (".html", "text/html; charset=utf-8");
  (".htm", "text/html; charset=utf-8");
  (".css", "text/css; charset=utf-8");
  (".js", "application/javascript; charset=utf-8");
  (".mjs", "application/javascript; charset=utf-8");
  (".json", "application/json; charset=utf-8");
  (".xml", "application/xml; charset=utf-8");
  (".txt", "text/plain; charset=utf-8");
  (".md", "text/markdown; charset=utf-8");

  (* Images *)
  (".png", "image/png");
  (".jpg", "image/jpeg");
  (".jpeg", "image/jpeg");
  (".gif", "image/gif");
  (".svg", "image/svg+xml");
  (".ico", "image/x-icon");
  (".webp", "image/webp");
  (".avif", "image/avif");

  (* Fonts *)
  (".woff", "font/woff");
  (".woff2", "font/woff2");
  (".ttf", "font/ttf");
  (".otf", "font/otf");
  (".eot", "application/vnd.ms-fontobject");

  (* Media *)
  (".mp3", "audio/mpeg");
  (".mp4", "video/mp4");
  (".webm", "video/webm");
  (".ogg", "audio/ogg");
  (".wav", "audio/wav");

  (* Documents *)
  (".pdf", "application/pdf");
  (".zip", "application/zip");
  (".gz", "application/gzip");
  (".tar", "application/x-tar");

  (* WebAssembly *)
  (".wasm", "application/wasm");
]

(** Get MIME type for a file path *)
let get_mime_type path =
  let ext = Filename.extension path |> String.lowercase_ascii in
  match List.assoc_opt ext mime_types with
  | Some mime -> mime
  | None -> "application/octet-stream"

(** Check if path is safe (no directory traversal) *)
let is_safe_path path =
  (* Split and check each component *)
  let parts = String.split_on_char '/' path in
  not (List.exists (fun p -> p = ".." || p = "." && String.length p > 1) parts)

(** Read file contents safely *)
let read_file path =
  try
    if not (Sys.file_exists path) then
      None
    else if Sys.is_directory path then
      (* Try index.html for directories *)
      let index_path = Filename.concat path "index.html" in
      if Sys.file_exists index_path then
        Some (In_channel.with_open_bin index_path In_channel.input_all, "text/html; charset=utf-8")
      else
        None
    else
      let content = In_channel.with_open_bin path In_channel.input_all in
      Some (content, get_mime_type path)
  with
  | _ -> None

(** Create a static file serving middleware

    Usage:
    {[
      Kirin.start ~port:3000
      @@ Kirin.logger
      @@ Kirin.static "/public" ~dir:"./static"
      @@ routes
    ]}

    This will serve files from ./static/ directory when the request path
    starts with /public/. For example:
    - GET /public/style.css -> ./static/style.css
    - GET /public/js/app.js -> ./static/js/app.js
*)
let serve ~prefix ~dir next_handler req =
  let path = Request.path req in
  let meth = Request.meth req in

  (* Only handle GET and HEAD for static files *)
  if meth <> `GET && meth <> `HEAD then
    next_handler req
  else
    (* Check if request path starts with prefix *)
    let prefix_len = String.length prefix in
    if String.length path >= prefix_len && String.sub path 0 prefix_len = prefix then
      (* Extract the file path after prefix *)
      let file_path =
        if String.length path = prefix_len then
          ""
        else
          String.sub path prefix_len (String.length path - prefix_len)
      in
      (* Remove leading slash if present *)
      let file_path =
        if String.length file_path > 0 && file_path.[0] = '/' then
          String.sub file_path 1 (String.length file_path - 1)
        else
          file_path
      in
      (* Security: prevent directory traversal *)
      if not (is_safe_path file_path) then
        Response.make ~status:`Forbidden "Forbidden"
        |> Response.with_header "content-type" "text/plain"
      else
        let full_path = Filename.concat dir file_path in
        match read_file full_path with
        | Some (content, mime) ->
          let resp = Response.make ~status:`OK content in
          let resp = Response.with_header "content-type" mime resp in
          (* Add cache headers for static files *)
          let resp = Response.with_header "cache-control" "public, max-age=3600" resp in
          if meth = `HEAD then
            Response.make ~status:`OK ""
            |> Response.with_header "content-type" mime
            |> Response.with_header "content-length" (string_of_int (String.length content))
          else
            resp
        | None ->
          (* File not found, pass to next handler *)
          next_handler req
    else
      (* Path doesn't match prefix, pass to next handler *)
      next_handler req

(** Convenient middleware constructor *)
let static prefix ~dir =
  fun next_handler -> fun req -> serve ~prefix ~dir next_handler req
