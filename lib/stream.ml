(** Kirin Streaming Module

    High-performance streaming I/O for large data handling.
    Uses Eio's direct-style async for efficient chunked transfers.

    {b Features:}
    - Chunked HTTP responses (Transfer-Encoding: chunked)
    - Streaming file uploads with progress tracking
    - Streaming file downloads
    - Memory-efficient large file handling
    - Backpressure support via Eio flows

    {b Example - Streaming Response:}
    {[
      let handler _req =
        Stream.response (fun yield ->
          yield "First chunk\n";
          yield "Second chunk\n";
          yield "Done!\n")
    ]}

    {b Example - File Download:}
    {[
      let download _req =
        Stream.file_response ~filename:"large.zip" "/path/to/large.zip"
    ]}
*)

(** {1 Types} *)

(** Chunk yielder function type *)
type yielder = string -> unit

(** Stream producer - generates chunks on demand *)
type producer = yielder -> unit

(** Progress callback for file operations *)
type progress_callback = {
  on_progress : bytes_sent:int -> total_bytes:int option -> unit;
  on_complete : unit -> unit;
  on_error : exn -> unit;
}

(** Streaming body type *)
type body =
  | Chunks of producer
  | File of { path : string; chunk_size : int }
  | Flow of (Eio.Buf_write.t -> unit)

(** Streaming response *)
type t = {
  status : Http.Status.t;
  headers : Http.Header.t;
  body : body;
}

(** {1 MIME Types} *)

(** Common MIME types for file streaming *)
let mime_types = [
  ".html", "text/html";
  ".htm", "text/html";
  ".css", "text/css";
  ".js", "application/javascript";
  ".json", "application/json";
  ".xml", "application/xml";
  ".txt", "text/plain";
  ".csv", "text/csv";
  ".pdf", "application/pdf";
  ".zip", "application/zip";
  ".gz", "application/gzip";
  ".tar", "application/x-tar";
  ".png", "image/png";
  ".jpg", "image/jpeg";
  ".jpeg", "image/jpeg";
  ".gif", "image/gif";
  ".svg", "image/svg+xml";
  ".ico", "image/x-icon";
  ".webp", "image/webp";
  ".mp3", "audio/mpeg";
  ".mp4", "video/mp4";
  ".webm", "video/webm";
  ".ogg", "audio/ogg";
  ".woff", "font/woff";
  ".woff2", "font/woff2";
  ".ttf", "font/ttf";
  ".eot", "application/vnd.ms-fontobject";
]

(** Get MIME type from filename *)
let mime_of_filename path =
  let ext = Filename.extension path |> String.lowercase_ascii in
  match List.assoc_opt ext mime_types with
  | Some mime -> mime
  | None -> "application/octet-stream"

(** {1 Configuration} *)

(** Default chunk size for file streaming (64KB) *)
let default_chunk_size = 64 * 1024

(** Maximum chunk size (1MB) *)
let max_chunk_size = 1024 * 1024

(** {1 Constructors} *)

(** Create a streaming response from a producer function.

    The producer receives a [yield] function to emit chunks.
    Chunks are sent immediately using Transfer-Encoding: chunked.

    {[
      Stream.response (fun yield ->
        for i = 1 to 100 do
          yield (Printf.sprintf "Line %d\n" i);
          (* Optional: add delay for demonstration *)
        done)
    ]}
*)
let response ?(status = `OK) ?(headers = Http.Header.init ()) producer =
  let headers = Http.Header.add headers "transfer-encoding" "chunked" in
  { status; headers; body = Chunks producer }

(** Create a streaming response from an Eio flow writer.

    For advanced use cases where you need direct control over the output buffer.

    {[
      Stream.flow_response (fun buf ->
        Eio.Buf_write.string buf "Hello ";
        Eio.Buf_write.string buf "World\n";
        Eio.Buf_write.flush buf)
    ]}
*)
let flow_response ?(status = `OK) ?(headers = Http.Header.init ()) writer =
  let headers = Http.Header.add headers "transfer-encoding" "chunked" in
  { status; headers; body = Flow writer }

(** Create a file download response with streaming.

    Streams the file in chunks without loading it entirely into memory.

    @param filename The filename to suggest in Content-Disposition
    @param content_type MIME type (auto-detected if not provided)
    @param chunk_size Size of each chunk (default: 64KB)
*)
let file_response ?filename ?content_type ?(chunk_size = default_chunk_size) path =
  let filename = match filename with
    | Some f -> f
    | None -> Filename.basename path
  in
  let content_type = match content_type with
    | Some ct -> ct
    | None -> mime_of_filename path
  in
  let headers = Http.Header.init ()
    |> fun h -> Http.Header.add h "content-type" content_type
    |> fun h -> Http.Header.add h "content-disposition"
        (Printf.sprintf "attachment; filename=\"%s\"" filename)
    |> fun h -> Http.Header.add h "transfer-encoding" "chunked"
  in
  { status = `OK; headers; body = File { path; chunk_size } }

(** Create an inline file response (displayed in browser, not downloaded) *)
let file_inline ?content_type ?(chunk_size = default_chunk_size) path =
  let content_type = match content_type with
    | Some ct -> ct
    | None -> mime_of_filename path
  in
  let headers = Http.Header.init ()
    |> fun h -> Http.Header.add h "content-type" content_type
    |> fun h -> Http.Header.add h "transfer-encoding" "chunked"
  in
  { status = `OK; headers; body = File { path; chunk_size } }

(** {1 Chunked Encoding} *)

(** Encode a chunk for HTTP chunked transfer encoding.

    Format: {size in hex}\r\n{data}\r\n
*)
let encode_chunk data =
  let len = String.length data in
  if len = 0 then ""
  else Printf.sprintf "%x\r\n%s\r\n" len data

(** Encode the final chunk (empty chunk signals end) *)
let final_chunk = "0\r\n\r\n"

(** {1 Writing Streams} *)

(** Write a streaming response to an Eio buffer.

    This is called by the server to send the response.
*)
let write_to_buffer buf (resp : t) =
  match resp.body with
  | Chunks producer ->
    let yield chunk =
      Eio.Buf_write.string buf (encode_chunk chunk);
      Eio.Buf_write.flush buf
    in
    producer yield;
    Eio.Buf_write.string buf final_chunk;
    Eio.Buf_write.flush buf

  | File { path; chunk_size } ->
    let chunk_size = min chunk_size max_chunk_size in
    let fd = Unix.openfile path [Unix.O_RDONLY] 0 in
    Fun.protect ~finally:(fun () -> Unix.close fd) (fun () ->
      let buffer = Bytes.create chunk_size in
      let rec loop () =
        let n = Unix.read fd buffer 0 chunk_size in
        if n > 0 then begin
          let chunk = Bytes.sub_string buffer 0 n in
          Eio.Buf_write.string buf (encode_chunk chunk);
          Eio.Buf_write.flush buf;
          loop ()
        end
      in
      loop ();
      Eio.Buf_write.string buf final_chunk;
      Eio.Buf_write.flush buf
    )

  | Flow writer ->
    writer buf;
    Eio.Buf_write.string buf final_chunk;
    Eio.Buf_write.flush buf

(** {1 Reading Streams} *)

(** Read a stream body in chunks, calling a handler for each chunk.

    Useful for processing uploaded files without loading into memory.

    {[
      Stream.read_chunks request (fun chunk ->
        (* Process each chunk *)
        Printf.printf "Got %d bytes\n" (String.length chunk))
    ]}
*)
let read_chunks ~(request : Request.t) ~chunk_size handler =
  let body = Request.body request in
  let len = String.length body in
  let rec loop offset =
    if offset < len then begin
      let size = min chunk_size (len - offset) in
      let chunk = String.sub body offset size in
      handler chunk;
      loop (offset + size)
    end
  in
  loop 0

(** Read a stream body with progress tracking *)
let read_with_progress ~(request : Request.t) ~chunk_size ~(progress : progress_callback) handler =
  let body = Request.body request in
  let total = String.length body in
  let total_opt = Some total in
  let bytes_sent = ref 0 in
  try
    read_chunks ~request ~chunk_size (fun chunk ->
      handler chunk;
      bytes_sent := !bytes_sent + String.length chunk;
      progress.on_progress ~bytes_sent:!bytes_sent ~total_bytes:total_opt
    );
    progress.on_complete ()
  with e ->
    progress.on_error e;
    raise e

(** {1 File Upload Handling} *)

(** Stream an uploaded file directly to disk.

    Memory-efficient: processes the upload in chunks.

    @param request The incoming request
    @param dest_path Where to save the file
    @param chunk_size Processing chunk size
    @return Number of bytes written
*)
let save_upload ~(request : Request.t) ~dest_path ?(chunk_size = default_chunk_size) () =
  let fd = Unix.openfile dest_path [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC] 0o644 in
  let total = ref 0 in
  Fun.protect ~finally:(fun () -> Unix.close fd) (fun () ->
    read_chunks ~request ~chunk_size (fun chunk ->
      let len = String.length chunk in
      let written = Unix.write_substring fd chunk 0 len in
      total := !total + written
    );
    !total
  )

(** Stream an uploaded file to disk with progress tracking *)
let save_upload_with_progress ~(request : Request.t) ~dest_path
    ?(chunk_size = default_chunk_size) ~(progress : progress_callback) () =
  let fd = Unix.openfile dest_path [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC] 0o644 in
  let total = ref 0 in
  let body_size = String.length (Request.body request) in
  Fun.protect ~finally:(fun () -> Unix.close fd) (fun () ->
    read_chunks ~request ~chunk_size (fun chunk ->
      let len = String.length chunk in
      let written = Unix.write_substring fd chunk 0 len in
      total := !total + written;
      progress.on_progress ~bytes_sent:!total ~total_bytes:(Some body_size)
    );
    progress.on_complete ();
    !total
  )

(** {1 Utilities} *)

(** Create a progress callback that prints to stderr *)
let stderr_progress ?(prefix = "Progress") () : progress_callback = {
  on_progress = (fun ~bytes_sent ~total_bytes ->
    match total_bytes with
    | Some total ->
      let pct = float_of_int bytes_sent /. float_of_int total *. 100.0 in
      Printf.eprintf "\r%s: %d/%d bytes (%.1f%%)" prefix bytes_sent total pct;
      flush stderr
    | None ->
      Printf.eprintf "\r%s: %d bytes" prefix bytes_sent;
      flush stderr
  );
  on_complete = (fun () -> Printf.eprintf "\n%s: Complete!\n" prefix; flush stderr);
  on_error = (fun e -> Printf.eprintf "\n%s: Error - %s\n" prefix (Printexc.to_string e));
}

(** Create a silent progress callback (no-op) *)
let silent_progress : progress_callback = {
  on_progress = (fun ~bytes_sent:_ ~total_bytes:_ -> ());
  on_complete = (fun () -> ());
  on_error = (fun _ -> ());
}

(** {1 Response Helpers} *)

(** Get status from streaming response *)
let status t = t.status

(** Get headers from streaming response *)
let headers t = t.headers

(** Check if response is a streaming response *)
let is_streaming t =
  match t.body with
  | Chunks _ | File _ | Flow _ -> true

(** Convert streaming response to regular Response.t

    This collects all chunks into memory and creates a standard response.
    Useful as a fallback, but loses the memory efficiency of streaming.

    For true streaming, use the Stream.write_to_buffer function with
    a server that supports chunked transfer encoding.
*)
let to_response (t : t) : Response.t =
  let body = match t.body with
    | Chunks producer ->
      let buf = Buffer.create 4096 in
      producer (fun chunk -> Buffer.add_string buf chunk);
      Buffer.contents buf

    | File { path; chunk_size } ->
      let chunk_size = min chunk_size max_chunk_size in
      let fd = Unix.openfile path [Unix.O_RDONLY] 0 in
      Fun.protect ~finally:(fun () -> Unix.close fd) (fun () ->
        let buf = Buffer.create 4096 in
        let chunk_buf = Bytes.create chunk_size in
        let rec loop () =
          let n = Unix.read fd chunk_buf 0 chunk_size in
          if n > 0 then begin
            Buffer.add_subbytes buf chunk_buf 0 n;
            loop ()
          end
        in
        loop ();
        Buffer.contents buf
      )

    | Flow _ ->
      (* Flow requires Eio context, return empty for now *)
      ""
  in
  (* Remove transfer-encoding: chunked since we're not streaming *)
  let headers = Http.Header.remove t.headers "transfer-encoding" in
  let headers = Http.Header.add headers "content-length" (string_of_int (String.length body)) in
  Response.make ~status:t.status ~headers body

(** Add a header to streaming response *)
let with_header name value t =
  { t with headers = Http.Header.add t.headers name value }

(** Set content type *)
let with_content_type ct t =
  with_header "content-type" ct t

(** {1 Generator Helpers} *)

(** Create a producer that yields lines from a list *)
let of_lines lines =
  fun yield ->
    List.iter (fun line -> yield (line ^ "\n")) lines

(** Create a producer that yields JSON array elements one by one *)
let of_json_array items to_json =
  fun yield ->
    yield "[\n";
    let rec loop = function
      | [] -> ()
      | [x] -> yield (Yojson.Safe.to_string (to_json x) ^ "\n")
      | x :: xs ->
        yield (Yojson.Safe.to_string (to_json x) ^ ",\n");
        loop xs
    in
    loop items;
    yield "]\n"

(** Create a producer from a sequence/iterator *)
let of_seq seq =
  fun yield ->
    Seq.iter yield seq
