(** Kirin Streaming Module

    High-performance streaming I/O for large data handling.
    Uses Eio's direct-style async for efficient chunked transfers.
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

(** Create a streaming response from a producer function. *)
let response ?(status = `OK) ?(headers = Http.Header.init ()) producer = 
  let headers = Http.Header.add headers "transfer-encoding" "chunked" in 
  let stream_producer stream = 
    let yield chunk = Eio.Stream.add stream chunk in 
    producer yield;
    Eio.Stream.add stream "" (* End marker *)
  in 
  Response.make ~status ~headers (`Producer stream_producer)

(** Create a streaming response from an Eio flow writer. *)
let flow_response ?(status = `OK) ?(headers = Http.Header.init ()) _writer =
  let headers = Http.Header.add headers "transfer-encoding" "chunked" in  (* Flow response not fully supported with new Producer type yet, fallback to String *)
  Response.make ~status ~headers (`String "") 

(** Create a file download response with streaming. *)
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
  
  let stream_producer stream = 
    let chunk_size = min chunk_size max_chunk_size in 
    let fd = Unix.openfile path [Unix.O_RDONLY] 0 in 
    Fun.protect ~finally:(fun () -> Unix.close fd) (fun () -> 
      let buffer = Bytes.create chunk_size in 
      let rec loop () = 
        let n = Unix.read fd buffer 0 chunk_size in 
        if n > 0 then begin 
          let chunk = Bytes.sub_string buffer 0 n in 
          Eio.Stream.add stream chunk;
          loop ()
        end 
      in 
      loop ();
      Eio.Stream.add stream "" (* End marker *)
    ) 
  in 
  
  Response.make ~status:`OK ~headers (`Producer stream_producer)

(** Create an inline file response *)
let file_inline ?content_type ?(chunk_size = default_chunk_size) path = 
  let content_type = match content_type with 
    | Some ct -> ct 
    | None -> mime_of_filename path 
  in 
  let headers = Http.Header.init () 
    |> fun h -> Http.Header.add h "content-type" content_type 
    |> fun h -> Http.Header.add h "transfer-encoding" "chunked" 
  in 
  
  let stream_producer stream = 
    let chunk_size = min chunk_size max_chunk_size in 
    let fd = Unix.openfile path [Unix.O_RDONLY] 0 in 
    Fun.protect ~finally:(fun () -> Unix.close fd) (fun () -> 
      let buffer = Bytes.create chunk_size in 
      let rec loop () = 
        let n = Unix.read fd buffer 0 chunk_size in 
        if n > 0 then begin 
          let chunk = Bytes.sub_string buffer 0 n in 
          Eio.Stream.add stream chunk;
          loop ()
        end 
      in 
      loop ();
      Eio.Stream.add stream "" (* End marker *)
    ) 
  in 
  
  Response.make ~status:`OK ~headers (`Producer stream_producer)

(** {1 Chunked Encoding} *)

(** Encode a chunk for HTTP chunked transfer encoding. *)
let encode_chunk data = 
  let len = String.length data in 
  if len = 0 then "" 
  else Printf.sprintf "%x\r\n%s\r\n" len data

(** Encode the final chunk *)
let final_chunk = "0\r\n\r\n"

(** {1 Writing Streams} *)

(** Write a streaming response to an Eio buffer. 
    Deprecated: Server handles Response.body directly now.
*)
let write_to_buffer _buf (_resp : Response.t) =
  () (* No-op *)
(** {1 Reading Streams} *)

(** Read a stream body in chunks *)
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

let silent_progress : progress_callback = {
  on_progress = (fun ~bytes_sent:_ ~total_bytes:_ -> ());
  on_complete = (fun () -> ());
  on_error = (fun _ -> ());
}

(** {1 Response Helpers} *)

(** Get status from response *)
let status t = Response.status t

(** Get headers from response *)
let headers t = Response.headers t

(** Check if response is a streaming response *)
let is_streaming (t : Response.t) = 
  match Response.body t with 
  | Response.Producer _ | Response.Stream _ -> true
  | _ -> false

(** Convert streaming response to regular Response.t (Identity) *)
let to_response (t : Response.t) = t

(** Add a header to streaming response *)
let with_header name value (t : Response.t) = 
  Response.with_header name value t

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