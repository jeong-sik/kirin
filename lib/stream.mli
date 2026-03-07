(** Kirin Streaming Module

    High-performance streaming I/O for large data handling.
    Uses Eio's direct-style async for chunked transfers. *)

(** {1 Types} *)

(** Chunk yielder function type. *)
type yielder = string -> unit

(** Stream producer - generates chunks on demand. *)
type producer = yielder -> unit

(** Progress callback for file operations. *)
type progress_callback = {
  on_progress : bytes_sent:int -> total_bytes:int option -> unit;
  on_complete : unit -> unit;
  on_error : exn -> unit;
}

(** {1 MIME Types} *)

(** Common MIME types for file streaming. *)
val mime_types : (string * string) list

(** [mime_of_filename path] returns the MIME type for the given filename. *)
val mime_of_filename : string -> string

(** {1 Configuration} *)

(** Default chunk size for file streaming (64KB). *)
val default_chunk_size : int

(** Maximum chunk size (1MB). *)
val max_chunk_size : int

(** {1 Constructors} *)

(** [response ?status ?headers producer] creates a streaming response
    from a producer function. *)
val response :
  ?status:Http.Status.t ->
  ?headers:Http.Header.t ->
  producer ->
  Response.t

(** [flow_response ?status ?headers writer] creates a streaming response
    from an Eio flow writer. *)
val flow_response :
  ?status:Http.Status.t ->
  ?headers:Http.Header.t ->
  ('a -> unit) ->
  Response.t

(** [file_response ?filename ?content_type ?chunk_size path] creates a
    file download response with streaming and Content-Disposition header. *)
val file_response :
  ?filename:string ->
  ?content_type:string ->
  ?chunk_size:int ->
  string ->
  Response.t

(** [file_inline ?content_type ?chunk_size path] creates an inline file
    response (displayed in browser, not downloaded). *)
val file_inline :
  ?content_type:string ->
  ?chunk_size:int ->
  string ->
  Response.t

(** {1 Chunked Encoding} *)

(** [encode_chunk data] encodes a string for HTTP chunked transfer encoding. *)
val encode_chunk : string -> string

(** The final chunk marker for chunked transfer encoding. *)
val final_chunk : string

(** {1 Writing Streams} *)

(** [write_to_buffer buf resp] is a no-op (deprecated). *)
val write_to_buffer : 'a -> Response.t -> unit

(** {1 Reading Streams} *)

(** [read_chunks ~request ~chunk_size handler] reads the request body in
    fixed-size chunks, calling [handler] for each chunk. *)
val read_chunks : request:Request.t -> chunk_size:int -> (string -> unit) -> unit

(** [read_with_progress ~request ~chunk_size ~progress handler] reads chunks
    with progress tracking. *)
val read_with_progress :
  request:Request.t ->
  chunk_size:int ->
  progress:progress_callback ->
  (string -> unit) ->
  unit

(** {1 File Upload Handling} *)

(** [save_upload ~request ~dest_path ?chunk_size ()] saves the request body
    to a file. Returns the number of bytes written. *)
val save_upload :
  request:Request.t ->
  dest_path:string ->
  ?chunk_size:int ->
  unit ->
  int

(** [save_upload_with_progress ~request ~dest_path ?chunk_size ~progress ()]
    saves with progress tracking. Returns the number of bytes written. *)
val save_upload_with_progress :
  request:Request.t ->
  dest_path:string ->
  ?chunk_size:int ->
  progress:progress_callback ->
  unit ->
  int

(** {1 Utilities} *)

(** [stderr_progress ?prefix ()] creates a progress callback that prints to stderr. *)
val stderr_progress : ?prefix:string -> unit -> progress_callback

(** A silent progress callback (no output). *)
val silent_progress : progress_callback

(** {1 Response Helpers} *)

(** [status t] returns the status of a response. *)
val status : Response.t -> Http.Status.t

(** [headers t] returns the headers of a response. *)
val headers : Response.t -> Http.Header.t

(** [is_streaming t] returns [true] if the response uses streaming body. *)
val is_streaming : Response.t -> bool

(** [to_response t] returns the response unchanged (identity). *)
val to_response : Response.t -> Response.t

(** [with_header name value t] adds a header to the response. *)
val with_header : string -> string -> Response.t -> Response.t

(** [with_content_type ct t] sets the Content-Type header. *)
val with_content_type : string -> Response.t -> Response.t

(** {1 Generator Helpers} *)

(** [of_lines lines] creates a producer that yields lines from a list. *)
val of_lines : string list -> producer

(** [of_json_array items to_json] creates a producer that yields a JSON array,
    serializing each item with [to_json]. *)
val of_json_array : 'a list -> ('a -> Yojson.Safe.t) -> producer

(** [of_seq seq] creates a producer from a string sequence. *)
val of_seq : string Seq.t -> producer
