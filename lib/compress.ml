(** Response compression middleware *)

(** Compression algorithms *)
type algorithm =
  | Gzip
  | Deflate

(** Parse Accept-Encoding header and return preferred algorithm *)
let parse_accept_encoding header_value =
  (* Accept-Encoding: gzip, deflate, br *)
  let encodings = String.split_on_char ',' header_value in
  let encodings = List.map String.trim encodings in
  (* Priority: gzip > deflate *)
  if List.mem "gzip" encodings then Some Gzip
  else if List.mem "deflate" encodings then Some Deflate
  else None

(** Content types that should not be compressed *)
let skip_compression_for content_type =
  let ct = String.lowercase_ascii content_type in
  (* Already compressed formats *)
  String.length ct >= 5 && String.sub ct 0 5 = "image" ||
  String.length ct >= 5 && String.sub ct 0 5 = "video" ||
  String.length ct >= 5 && String.sub ct 0 5 = "audio" ||
  (* Compressed archives *)
  let ends_with s suffix =
    let sl = String.length s and sufl = String.length suffix in
    sl >= sufl && String.sub s (sl - sufl) sufl = suffix
  in
  ends_with ct "/zip" || ends_with ct "/gzip" ||
  ends_with ct "/x-gzip" || ends_with ct "/x-compress"

(** Compress string using gzip *)
let compress_gzip input =
  let input_len = String.length input in
  if input_len = 0 then input
  else begin
    (* Use Gz module with String source and Buffer destination *)
    let output_buf = Buffer.create (input_len / 2 + 64) in
    let q = De.Queue.create 4096 in
    let w = De.Lz77.make_window ~bits:15 in

    let encoder = Gz.Def.encoder
      (`String input)
      (`Buffer output_buf)
      ~mtime:0l
      Gz.Unix
      ~q ~w
      ~level:6
    in

    let rec loop encoder =
      match Gz.Def.encode encoder with
      | `Await encoder -> loop encoder
      | `Flush encoder -> loop encoder
      | `End _encoder -> ()
    in
    loop encoder;
    Buffer.contents output_buf
  end

(** Compress string using deflate (zlib) *)
let compress_deflate input =
  let input_len = String.length input in
  if input_len = 0 then input
  else begin
    (* Use Zl module with String source and Buffer destination *)
    let output_buf = Buffer.create (input_len / 2 + 64) in
    let q = De.Queue.create 4096 in
    let w = De.Lz77.make_window ~bits:15 in

    let encoder = Zl.Def.encoder
      (`String input)
      (`Buffer output_buf)
      ~q ~w
      ~level:6
    in

    let rec loop encoder =
      match Zl.Def.encode encoder with
      | `Await encoder -> loop encoder
      | `Flush encoder -> loop encoder
      | `End _encoder -> ()
    in
    loop encoder;
    Buffer.contents output_buf
  end

(** Compress using specified algorithm *)
let compress algo input =
  match algo with
  | Gzip -> compress_gzip input
  | Deflate -> compress_deflate input

(** Get Content-Encoding header value for algorithm *)
let content_encoding_of = function
  | Gzip -> "gzip"
  | Deflate -> "deflate"

(** Minimum size to compress (smaller payloads don't benefit) *)
let min_compress_size = 1024

(** Compression middleware *)
let middleware ?(min_size = min_compress_size) : (Request.t -> Response.t) -> (Request.t -> Response.t) =
  fun handler req ->
    let resp = handler req in
    let body = Response.body resp in
    let body_len = String.length body in

    (* Skip if body is too small *)
    if body_len < min_size then resp
    else
      (* Check if content type should be compressed *)
      let content_type =
        Response.header "content-type" resp
        |> Option.value ~default:"text/plain"
      in
      if skip_compression_for content_type then resp
      else
        (* Check Accept-Encoding *)
        match Request.header "accept-encoding" req with
        | None -> resp
        | Some accept_encoding ->
          match parse_accept_encoding accept_encoding with
          | None -> resp
          | Some algo ->
            (* Compress the body *)
            let compressed = compress algo body in
            let compressed_len = String.length compressed in

            (* Only use compression if it actually reduces size *)
            if compressed_len >= body_len then resp
            else
              Response.make ~status:(Response.status resp) compressed
              |> Response.with_headers (Http.Header.to_list (Response.headers resp))
              |> Response.with_header "content-encoding" (content_encoding_of algo)
              |> Response.with_header "vary" "Accept-Encoding"
