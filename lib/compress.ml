(** Response compression middleware *)

(** Compression algorithms *)
type algorithm =
  | Gzip
  | Deflate

(* RFC 7231 §3.1.2.1 + §5.3.4: an Accept-Encoding token is a
   coding name plus optional parameters, the most common of which
   is [q=<float>].  Two contracts the old [List.mem]-based parser
   silently broke:

   - codings are case-insensitive ([GZIP] is the same as [gzip])
   - [q=0] is an explicit *reject*, not a no-op — the client is
     saying "I cannot or will not accept this encoding"

   The old code therefore (a) refused to compress for clients
   that capitalised their header (browsers don't, but proxies and
   custom HTTP clients sometimes do), and (b) compressed *anyway*
   for clients that explicitly opted out via [q=0].  The latter
   is the more concerning surface: a client running in a
   CRIME/BREACH-aware environment may set
   [Accept-Encoding: gzip;q=0] precisely to disable compression,
   and the server has to honour that. *)
let parse_encoding_token raw =
  match String.split_on_char ';' raw with
  | [] -> None
  | name :: params ->
    let name = String.trim name |> String.lowercase_ascii in
    if name = "" then None
    else
      let q =
        List.fold_left (fun acc param ->
          match String.split_on_char '=' (String.trim param) with
          | [k; v] when String.lowercase_ascii (String.trim k) = "q" ->
            (match float_of_string_opt (String.trim v) with
             | Some f -> f
             | None -> acc)
          | _ -> acc
        ) 1.0 params
      in
      Some (name, q)

(** Parse Accept-Encoding header and return the preferred algorithm
    that the client *accepts* with [q > 0].  Priority gzip > deflate
    matches the pre-PR ordering. *)
let parse_accept_encoding header_value =
  let tokens =
    String.split_on_char ',' header_value
    |> List.filter_map parse_encoding_token
  in
  let accepts name =
    List.exists (fun (n, q) -> n = name && q > 0.0) tokens
  in
  if accepts "gzip" then Some Gzip
  else if accepts "deflate" then Some Deflate
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
    
    match Response.body resp with
    | Response.Stream _ | Response.Producer _ -> resp (* Streaming compression not yet supported *)
    | Response.String body ->
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
                Response.make ~status:(Response.status resp)
                  ~headers:(Response.headers resp)
                  (`String compressed)
                |> Response.with_header "content-encoding" (content_encoding_of algo)
                |> Response.with_header "vary" "Accept-Encoding"
