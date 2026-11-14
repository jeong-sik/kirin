(** Server module - Eio-based HTTP server *)

(** Server configuration *)
type config = {
  port : int;
  host : string;
  backlog : int;
  request_timeout : float;  (** Timeout for individual requests in seconds *)
  stream_read_timeout : float;  (** Timeout for reading stream chunks in seconds *)
  max_body_size : int;  (** Maximum request body size in bytes. Default 10 MB. *)
}

let default_config = {
  port = 8000;
  host = "0.0.0.0";
  backlog = 128;
  request_timeout = 30.0;  (** 30 seconds default request timeout *)
  stream_read_timeout = 5.0;  (** 5 seconds default stream read timeout *)
  max_body_size = 10_485_760;  (** 10 MB *)
}

(** Adapter: Eio.Stream.t string -> Eio.Flow.source for true chunked streaming.
    Reads from the stream on demand, buffering partial chunks. *)
module Stream_source = struct
  type t = {
    stream : string option Eio.Stream.t;
    mutable buf : string;
    mutable pos : int;
  }

  let read_methods = []

  let single_read t dst =
    (* Use buffered data first *)
    if t.pos < String.length t.buf then begin
      let available = String.length t.buf - t.pos in
      let n = min available (Cstruct.length dst) in
      Cstruct.blit_from_string t.buf t.pos dst 0 n;
      t.pos <- t.pos + n;
      n
    end else begin
      (* Take next chunk from stream; None = end of stream *)
      match Eio.Stream.take t.stream with
      | None -> raise End_of_file
      | Some chunk ->
        let n = min (String.length chunk) (Cstruct.length dst) in
        Cstruct.blit_from_string chunk 0 dst 0 n;
        t.buf <- chunk;
        t.pos <- n;
        n
    end
end

let stream_source_handler : (Stream_source.t, Eio.Flow.source_ty) Eio.Resource.handler =
  Eio.Flow.Pi.source (module Stream_source)

let make_stream_source (stream : string option Eio.Stream.t) : Eio.Flow.source_ty Eio.Resource.t =
  Eio.Resource.T ({ Stream_source.stream; buf = ""; pos = 0 }, stream_source_handler)

(* HIGH-RISK-UNREVIEWED: request framing on keep-alive connections.

   Cohttp_eio.Server reads the next request from the same buffer the body
   reader uses, as soon as the response is written. Body bytes a handler did
   not read would be parsed as the start of the next request on that
   connection. So the rest of every body is read and dropped before the
   response, and a connection whose body cannot be finished is closed after
   its response instead of being reused. *)

(** How a request body ended after the handler returned. *)
type body_end =
  | Read_to_end  (** every byte of the body was read; the connection can be reused *)
  | Left_unread  (** bytes may remain; the connection must close after the response *)

(** Raised by a response writer after the response is flushed, to end the
    connection: the request body was not read to its end. [on_error] ignores
    it. *)
exception Close_after_response

(** Read what the handler left of the request body and drop it. Gives up,
    leaving the rest unread, after [config.max_body_size] more bytes or
    [config.request_timeout] seconds. *)
let drain_body ~clock ~config body_buf =
  let rec drop dropped =
    if Eio.Buf_read.at_end_of_input body_buf then Read_to_end
    else
      let n = Eio.Buf_read.buffered_bytes body_buf in
      if dropped + n > config.max_body_size then Left_unread
      else begin
        Eio.Buf_read.consume body_buf n;
        drop (dropped + n)
      end
  in
  match Eio.Time.with_timeout clock config.request_timeout (fun () -> Ok (drop 0)) with
  | Ok body_end -> body_end
  | Error `Timeout -> Left_unread

let on_server_error ~where = function
  | Close_after_response -> ()
  | exn -> Logger.error "%s: %s" where (Printexc.to_string exn)

(** Convert Kirin handler to cohttp-eio handler *)
let make_cohttp_handler ~clock ~config _sw (handler : Router.handler) =
  fun _socket request body ->
    (* Create Kirin request with raw body source (Zero-Copy) *)
    (* Cohttp_eio passes body as Flow source, convert to Buf_read.
       max_size enforces the body limit; Buf_read.Buffer_limit_exceeded
       is raised lazily when the handler reads beyond the limit. *)
    let body_buf = Eio.Buf_read.of_flow body ~initial_size:4096 ~max_size:config.max_body_size in
    let req = Request.make ~raw:request ~body_source:body_buf in

    (* Call handler with request timeout to prevent hanging requests *)
    let handle_with_timeout () : Response.t =
      (* with_timeout expects fn to return result type *)
      match Eio.Time.with_timeout clock config.request_timeout (fun () ->
        Ok (handler req)
      ) with
      | Ok resp -> resp
      | Error `Timeout ->
          Logger.warn "Request timeout after %.1fs: %s %s"
            config.request_timeout
            (Http.Method.to_string (Cohttp.Request.meth request))
            (Cohttp.Request.resource request);
          Response.make
            ~status:`Gateway_timeout
            ~headers:(Cohttp.Header.of_list [("content-type", "application/json")])
            (`String "{\"error\":\"Request timeout\"}")
    in

    let resp, body_end =
      match handle_with_timeout () with
      | resp -> (resp, drain_body ~clock ~config body_buf)
      | exception Eio.Buf_read.Buffer_limit_exceeded ->
        Logger.warn "Request body too large (limit %d bytes): %s %s"
          config.max_body_size
          (Http.Method.to_string (Cohttp.Request.meth request))
          (Cohttp.Request.resource request);
        ( Response.make
            ~status:`Request_entity_too_large
            ~headers:(Cohttp.Header.of_list [("content-type", "application/json")])
            (`String (Printf.sprintf "{\"error\":\"Request body too large\",\"max_bytes\":%d}" config.max_body_size)),
          Left_unread )
    in

    (* Convert to cohttp-eio response *)
    let status = Response.status resp in
    let headers =
      match body_end with
      | Read_to_end -> Response.headers resp
      | Left_unread -> Cohttp.Header.replace (Response.headers resp) "connection" "close"
    in
    let body = Response.body resp in

    let respond =
    match body with
    | Response.String s ->
        Cohttp_eio.Server.respond_string ~status ~headers ~body:s ()

    | Response.Stream s ->
        (* True chunked streaming via Flow.source adapter *)
        let body = make_stream_source s in
        Cohttp_eio.Server.respond ~status ~headers ~body ()

    | Response.Producer p ->
        (* Create a per-request switch so the producer fiber is cancelled
           when the response finishes (e.g. client disconnect). Without
           this, the fiber lives on the server switch and outlives the
           request. *)
        Eio.Switch.run @@ fun request_sw ->
        let stream = Eio.Stream.create 16 in
        Eio.Fiber.fork ~sw:request_sw (fun () ->
          try p stream with
          | Eio.Cancel.Cancelled _ as exn -> raise exn
          | exn ->
            Logger.error "Stream producer error: %s" (Printexc.to_string exn);
            (* Signal end of stream on error *)
            Eio.Stream.add stream None
        );
        let body = make_stream_source stream in
        Cohttp_eio.Server.respond ~status ~headers ~body ()
    in
    match body_end with
    | Read_to_end -> respond
    | Left_unread -> fun writer -> respond writer; raise Close_after_response

(** Run the server with given handler *)
let run ?(config = default_config) ~sw ~env handler =
  let net = Eio.Stdenv.net env in
  let clock = Eio.Stdenv.clock env in
  let addr = `Tcp (Eio.Net.Ipaddr.V4.any, config.port) in
  (* Enable SO_REUSEPORT/ADDR to allow fast restarts and multicore sharing *)
  let socket = Eio.Net.listen net ~sw ~backlog:config.backlog ~reuse_addr:true ~reuse_port:true addr in

  Printf.printf "🦒 Kirin running on http://%s:%d\n%!" config.host config.port;

  let server = Cohttp_eio.Server.make ~callback:(make_cohttp_handler ~clock ~config sw handler) () in
  Cohttp_eio.Server.run socket server ~on_error:(on_server_error ~where:"Server error")

(** Main entry point - sets up Eio and runs multicore server

    In OCaml 5, we use Domain_manager to distribute request handling across cores.
    Shared-socket pattern is used for optimal efficiency.
*)
let start ?(port = 8000) ?(request_timeout = 30.0) ?(stream_read_timeout = 5.0) ?(max_body_size = 10_485_760) ?(domains = Domain.recommended_domain_count ()) handler =
  Printf.printf "🦒 Kirin scaling up across %d domains (shared socket)...\n%!" domains;

  let config = { default_config with
    port;
    request_timeout;
    stream_read_timeout;
    max_body_size;
  } in

  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->

  (* Set global filesystem for Fs_compat (Eio-native file I/O) *)
  Fs_compat.set_fs (Eio.Stdenv.fs env);

  (* Set global clock for Time_compat (Eio-native timestamps) *)
  let clock = Eio.Stdenv.clock env in
  Time_compat.set_clock clock;

  (* Start Async Logger (Phase 30) *)
  Logger.start sw;

  let net = Eio.Stdenv.net env in
  let domain_mgr = Eio.Stdenv.domain_mgr env in

  let addr = `Tcp (Eio.Net.Ipaddr.V4.any, port) in
  (* Enable SO_REUSEPORT/ADDR to allow fast restarts and multicore sharing *)
  let socket = Eio.Net.listen net ~sw ~backlog:config.backlog ~reuse_addr:true ~reuse_port:true addr in

  (* Ensure socket is closed when switch finishes *)
  Eio.Switch.on_release sw (fun () -> Eio.Net.close socket);

  let server = Cohttp_eio.Server.make ~callback:(make_cohttp_handler ~clock ~config sw handler) () in

  let run_worker () =
    try
      Cohttp_eio.Server.run socket server
        ~on_error:(on_server_error
                     ~where:(Printf.sprintf "Worker error on domain %d" (Domain.self () :> int)))
    with
    | Eio.Cancel.Cancelled _ as e -> raise e
    | e ->
      Logger.error "Fatal worker error: %s" (Printexc.to_string e)
  in

  (* Spawn workers on separate domains *)
  let workers = List.init (domains - 1) (fun _ ->
    fun () -> Eio.Domain_manager.run domain_mgr run_worker
  ) in

  (* Run workers in parallel including the current domain *)
  Eio.Fiber.all (List.map (fun f -> f) workers @ [run_worker]);
  ()