(** Server module - Eio-based HTTP server *)

(** Server configuration *)
type config = {
  port : int;
  host : string;
  backlog : int;
  request_timeout : float;  (** Timeout for individual requests in seconds *)
  stream_read_timeout : float;  (** Timeout for reading stream chunks in seconds *)
}

let default_config = {
  port = 8000;
  host = "0.0.0.0";
  backlog = 128;
  request_timeout = 30.0;  (** 30 seconds default request timeout *)
  stream_read_timeout = 5.0;  (** 5 seconds default stream read timeout *)
}

(** Read stream chunks with timeout to prevent blocking forever.
    Returns collected string. On timeout, returns partial data collected so far. *)
let read_stream_with_timeout ~clock ~timeout (stream : string Eio.Stream.t) : string =
  let buf = Buffer.create 4096 in
  let rec loop () : string =
    (* with_timeout expects fn to return result type *)
    match Eio.Time.with_timeout clock timeout (fun () ->
      Ok (Eio.Stream.take stream)
    ) with
    | Ok chunk when chunk = "" ->
        (* Empty string = end of stream *)
        Buffer.contents buf
    | Ok chunk ->
        Buffer.add_string buf chunk;
        loop ()
    | Error `Timeout ->
        (* Timeout reading chunk - return what we have *)
        Logger.warn "Stream read timeout after %.1fs, returning partial data (%d bytes)"
          timeout (Buffer.length buf);
        Buffer.contents buf
  in
  loop ()

(** Convert Kirin handler to cohttp-eio handler *)
let make_cohttp_handler ~clock ~config sw (handler : Router.handler) =
  fun _socket request body ->
    (* Create Kirin request with raw body source (Zero-Copy) *)
    (* Cohttp_eio passes body as Flow source, convert to Buf_read *)
    let body_buf = Eio.Buf_read.of_flow body ~initial_size:4096 ~max_size:max_int in
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

    let resp = handle_with_timeout () in

    (* Convert to cohttp-eio response *)
    let status = Response.status resp in
    let headers = Response.headers resp in
    let body = Response.body resp in

    match body with
    | Response.String s ->
        Cohttp_eio.Server.respond_string ~status ~headers ~body:s ()

    | Response.Stream s ->
        (* Convert Eio.Stream to string body with timeout protection *)
        let body = read_stream_with_timeout ~clock ~timeout:config.stream_read_timeout s in
        Cohttp_eio.Server.respond_string ~status ~headers ~body ()

    | Response.Producer p ->
        (* Create stream and fork producer *)
        let stream = Eio.Stream.create 16 in
        Eio.Fiber.fork ~sw (fun () ->
          try p stream with exn ->
            Logger.error "Stream producer error: %s" (Printexc.to_string exn);
            (* Signal end of stream on error *)
            Eio.Stream.add stream ""
        );
        (* Consume stream with timeout protection *)
        let body = read_stream_with_timeout ~clock ~timeout:config.stream_read_timeout stream in
        Cohttp_eio.Server.respond_string ~status ~headers ~body ()

(** Run the server with given handler *)
let run ?(config = default_config) ~sw ~env handler =
  let net = Eio.Stdenv.net env in
  let clock = Eio.Stdenv.clock env in
  let addr = `Tcp (Eio.Net.Ipaddr.V4.any, config.port) in
  (* Enable SO_REUSEPORT/ADDR to allow fast restarts and multicore sharing *)
  let socket = Eio.Net.listen net ~sw ~backlog:config.backlog ~reuse_addr:true ~reuse_port:true addr in

  Printf.printf "🦒 Kirin running on http://%s:%d\n%!" config.host config.port;

  let server = Cohttp_eio.Server.make ~callback:(make_cohttp_handler ~clock ~config sw handler) () in
  Cohttp_eio.Server.run socket server ~on_error:(fun exn ->
    Printf.eprintf "Server error: %s\n%!" (Printexc.to_string exn)
  )

(** Main entry point - sets up Eio and runs multicore server

    In OCaml 5, we use Domain_manager to distribute request handling across cores.
    Shared-socket pattern is used for optimal efficiency.
*)
let start ?(port = 8000) ?(request_timeout = 30.0) ?(stream_read_timeout = 5.0) ?(domains = Domain.recommended_domain_count ()) handler =
  Printf.printf "🦒 Kirin scaling up across %d domains (shared socket)...\n%!" domains;

  let config = { default_config with
    port;
    request_timeout;
    stream_read_timeout
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
      Cohttp_eio.Server.run socket server ~on_error:(fun exn ->
        Logger.error "Worker error on domain %d: %s" (Domain.self () :> int) (Printexc.to_string exn)
      )
    with e ->
      Logger.error "Fatal worker error: %s" (Printexc.to_string e)
  in

  (* Spawn workers on separate domains *)
  let workers = List.init (domains - 1) (fun _ ->
    fun () -> Eio.Domain_manager.run domain_mgr run_worker
  ) in

  (* Run workers in parallel including the current domain *)
  Eio.Fiber.all (List.map (fun f -> f) workers @ [run_worker]);
  ()