(** Server module - Eio-based HTTP server *)

(** Server configuration *)
type config = {
  port : int;
  host : string;
  backlog : int;
}

let default_config = {
  port = 8000;
  host = "0.0.0.0";
  backlog = 128;
}

(** Convert Kirin handler to cohttp-eio handler *)

let make_cohttp_handler sw (handler : Router.handler) =

  fun _socket request body ->

    (* Read body from Eio flow 

       TODO: Implement streaming body instead of full allocation to avoid OOM

    *)

    let body_str = Eio.Buf_read.(parse_exn take_all) body ~max_size:(1024 * 1024 * 10) in

    (* Create Kirin request *)

    let req = Request.make ~raw:request ~body:body_str in

    (* Call handler to get Kirin response *)

    let resp = handler req in

    (* Convert to cohttp-eio response *)

    let status = Response.status resp in

    let headers = Response.headers resp in

    let body = Response.body resp in

    

    match body with

    | Response.String s ->

      Cohttp_eio.Server.respond_string ~status ~headers ~body:s ()

    | Response.Stream s ->

      (* Convert Eio.Stream to string body (Temporary) *)

      let buf = Buffer.create 4096 in

      (try

        while true do

          let chunk = Eio.Stream.take s in

          if chunk = "" then raise End_of_file;

          Buffer.add_string buf chunk

        done

       with End_of_file -> ());

      let body = Buffer.contents buf in

      Cohttp_eio.Server.respond_string ~status ~headers ~body ()

    | Response.Producer p ->

      (* Create stream and fork producer *)

      let stream = Eio.Stream.create 16 in

      Eio.Fiber.fork ~sw (fun () -> 

        try p stream with exn -> 

          Printf.eprintf "Stream producer error: %s\n%!" (Printexc.to_string exn)

      );

      

      (* Consume stream (Temporary: to string) *)

      let buf = Buffer.create 4096 in

      (try

        while true do

          let chunk = Eio.Stream.take stream in

          if chunk = "" then raise End_of_file;

          Buffer.add_string buf chunk

        done

       with End_of_file -> ());

      let body = Buffer.contents buf in

      Cohttp_eio.Server.respond_string ~status ~headers ~body ()

(** Run the server with given handler *)
let run ?(config = default_config) ~sw ~env handler =
  let net = Eio.Stdenv.net env in
  let addr = `Tcp (Eio.Net.Ipaddr.V4.any, config.port) in
  (* Enable SO_REUSEPORT/ADDR to allow fast restarts and multicore sharing *)
  let socket = Eio.Net.listen net ~sw ~backlog:config.backlog ~reuse_addr:true ~reuse_port:true addr in

  Printf.printf "🦒 Kirin running on http://%s:%d\n%!" config.host config.port;

  let server = Cohttp_eio.Server.make ~callback:(make_cohttp_handler sw handler) () in
  Cohttp_eio.Server.run socket server ~on_error:(fun exn ->
    Printf.eprintf "Server error: %s\n%!" (Printexc.to_string exn)
  )

(** Main entry point - sets up Eio and runs multicore server 
    
    In OCaml 5, we use Domain_manager to distribute request handling across cores.
    Shared-socket pattern is used for optimal efficiency.
*)
let start ?(port = 8000) ?(domains = Domain.recommended_domain_count ()) handler =
  Printf.printf "🦒 Kirin scaling up across %d domains (shared socket)...
%!" domains;
  
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  
  (* Start Async Logger (Phase 30) *)
  Logger.start sw;
  
  let net = Eio.Stdenv.net env in
  let domain_mgr = Eio.Stdenv.domain_mgr env in
  
  let addr = `Tcp (Eio.Net.Ipaddr.V4.any, port) in
  (* Enable SO_REUSEPORT/ADDR to allow fast restarts and multicore sharing *)
  let socket = Eio.Net.listen net ~sw ~backlog:128 ~reuse_addr:true ~reuse_port:true addr in
  
  (* Ensure socket is closed when switch finishes *)
  Eio.Switch.on_release sw (fun () -> Eio.Net.close socket);

  let server = Cohttp_eio.Server.make ~callback:(make_cohttp_handler sw handler) () in
  
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