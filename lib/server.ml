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
let make_cohttp_handler (handler : Router.handler) =
  fun _socket request body ->
    (* Read body from Eio flow *)
    let body_str = Eio.Buf_read.(parse_exn take_all) body ~max_size:(1024 * 1024 * 10) in
    (* Create Kirin request *)
    let req = Request.make ~raw:request ~body:body_str in
    (* Call handler to get Kirin response *)
    let resp = handler req in
    (* Convert to cohttp-eio response *)
    let status = Response.status resp in
    let headers = Response.headers resp in
    let body = Response.body resp in
    Cohttp_eio.Server.respond_string ~status ~headers ~body ()

(** Run the server with given handler *)
let run ?(config = default_config) ~sw ~env handler =
  let net = Eio.Stdenv.net env in

  let addr = `Tcp (Eio.Net.Ipaddr.V4.any, config.port) in
  let socket = Eio.Net.listen net ~sw ~backlog:config.backlog ~reuse_addr:true addr in

  Printf.printf "🦒 Kirin running on http://%s:%d\n%!" config.host config.port;
  Printf.printf "Type Ctrl+C to stop\n%!";

  let server = Cohttp_eio.Server.make ~callback:(make_cohttp_handler handler) () in
  Cohttp_eio.Server.run socket server ~on_error:(fun exn ->
    Printf.eprintf "Server error: %s\n%!" (Printexc.to_string exn)
  )

(** Main entry point - sets up Eio and runs server *)
let start ?(port = 8000) handler =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let config = { default_config with port } in
  run ~config ~sw ~env handler
