(** Kirin gRPC Hello World Example

    This example shows how to create a simple gRPC service with Kirin.

    Note: For actual protobuf support, use ocaml-protoc or similar.
    This example uses raw bytes for simplicity.

    Run: dune exec examples/grpc_hello/main.exe
*)

(* Simple message encoding (in real app, use protobuf) *)
let encode_hello_reply name =
  Printf.sprintf "Hello, %s!" name

let decode_hello_request bytes =
  (* In real app, decode protobuf message *)
  bytes

(* Create the Greeter service *)
let greeter_service =
  Kirin.Grpc.service "helloworld.Greeter"
  |> Kirin.Grpc.unary "SayHello" (fun request ->
       let name = decode_hello_request request in
       encode_hello_reply name)
  |> Kirin.Grpc.server_streaming "SayHelloStream" (fun request ->
       (* Stream multiple greetings *)
       let name = decode_hello_request request in
       let stream = Kirin.Grpc.stream_create 10 in
       for i = 1 to 3 do
         Kirin.Grpc.stream_add stream
           (Printf.sprintf "Hello %d, %s!" i name)
       done;
       Kirin.Grpc.stream_close stream;
       stream)

(* HTTP routes for comparison - shown for reference *)
let _http_routes = Kirin.router [
  Kirin.get "/" (fun _ ->
    Kirin.html "<h1>Kirin HTTP + gRPC Server</h1>");

  Kirin.get "/hello/:name" (fun req ->
    let name = Kirin.param "name" req in
    Kirin.json (`Assoc [
      ("message", `String (encode_hello_reply name));
      ("protocol", `String "HTTP/1.1");
    ]));
]

let () =
  Printf.printf "=== Kirin gRPC Example ===\n";
  Printf.printf "gRPC port: 50051\n";
  Printf.printf "HTTP port: 8000\n";
  Printf.printf "\n";
  Printf.printf "Test HTTP: curl http://localhost:8000/hello/World\n";
  Printf.printf "Test gRPC: grpcurl -plaintext localhost:50051 helloworld.Greeter/SayHello\n";
  Printf.printf "\n";

  (* Start the gRPC server only for this example *)
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let server =
    Kirin.Grpc.grpc_server ()
    |> Kirin.Grpc.add_service greeter_service
    |> Kirin.Grpc.with_interceptor (Kirin.Grpc.logging_interceptor ())
    |> Kirin.Grpc.with_interceptor (Kirin.Grpc.timing_interceptor ())
  in
  Printf.printf "[Kirin] Starting gRPC server on 0.0.0.0:50051\n%!";
  Kirin.Grpc.Server.serve ~sw ~env server
