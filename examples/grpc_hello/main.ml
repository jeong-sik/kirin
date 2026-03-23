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
  Grpc.service "helloworld.Greeter"
  |> Grpc.unary "SayHello" (fun request ->
       let name = decode_hello_request request in
       encode_hello_reply name)
  |> Grpc.server_streaming "SayHelloStream" (fun request ->
       (* Stream multiple greetings *)
       let name = decode_hello_request request in
       let stream = Grpc.stream_create 10 in
       for i = 1 to 3 do
         Grpc.stream_add stream
           (Printf.sprintf "Hello %d, %s!" i name)
       done;
       Grpc.stream_close stream;
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
    Grpc.grpc_server ()
    |> Grpc.add_service greeter_service
    |> Grpc.with_interceptor (Grpc.logging_interceptor ())
    |> Grpc.with_interceptor (Grpc.timing_interceptor ())
  in
  Printf.printf "[Kirin] Starting gRPC server on 0.0.0.0:50051\n%!";
  Grpc.Server.serve ~sw ~env server
