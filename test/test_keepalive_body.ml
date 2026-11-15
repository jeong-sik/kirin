(* Requests on one keep-alive connection. The server must start reading the
   next request where the previous body ends, whether or not the handler read
   that body; a body it cannot finish reading ends the connection. *)

let max_body_size = 64

(* GET /ok answers "ok", POST /echo reads its body; any other path is a 404
   that never reads the body. *)
let routes =
  Kirin.router
    [ Kirin.get "/ok" (fun _ -> Kirin.text "ok");
      Kirin.post "/echo" (fun req -> Kirin.text (Kirin.body req)) ]

let post path body =
  Printf.sprintf "POST %s HTTP/1.1\r\nHost: t\r\nContent-Length: %d\r\n\r\n%s" path
    (String.length body) body

let get_then_close path =
  Printf.sprintf "GET %s HTTP/1.1\r\nHost: t\r\nConnection: close\r\n\r\n" path

let free_port () =
  let s = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Fun.protect ~finally:(fun () -> Unix.close s) (fun () ->
    Unix.bind s (Unix.ADDR_INET (Unix.inet_addr_loopback, 0));
    match Unix.getsockname s with
    | Unix.ADDR_INET (_, port) -> port
    | Unix.ADDR_UNIX _ -> Alcotest.fail "a TCP socket has no port")

(* Writes [wire] on one connection and returns everything the server sends
   until it closes the connection. *)
let exchange wire =
  Eio_main.run @@ fun env ->
  let port = free_port () in
  let config =
    { Kirin.Server.default_config with port; max_body_size; request_timeout = 5.0 }
  in
  Eio.Switch.run @@ fun sw ->
  Eio.Fiber.first
    (fun () ->
      Kirin.Server.run ~config ~sw ~env routes;
      "")
    (fun () ->
      let flow =
        Eio.Net.connect ~sw (Eio.Stdenv.net env) (`Tcp (Eio.Net.Ipaddr.V4.loopback, port))
      in
      Eio.Flow.copy_string wire flow;
      Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 5.0 (fun () ->
        Eio.Buf_read.(parse_exn ~max_size:1_000_000 take_all) flow))

type response = { status : string; headers : (string * string) list; body : string }

let find_from s needle from =
  let n = String.length needle in
  let rec at i =
    if i + n > String.length s then None
    else if String.sub s i n = needle then Some i
    else at (i + 1)
  in
  at from

(* The responses in [out], one after another, each body as long as its
   Content-Length. Bytes that do not make a response end the list as one
   response whose status is "unparsed: <bytes>". *)
let responses out =
  let header line =
    match String.index_opt line ':' with
    | Some i ->
        Some
          ( String.lowercase_ascii (String.sub line 0 i),
            String.trim (String.sub line (i + 1) (String.length line - i - 1)) )
    | None -> None
  in
  let unparsed pos = { status = "unparsed: " ^ String.sub out pos (String.length out - pos); headers = []; body = "" } in
  let rec go pos acc =
    if pos >= String.length out then List.rev acc
    else
      match find_from out "\r\n\r\n" pos with
      | None -> List.rev (unparsed pos :: acc)
      | Some head_end -> (
          match String.split_on_char '\n' (String.sub out pos (head_end - pos)) |> List.map String.trim with
          | status :: lines -> (
              let headers = List.filter_map header lines in
              match Option.bind (List.assoc_opt "content-length" headers) int_of_string_opt with
              | Some len when head_end + 4 + len <= String.length out ->
                  let body = String.sub out (head_end + 4) len in
                  go (head_end + 4 + len) ({ status; headers; body } :: acc)
              | Some _ | None -> List.rev (unparsed pos :: acc))
          | [] -> List.rev (unparsed pos :: acc))
  in
  go 0 []

let status_lines out = List.map (fun r -> r.status) (responses out)
let has_close r = List.assoc_opt "connection" r.headers = Some "close"
let statuses = Alcotest.(list string)

let only_response out =
  match responses out with
  | [ r ] -> r
  | rs -> Alcotest.failf "expected one response, got %d" (List.length rs)

let test_unread_body_is_skipped () =
  let out = exchange (post "/missing" {|{"a":1}|} ^ get_then_close "/ok") in
  Alcotest.check statuses "404, then the next request on the same connection"
    [ "HTTP/1.1 404 Not Found"; "HTTP/1.1 200 OK" ]
    (status_lines out)

let test_read_body_keeps_connection () =
  let out = exchange (post "/echo" "hello" ^ get_then_close "/ok") in
  Alcotest.check statuses "both answered" [ "HTTP/1.1 200 OK"; "HTTP/1.1 200 OK" ]
    (status_lines out);
  match responses out with
  | first :: _ ->
      Alcotest.(check string) "the echo comes back" "hello" first.body;
      Alcotest.(check bool) "the first response keeps the connection" false (has_close first)
  | [] -> Alcotest.fail "no response"

let test_unread_body_over_limit_closes () =
  let out = exchange (post "/missing" (String.make (2 * max_body_size) 'x')) in
  Alcotest.check statuses "one response, nothing read from the rest of the body"
    [ "HTTP/1.1 404 Not Found" ] (status_lines out);
  Alcotest.(check bool) "connection: close" true (has_close (only_response out))

let test_too_large_closes () =
  let out = exchange (post "/echo" (String.make (2 * max_body_size) 'x')) in
  Alcotest.check statuses "one 413, nothing read from the rest of the body"
    [ "HTTP/1.1 413 Request Entity Too Large" ] (status_lines out);
  Alcotest.(check bool) "connection: close" true (has_close (only_response out))

let () =
  Alcotest.run "Keep-alive body"
    [ ( "body",
        [ Alcotest.test_case "an unread body is skipped" `Quick test_unread_body_is_skipped;
          Alcotest.test_case "a read body keeps the connection" `Quick
            test_read_body_keeps_connection;
          Alcotest.test_case "an unread body over the limit closes" `Quick
            test_unread_body_over_limit_closes;
          Alcotest.test_case "a body too large closes" `Quick test_too_large_closes ] ) ]
