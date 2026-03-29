(** Multipart tests (RFC 7578) *)

open Alcotest

let test_multipart_extract_boundary () =
  let ct = "multipart/form-data; boundary=----WebKitFormBoundary7MA4YWxk" in
  let boundary = Kirin.Multipart.extract_boundary ct in
  check (option string) "boundary" (Some "----WebKitFormBoundary7MA4YWxk") boundary
;;

let test_multipart_parse_simple () =
  let boundary = "----boundary123" in
  let body =
    "------boundary123\r\n"
    ^ "Content-Disposition: form-data; name=\"username\"\r\n"
    ^ "\r\n"
    ^ "john_doe\r\n"
    ^ "------boundary123\r\n"
    ^ "Content-Disposition: form-data; name=\"email\"\r\n"
    ^ "\r\n"
    ^ "john@example.com\r\n"
    ^ "------boundary123--\r\n"
  in
  let result = Kirin.Multipart.parse ~boundary body in
  let fields = Kirin.Multipart.fields result in
  check int "field count" 2 (List.length fields);
  check
    (option string)
    "username"
    (Some "john_doe")
    (Kirin.Multipart.field "username" result);
  check
    (option string)
    "email"
    (Some "john@example.com")
    (Kirin.Multipart.field "email" result)
;;

let test_multipart_parse_file () =
  let boundary = "----boundary456" in
  let body =
    "------boundary456\r\n"
    ^ "Content-Disposition: form-data; name=\"document\"; filename=\"test.txt\"\r\n"
    ^ "Content-Type: text/plain\r\n"
    ^ "\r\n"
    ^ "Hello, World!\r\n"
    ^ "------boundary456--\r\n"
  in
  let result = Kirin.Multipart.parse ~boundary body in
  let files = Kirin.Multipart.files result in
  check int "file count" 1 (List.length files);
  match Kirin.Multipart.file "document" result with
  | Some file ->
    check (option string) "filename" (Some "test.txt") file.filename;
    check (option string) "content-type" (Some "text/plain") file.content_type;
    check string "content" "Hello, World!" file.content
  | None -> failwith "file not found"
;;

let test_multipart_mixed () =
  let boundary = "----boundary789" in
  let body =
    "------boundary789\r\n"
    ^ "Content-Disposition: form-data; name=\"title\"\r\n"
    ^ "\r\n"
    ^ "My Document\r\n"
    ^ "------boundary789\r\n"
    ^ "Content-Disposition: form-data; name=\"file\"; filename=\"doc.pdf\"\r\n"
    ^ "Content-Type: application/pdf\r\n"
    ^ "\r\n"
    ^ "PDF content here\r\n"
    ^ "------boundary789--\r\n"
  in
  let result = Kirin.Multipart.parse ~boundary body in
  check
    (option string)
    "title field"
    (Some "My Document")
    (Kirin.Multipart.field "title" result);
  check int "files count" 1 (List.length (Kirin.Multipart.files result));
  check int "fields count" 1 (List.length (Kirin.Multipart.fields result))
;;

let tests =
  [ test_case "extract boundary" `Quick test_multipart_extract_boundary
  ; test_case "parse simple form" `Quick test_multipart_parse_simple
  ; test_case "parse file upload" `Quick test_multipart_parse_file
  ; test_case "parse mixed form" `Quick test_multipart_mixed
  ]
;;
