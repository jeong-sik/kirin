(** Multipart tests (RFC 7578) *)

open Alcotest

let test_multipart_extract_boundary () =
  let ct = "multipart/form-data; boundary=----WebKitFormBoundary7MA4YWxk" in
  let boundary = Kirin.Multipart.extract_boundary ct in
  check (option string) "boundary" (Some "----WebKitFormBoundary7MA4YWxk") boundary

let test_multipart_parse_simple () =
  let boundary = "----boundary123" in
  let body =
    "------boundary123\r\n" ^
    "Content-Disposition: form-data; name=\"username\"\r\n" ^
    "\r\n" ^
    "john_doe\r\n" ^
    "------boundary123\r\n" ^
    "Content-Disposition: form-data; name=\"email\"\r\n" ^
    "\r\n" ^
    "john@example.com\r\n" ^
    "------boundary123--\r\n"
  in
  let result = Kirin.Multipart.parse ~boundary body in
  let fields = Kirin.Multipart.fields result in
  check int "field count" 2 (List.length fields);
  check (option string) "username" (Some "john_doe") (Kirin.Multipart.field "username" result);
  check (option string) "email" (Some "john@example.com") (Kirin.Multipart.field "email" result)

let test_multipart_parse_file () =
  let boundary = "----boundary456" in
  let body =
    "------boundary456\r\n" ^
    "Content-Disposition: form-data; name=\"document\"; filename=\"test.txt\"\r\n" ^
    "Content-Type: text/plain\r\n" ^
    "\r\n" ^
    "Hello, World!\r\n" ^
    "------boundary456--\r\n"
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

let test_multipart_mixed () =
  let boundary = "----boundary789" in
  let body =
    "------boundary789\r\n" ^
    "Content-Disposition: form-data; name=\"title\"\r\n" ^
    "\r\n" ^
    "My Document\r\n" ^
    "------boundary789\r\n" ^
    "Content-Disposition: form-data; name=\"file\"; filename=\"doc.pdf\"\r\n" ^
    "Content-Type: application/pdf\r\n" ^
    "\r\n" ^
    "PDF content here\r\n" ^
    "------boundary789--\r\n"
  in
  let result = Kirin.Multipart.parse ~boundary body in
  check (option string) "title field" (Some "My Document") (Kirin.Multipart.field "title" result);
  check int "files count" 1 (List.length (Kirin.Multipart.files result));
  check int "fields count" 1 (List.length (Kirin.Multipart.fields result))

(* Boundary validation regression tests.  RFC 2046 §5.1.1 requires
   boundaries to be 1..70 characters from a restricted class; the
   old [extract_boundary] returned anything after "boundary=", so
   a sender that passed [boundary=] (empty) made the delimiter
   just "--", which appears all over real bodies and turned the
   parser into garbage. *)

let test_extract_boundary_rejects_empty () =
  check (option string) "empty boundary -> None"
    None
    (Kirin.Multipart.extract_boundary "multipart/form-data; boundary=")

let test_extract_boundary_rejects_too_long () =
  let long = String.make 71 'a' in
  check (option string) "71-char boundary -> None"
    None
    (Kirin.Multipart.extract_boundary
       (Printf.sprintf "multipart/form-data; boundary=%s" long));
  let just_at_limit = String.make 70 'a' in
  check (option string) "70-char boundary -> Some"
    (Some just_at_limit)
    (Kirin.Multipart.extract_boundary
       (Printf.sprintf "multipart/form-data; boundary=%s" just_at_limit))

let test_extract_boundary_rejects_disallowed_bytes () =
  check (option string) "boundary with CR"
    None
    (Kirin.Multipart.extract_boundary "multipart/form-data; boundary=ab\rcd");
  check (option string) "boundary with LF"
    None
    (Kirin.Multipart.extract_boundary "multipart/form-data; boundary=ab\ncd");
  check (option string) "boundary with NUL"
    None
    (Kirin.Multipart.extract_boundary "multipart/form-data; boundary=ab\x00cd");
  check (option string) "boundary with tab"
    None
    (Kirin.Multipart.extract_boundary "multipart/form-data; boundary=ab\tcd")

let test_extract_boundary_accepts_quoted () =
  (* Some senders wrap the boundary as [boundary="abc"]; unquote
     before the character-class check so this still passes. *)
  check (option string) "quoted boundary"
    (Some "abc-123")
    (Kirin.Multipart.extract_boundary
       "multipart/form-data; boundary=\"abc-123\"")

(* [max_parts] cap regression test.  A malicious body with many
   short parts used to drive the splitter into unbounded
   [String.sub] allocations and an unbounded result list.  The
   new contract: once the cap is exceeded the entire parse is
   refused (empty multipart returned), so downstream code does
   not see an attacker-shaped partial view. *)
let test_parse_caps_part_count () =
  let boundary = "----testboundary" in
  let buf = Buffer.create 8192 in
  for i = 0 to 9 do
    Buffer.add_string buf "------testboundary\r\n";
    Buffer.add_string buf
      (Printf.sprintf "Content-Disposition: form-data; name=\"f%d\"\r\n" i);
    Buffer.add_string buf "\r\n";
    Buffer.add_string buf (Printf.sprintf "v%d\r\n" i)
  done;
  Buffer.add_string buf "------testboundary--\r\n";
  let body = Buffer.contents buf in
  (* Default cap (1000) allows 10 parts through. *)
  let normal = Kirin.Multipart.parse ~boundary body in
  check int "10 parts under default cap" 10
    (List.length (Kirin.Multipart.fields normal));
  (* Cap=3 should refuse the whole body and return zero parts —
     not a 3-part truncation. *)
  let capped = Kirin.Multipart.parse ~max_parts:3 ~boundary body in
  check int "over-cap body -> empty result" 0
    (List.length (Kirin.Multipart.fields capped))

let test_parse_at_cap_succeeds () =
  (* Boundary case: exactly [max_parts] fragments must succeed,
     not get refused.  The refusal is for *exceeding* the cap. *)
  let boundary = "----testboundary" in
  let buf = Buffer.create 4096 in
  for i = 0 to 2 do
    Buffer.add_string buf "------testboundary\r\n";
    Buffer.add_string buf
      (Printf.sprintf "Content-Disposition: form-data; name=\"f%d\"\r\n" i);
    Buffer.add_string buf "\r\n";
    Buffer.add_string buf (Printf.sprintf "v%d\r\n" i)
  done;
  Buffer.add_string buf "------testboundary--\r\n";
  let body = Buffer.contents buf in
  let r = Kirin.Multipart.parse ~max_parts:3 ~boundary body in
  check int "exactly 3 parts at cap" 3
    (List.length (Kirin.Multipart.fields r))

let tests = [
  test_case "extract boundary" `Quick test_multipart_extract_boundary;
  test_case "parse simple form" `Quick test_multipart_parse_simple;
  test_case "parse file upload" `Quick test_multipart_parse_file;
  test_case "parse mixed form" `Quick test_multipart_mixed;
  test_case "extract boundary rejects empty" `Quick test_extract_boundary_rejects_empty;
  test_case "extract boundary rejects too long" `Quick test_extract_boundary_rejects_too_long;
  test_case "extract boundary rejects disallowed bytes" `Quick test_extract_boundary_rejects_disallowed_bytes;
  test_case "extract boundary accepts quoted" `Quick test_extract_boundary_accepts_quoted;
  test_case "parse caps part count and refuses overflow" `Quick test_parse_caps_part_count;
  test_case "parse at exact cap succeeds" `Quick test_parse_at_cap_succeeds;
]
