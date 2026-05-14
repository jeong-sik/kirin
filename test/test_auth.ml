(** Authentication Module Tests *)

open Alcotest

(** {1 JWT Tests} *)

let jwt_secret = "test-secret-key-for-jwt"

let test_jwt_encode () =
  let payload = `Assoc [("user_id", `String "123")] in
  let token = Kirin_auth.Jwt.encode ~secret:jwt_secret ~payload
    ~sub:"user-123" ~exp:(Unix.gettimeofday () +. 3600.) () in
  check bool "token is non-empty" true (String.length token > 0);
  (* JWT has 3 parts separated by dots *)
  let parts = String.split_on_char '.' token in
  check int "JWT has 3 parts" 3 (List.length parts)

let test_jwt_decode () =
  let payload = `Assoc [("user_id", `String "123")] in
  let token = Kirin_auth.Jwt.encode ~secret:jwt_secret ~payload
    ~sub:"user-123" ~exp:(Unix.gettimeofday () +. 3600.) () in
  match Kirin_auth.Jwt.decode ~secret:jwt_secret token with
  | Ok decoded ->
      check (option string) "sub matches" (Some "user-123") decoded.claims.sub
  | Error msg ->
      fail ("JWT decode failed: " ^ msg)

let test_jwt_expired () =
  let payload = `Assoc [] in
  let token = Kirin_auth.Jwt.encode ~secret:jwt_secret ~payload
    ~sub:"user-123" ~exp:(Unix.gettimeofday () -. 100.) () in  (* Already expired *)
  match Kirin_auth.Jwt.decode ~secret:jwt_secret token with
  | Ok _ -> fail "Should have rejected expired token"
  | Error msg ->
      check bool "error mentions expired" true
        (String.lowercase_ascii msg |> fun s ->
         String.sub s 0 (min (String.length s) 7) = "expired" ||
         String.sub s 0 (min (String.length s) 5) = "token")

let test_jwt_wrong_secret () =
  let payload = `Assoc [] in
  let token = Kirin_auth.Jwt.encode ~secret:jwt_secret ~payload
    ~sub:"user-123" ~exp:(Unix.gettimeofday () +. 3600.) () in
  match Kirin_auth.Jwt.decode ~secret:"wrong-secret" token with
  | Ok _ -> fail "Should have rejected token with wrong secret"
  | Error _ -> ()

let test_jwt_custom_claims () =
  let payload = `Assoc [("role", `String "admin"); ("level", `Int 5)] in
  let token = Kirin_auth.Jwt.encode ~secret:jwt_secret ~payload
    ~sub:"user-123"
    ~exp:(Unix.gettimeofday () +. 3600.)
    () in
  match Kirin_auth.Jwt.decode ~secret:jwt_secret token with
  | Ok decoded ->
      (match Yojson.Safe.Util.member "role" decoded.payload with
       | `String "admin" -> ()
       | _ -> fail "role claim missing or wrong")
  | Error msg -> fail ("JWT decode failed: " ^ msg)

let jwt_tests = [
  test_case "encode" `Quick test_jwt_encode;
  test_case "decode" `Quick test_jwt_decode;
  test_case "expired" `Quick test_jwt_expired;
  test_case "wrong secret" `Quick test_jwt_wrong_secret;
  test_case "custom claims" `Quick test_jwt_custom_claims;
]

(** {1 Password Tests} *)

let test_password_hash () =
  let hash = Kirin_auth.Password.hash "my-password" in
  check bool "hash starts with $pbkdf2-sha256$" true
    (String.sub hash 0 14 = "$pbkdf2-sha256")

let test_password_verify () =
  let hash = Kirin_auth.Password.hash "my-password" in
  check bool "correct password verifies" true
    (Kirin_auth.Password.verify "my-password" hash);
  check bool "wrong password fails" false
    (Kirin_auth.Password.verify "wrong-password" hash)

let test_password_different_hashes () =
  let hash1 = Kirin_auth.Password.hash "same-password" in
  let hash2 = Kirin_auth.Password.hash "same-password" in
  check bool "same password produces different hashes (salt)" true
    (hash1 <> hash2);
  check bool "both verify correctly" true
    (Kirin_auth.Password.verify "same-password" hash1 &&
     Kirin_auth.Password.verify "same-password" hash2)

let test_password_strength () =
  check (testable (Fmt.of_to_string Kirin_auth.Password.strength_to_string) (=))
    "weak password" Kirin_auth.Password.Weak
    (Kirin_auth.Password.check_strength "abc");
  check (testable (Fmt.of_to_string Kirin_auth.Password.strength_to_string) (=))
    "strong password" Kirin_auth.Password.Strong
    (Kirin_auth.Password.check_strength "MyStr0ng!P@ss")

let password_tests = [
  test_case "hash" `Quick test_password_hash;
  test_case "verify" `Quick test_password_verify;
  test_case "different hashes" `Quick test_password_different_hashes;
  test_case "strength" `Quick test_password_strength;
]

(** {1 Session Tests} *)

let with_eio f () = Eio_main.run @@ fun _env -> f ()

let test_session_create () =
  let store = Kirin_auth.Session.create_memory_store () in
  let id = Kirin_auth.Session.create store () in
  check bool "session ID non-empty" true (String.length id > 0)

let test_session_set_get () =
  let store = Kirin_auth.Session.create_memory_store () in
  let id = Kirin_auth.Session.create store () in
  let _ = Kirin_auth.Session.set store id "user_id" "123" in
  check (option string) "get returns value" (Some "123")
    (Kirin_auth.Session.get store id "user_id")

let test_session_destroy () =
  let store = Kirin_auth.Session.create_memory_store () in
  let id = Kirin_auth.Session.create store () in
  let _ = Kirin_auth.Session.set store id "key" "value" in
  Kirin_auth.Session.destroy store id;
  check (option string) "destroyed session returns None" None
    (Kirin_auth.Session.get store id "key")

let session_tests = [
  test_case "create" `Quick (with_eio test_session_create);
  test_case "set/get" `Quick (with_eio test_session_set_get);
  test_case "destroy" `Quick (with_eio test_session_destroy);
]

(** {1 CSRF Tests} *)

let test_csrf_generate () =
  let token = Kirin_auth.Csrf.generate ~secret:"csrf-secret" ~session_id:"sess-123" () in
  check bool "token non-empty" true (String.length token > 0)

let test_csrf_validate () =
  let secret = "csrf-secret" in
  let session_id = "sess-123" in
  let token = Kirin_auth.Csrf.generate ~secret ~session_id () in
  match Kirin_auth.Csrf.validate_token ~secret ~session_id token with
  | Ok () -> ()
  | Error msg -> fail ("CSRF validation failed: " ^ msg)

let test_csrf_wrong_session () =
  let secret = "csrf-secret" in
  let token = Kirin_auth.Csrf.generate ~secret ~session_id:"sess-123" () in
  match Kirin_auth.Csrf.validate_token ~secret ~session_id:"sess-456" token with
  | Ok () -> fail "Should have rejected token from different session"
  | Error _ -> ()

let test_csrf_wrong_secret () =
  let token = Kirin_auth.Csrf.generate ~secret:"secret1" ~session_id:"sess" () in
  match Kirin_auth.Csrf.validate_token ~secret:"secret2" ~session_id:"sess" token with
  | Ok () -> fail "Should have rejected token with wrong secret"
  | Error _ -> ()

let csrf_tests = [
  test_case "generate" `Quick test_csrf_generate;
  test_case "validate" `Quick test_csrf_validate;
  test_case "wrong session" `Quick test_csrf_wrong_session;
  test_case "wrong secret" `Quick test_csrf_wrong_secret;
]

(** {1 OAuth2 Tests} *)

let test_oauth2_provider_google () =
  let p = Kirin_auth.Oauth2.Provider.google
    ~client_id:"test-id"
    ~client_secret:"test-secret"
    ~redirect_uri:"http://localhost/callback" in
  check string "name" "google" p.name;
  check bool "has userinfo_url" true (Option.is_some p.userinfo_url)

let test_oauth2_provider_github () =
  let p = Kirin_auth.Oauth2.Provider.github
    ~client_id:"test-id"
    ~client_secret:"test-secret"
    ~redirect_uri:"http://localhost/callback" in
  check string "name" "github" p.name

let test_oauth2_authorization_url () =
  let p = Kirin_auth.Oauth2.Provider.google
    ~client_id:"test-id"
    ~client_secret:"test-secret"
    ~redirect_uri:"http://localhost/callback" in
  let (url, state) = Kirin_auth.Oauth2.authorization_url p in
  check bool "URL contains accounts.google.com" true
    (String.length url > 30 && String.sub url 0 30 = "https://accounts.google.com/o/");
  check bool "state non-empty" true (String.length state > 0)

let test_oauth2_pkce () =
  let p = Kirin_auth.Oauth2.Provider.google
    ~client_id:"test-id"
    ~client_secret:"test-secret"
    ~redirect_uri:"http://localhost/callback" in
  let (url, state, verifier) = Kirin_auth.Oauth2.authorization_url_pkce p in
  check bool "URL contains code_challenge" true
    (String.length url > 0 &&
     try ignore (Str.search_forward (Str.regexp "code_challenge=") url 0); true
     with Not_found -> false);
  check bool "state non-empty" true (String.length state > 0);
  check bool "verifier non-empty" true (String.length verifier > 0)

(* OAuth state hardening regression tests.

   Two distinct defenses pinned individually so a future "simplification"
   trips a specific test instead of silently re-opening one of them:

   - verify_state must agree byte-for-byte with [=] on functional
     output (constant-time helper must remain a drop-in). Without a
     test that references the helper indirectly, a refactor could
     replace [constant_time_string_eq] with [=] and nothing here
     would notice until a timing-harness audit.

   - login_handler must default the Secure flag on. Production OAuth
     callbacks travel over HTTPS; the state cookie is the only CSRF
     guard, and a non-Secure cookie is readable by any active network
     attacker on the same path. *)

let oauth_make_request ?(headers=[]) path =
  let raw =
    Http.Request.make ~meth:`GET ~headers:(Http.Header.of_list headers) path
  in
  let body_source =
    Eio.Flow.string_source "" |> Eio.Buf_read.of_flow ~max_size:1024
  in
  Kirin.Request.make ~raw ~body_source

let oauth_make_response_with_state_cookie state =
  let req = oauth_make_request
    ~headers:[("cookie", "oauth_state=" ^ state)] "/cb" in
  req

let test_oauth_verify_state_match () =
  let req = oauth_make_response_with_state_cookie "abc123" in
  check bool "matching state passes" true
    (Kirin_auth.Oauth2.verify_state req "abc123")

let test_oauth_verify_state_mismatch () =
  let req = oauth_make_response_with_state_cookie "abc123" in
  check bool "different value rejected" false
    (Kirin_auth.Oauth2.verify_state req "abcxxx");
  check bool "shorter value rejected" false
    (Kirin_auth.Oauth2.verify_state req "abc");
  check bool "longer value rejected" false
    (Kirin_auth.Oauth2.verify_state req "abc1234")

let test_oauth_verify_state_no_cookie () =
  let req = oauth_make_request "/cb" in
  check bool "missing cookie rejected" false
    (Kirin_auth.Oauth2.verify_state req "anything")

let test_oauth_login_handler_secure_default () =
  (* The Secure attribute must be present on the state cookie unless
     the caller explicitly opts out. Inspect the raw Set-Cookie value. *)
  let p = Kirin_auth.Oauth2.Provider.google
    ~client_id:"id" ~client_secret:"sec"
    ~redirect_uri:"https://app/cb" in
  let handler = Kirin_auth.Oauth2.login_handler p () in
  let req = oauth_make_request "/login" in
  let resp = handler req in
  match Kirin.Response.header "set-cookie" resp with
  | None -> Alcotest.fail "expected set-cookie header"
  | Some v ->
    let contains needle =
      let nl = String.length needle in
      let vl = String.length v in
      let rec scan i =
        if i + nl > vl then false
        else if String.sub v i nl = needle then true
        else scan (i + 1)
      in
      scan 0
    in
    check bool "Secure flag present by default" true (contains "; Secure")

let test_oauth_login_handler_secure_opt_out () =
  (* Local-dev callers can pass ~secure:false. The flag must then be
     absent so the cookie works on plain HTTP. *)
  let p = Kirin_auth.Oauth2.Provider.google
    ~client_id:"id" ~client_secret:"sec"
    ~redirect_uri:"https://app/cb" in
  let handler = Kirin_auth.Oauth2.login_handler p ~secure:false () in
  let req = oauth_make_request "/login" in
  let resp = handler req in
  match Kirin.Response.header "set-cookie" resp with
  | None -> Alcotest.fail "expected set-cookie header"
  | Some v ->
    let contains needle =
      let nl = String.length needle in
      let vl = String.length v in
      let rec scan i =
        if i + nl > vl then false
        else if String.sub v i nl = needle then true
        else scan (i + 1)
      in
      scan 0
    in
    check bool "Secure flag absent when opted out" false
      (contains "; Secure")

let oauth2_tests = [
  test_case "provider google" `Quick test_oauth2_provider_google;
  test_case "provider github" `Quick test_oauth2_provider_github;
  test_case "authorization url" `Quick test_oauth2_authorization_url;
  test_case "pkce" `Quick test_oauth2_pkce;
  test_case "verify_state matches" `Quick test_oauth_verify_state_match;
  test_case "verify_state rejects mismatch / length diff" `Quick
    test_oauth_verify_state_mismatch;
  test_case "verify_state rejects when cookie missing" `Quick
    test_oauth_verify_state_no_cookie;
  test_case "login_handler sets Secure by default" `Quick
    test_oauth_login_handler_secure_default;
  test_case "login_handler honours ~secure:false" `Quick
    test_oauth_login_handler_secure_opt_out;
]

(** {1 Auth Middleware Tests (Hmap context, no global ref)} *)

let make_req ?(meth = `GET) ?(headers = []) path =
  let raw = Http.Request.make ~meth ~headers:(Http.Header.of_list headers) path in
  let body_source = Eio.Buf_read.of_flow ~max_size:1024 (Eio.Flow.string_source "") in
  Kirin.Request.make ~raw ~body_source

(** set_auth_info stores into Hmap, get_auth_info reads back *)
let test_middleware_set_get_auth_info () =
  let req = make_req "/" in
  check (option string) "no auth initially" None
    (Kirin_auth.Middleware.get_user_id req);
  let info : Kirin_auth.Middleware.auth_info =
    { user_id = "u-42"; claims = None; token_type = "test" }
  in
  let req2 = Kirin_auth.Middleware.set_auth_info req info in
  check (option string) "user_id present after set" (Some "u-42")
    (Kirin_auth.Middleware.get_user_id req2);
  (* Original request is unchanged (immutable Hmap) *)
  check (option string) "original req unaffected" None
    (Kirin_auth.Middleware.get_user_id req)

(** JWT middleware stores auth info in per-request context *)
let test_middleware_jwt_success () =
  let payload = `Assoc [("role", `String "admin")] in
  let token = Kirin_auth.Jwt.encode ~secret:jwt_secret ~payload
    ~sub:"user-99" ~exp:(Unix.gettimeofday () +. 3600.) () in
  let req = make_req ~headers:[("Authorization", "Bearer " ^ token)] "/api" in
  let captured_uid = ref None in
  let handler req =
    captured_uid := Kirin_auth.Middleware.get_user_id req;
    Kirin.Response.text "ok"
  in
  let protected = Kirin_auth.Middleware.jwt ~secret:jwt_secret handler in
  let _resp = protected req in
  check (option string) "handler sees user_id" (Some "user-99") !captured_uid

(** JWT middleware returns 401 on missing token *)
let test_middleware_jwt_missing_token () =
  let req = make_req "/api" in
  let handler _req = Kirin.Response.text "ok" in
  let protected = Kirin_auth.Middleware.jwt ~secret:jwt_secret handler in
  let resp = protected req in
  check int "401 on missing token" 401
    (Http.Status.to_int (Kirin.Response.status resp))

(** Two fibers with different auth info do not interfere.
    This is the scenario that the old global ref got wrong. *)
let test_middleware_concurrent_isolation () =
  Eio_main.run @@ fun _env ->
    let token_a = Kirin_auth.Jwt.encode ~secret:jwt_secret
      ~payload:(`Assoc []) ~sub:"alice" ~exp:(Unix.gettimeofday () +. 3600.) () in
    let token_b = Kirin_auth.Jwt.encode ~secret:jwt_secret
      ~payload:(`Assoc []) ~sub:"bob" ~exp:(Unix.gettimeofday () +. 3600.) () in
    let req_a = make_req ~headers:[("Authorization", "Bearer " ^ token_a)] "/" in
    let req_b = make_req ~headers:[("Authorization", "Bearer " ^ token_b)] "/" in
    let uid_a = ref None in
    let uid_b = ref None in
    let handler_a req =
      (* Yield to let fiber B run and set its own auth *)
      Eio.Fiber.yield ();
      uid_a := Kirin_auth.Middleware.get_user_id req;
      Kirin.Response.text "a"
    in
    let handler_b req =
      uid_b := Kirin_auth.Middleware.get_user_id req;
      Kirin.Response.text "b"
    in
    Eio.Fiber.both
      (fun () ->
        ignore (Kirin_auth.Middleware.jwt ~secret:jwt_secret handler_a req_a))
      (fun () ->
        ignore (Kirin_auth.Middleware.jwt ~secret:jwt_secret handler_b req_b));
    (* Each fiber must see its own user, never the other *)
    check (option string) "fiber A sees alice" (Some "alice") !uid_a;
    check (option string) "fiber B sees bob" (Some "bob") !uid_b

(** get_claims returns claims from Hmap context *)
let test_middleware_get_claims () =
  let payload = `Assoc [("role", `String "admin")] in
  let token = Kirin_auth.Jwt.encode ~secret:jwt_secret ~payload
    ~sub:"user-1" ~exp:(Unix.gettimeofday () +. 3600.) () in
  let req = make_req ~headers:[("Authorization", "Bearer " ^ token)] "/" in
  let captured_claims = ref None in
  let handler req =
    captured_claims := Kirin_auth.Middleware.get_claims req;
    Kirin.Response.text "ok"
  in
  ignore (Kirin_auth.Middleware.jwt ~secret:jwt_secret handler req);
  match !captured_claims with
  | None -> fail "claims should be present"
  | Some claims ->
    (match Yojson.Safe.Util.member "role" claims with
     | `String "admin" -> ()
     | _ -> fail "role claim missing")

(** API key middleware stores auth info in request context *)
let test_middleware_api_key () =
  let validate = function
    | "valid-key-123" -> Some "api-user-7"
    | _ -> None
  in
  let req = make_req ~headers:[("X-API-Key", "valid-key-123")] "/" in
  let captured = ref None in
  let handler req =
    captured := Kirin_auth.Middleware.get_auth_info req;
    Kirin.Response.text "ok"
  in
  ignore (Kirin_auth.Middleware.api_key ~validate handler req);
  match !captured with
  | None -> fail "auth_info should be set for valid API key"
  | Some info ->
    check string "user_id from api key" "api-user-7" info.user_id;
    check string "token_type" "api_key" info.token_type

let middleware_tests = [
  test_case "set/get auth info via Hmap" `Quick test_middleware_set_get_auth_info;
  test_case "jwt success" `Quick test_middleware_jwt_success;
  test_case "jwt missing token" `Quick test_middleware_jwt_missing_token;
  test_case "concurrent fiber isolation" `Quick test_middleware_concurrent_isolation;
  test_case "get_claims" `Quick test_middleware_get_claims;
  test_case "api_key" `Quick test_middleware_api_key;
]

(** {1 Auth Middleware Tests (any_of)} *)

let make_test_request ?(meth=`GET) ?(headers=[]) ?(body="") path =
  let raw = Http.Request.make ~meth ~headers:(Http.Header.of_list headers) path in
  let body_source = Eio.Flow.string_source body |> Eio.Buf_read.of_flow ~max_size:(max 1024 (String.length body + 1)) in
  Kirin.Request.make ~raw ~body_source

(** Count how many times the handler is invoked *)
let test_any_of_no_double_execution () =
  (* Create a valid JWT token *)
  let secret = "test-secret" in
  let payload = `Assoc [("role", `String "user")] in
  let token = Kirin_auth.Jwt.encode ~secret ~payload
    ~sub:"user-42" ~exp:(Unix.gettimeofday () +. 3600.) () in

  let call_count = ref 0 in
  let handler _req =
    incr call_count;
    (* Return a 404 -- a business logic error, NOT auth failure *)
    Kirin.Response.json ~status:`Not_found
      (`Assoc [("error", `String "User not found")])
  in

  let auth_jwt = Kirin_auth.Middleware.jwt ~secret in
  let protected = Kirin_auth.Middleware.any_of [auth_jwt] handler in

  let req = make_test_request ~headers:[("authorization", "Bearer " ^ token)] "/api/user" in
  let resp = protected req in

  (* Handler should be called exactly once *)
  check int "handler called once" 1 !call_count;
  (* Response should be the 404 from handler, not a 401 from auth *)
  check int "status is 404 (business logic)" 404
    (Http.Status.to_int (Kirin.Response.status resp))

(** When auth fails, handler should never be called *)
let test_any_of_no_execution_on_auth_failure () =
  let call_count = ref 0 in
  let handler _req =
    incr call_count;
    Kirin.Response.text "should not reach here"
  in

  let auth_jwt = Kirin_auth.Middleware.jwt ~secret:"test-secret" in
  let protected = Kirin_auth.Middleware.any_of [auth_jwt] handler in

  (* Request without auth token *)
  let req = make_test_request "/api/user" in
  let resp = protected req in

  check int "handler never called" 0 !call_count;
  check int "status is 401" 401
    (Http.Status.to_int (Kirin.Response.status resp))

(** First successful auth wins, handler called once *)
let test_any_of_first_success_wins () =
  let secret1 = "secret1" in
  let secret2 = "secret2" in
  let payload = `Assoc [] in
  let token2 = Kirin_auth.Jwt.encode ~secret:secret2 ~payload
    ~sub:"user-99" ~exp:(Unix.gettimeofday () +. 3600.) () in

  let call_count = ref 0 in
  let handler _req =
    incr call_count;
    let uid = Kirin_auth.Middleware.get_user_id _req in
    Kirin.Response.text (Option.value uid ~default:"none")
  in

  (* First strategy uses wrong secret, second matches *)
  let auth1 = Kirin_auth.Middleware.jwt ~secret:secret1 in
  let auth2 = Kirin_auth.Middleware.jwt ~secret:secret2 in
  let protected = Kirin_auth.Middleware.any_of [auth1; auth2] handler in

  let req = make_test_request ~headers:[("authorization", "Bearer " ^ token2)] "/api" in
  let resp = protected req in

  check int "handler called once" 1 !call_count;
  check int "status 200" 200 (Http.Status.to_int (Kirin.Response.status resp))

(** auth_info is available in handler after any_of succeeds *)
let test_any_of_preserves_auth_info () =
  let secret = "test-secret" in
  let payload = `Assoc [("admin", `Bool true)] in
  let token = Kirin_auth.Jwt.encode ~secret ~payload
    ~sub:"admin-1" ~exp:(Unix.gettimeofday () +. 3600.) () in

  let handler _req =
    let uid = Kirin_auth.Middleware.get_user_id _req in
    check (option string) "user_id set" (Some "admin-1") uid;
    Kirin.Response.text "ok"
  in

  let auth_jwt = Kirin_auth.Middleware.jwt ~secret in
  let protected = Kirin_auth.Middleware.any_of [auth_jwt] handler in

  let req = make_test_request ~headers:[("authorization", "Bearer " ^ token)] "/api" in
  let _resp = protected req in
  ()

let any_of_tests = [
  test_case "no double execution on 4xx" `Quick test_any_of_no_double_execution;
  test_case "no execution on auth failure" `Quick test_any_of_no_execution_on_auth_failure;
  test_case "first success wins" `Quick test_any_of_first_success_wins;
  test_case "preserves auth_info" `Quick test_any_of_preserves_auth_info;
]

(** {1 Main} *)

let () =
  run "Auth" [
    ("Jwt", jwt_tests);
    ("Password", password_tests);
    ("Session", session_tests);
    ("Csrf", csrf_tests);
    ("Oauth2", oauth2_tests);
    ("Middleware", middleware_tests);
    ("Any_of", any_of_tests);
  ]
