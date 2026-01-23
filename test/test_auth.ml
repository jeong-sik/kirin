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
  test_case "create" `Quick test_session_create;
  test_case "set/get" `Quick test_session_set_get;
  test_case "destroy" `Quick test_session_destroy;
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

let oauth2_tests = [
  test_case "provider google" `Quick test_oauth2_provider_google;
  test_case "provider github" `Quick test_oauth2_provider_github;
  test_case "authorization url" `Quick test_oauth2_authorization_url;
  test_case "pkce" `Quick test_oauth2_pkce;
]

(** {1 Main} *)

let () =
  run "Auth" [
    ("Jwt", jwt_tests);
    ("Password", password_tests);
    ("Session", session_tests);
    ("Csrf", csrf_tests);
    ("Oauth2", oauth2_tests);
  ]
