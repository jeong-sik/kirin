# Dream → Kirin Migration Guide

Dream에서 Kirin으로 마이그레이션하는 가이드입니다.

## 왜 Kirin?

| 특징 | Dream | Kirin |
|------|-------|-------|
| **Async 모델** | Lwt (모나딕) | Eio (직접 스타일) |
| **OCaml 버전** | 4.x / 5.x | 5.x only |
| **코드 스타일** | `let%lwt`, `>>=` | 일반 함수 호출 |
| **멀티코어** | 제한적 | Domain 기반 병렬 |

## 설치

```bash
# Dream 제거 (선택)
opam remove dream

# Kirin 설치
opam pin add kirin git+https://github.com/jeong-sik/kirin.git
```

## dune 파일 변경

```diff
(executable
 (name main)
- (libraries dream)
+ (libraries kirin)
)
```

## 기본 서버

### Dream
```ocaml
let () =
  Dream.run
  @@ Dream.logger
  @@ Dream.router [
    Dream.get "/" (fun _ -> Dream.html "Hello!");
  ]
```

### Kirin
```ocaml
let () =
  Kirin.start ~port:8080
  @@ Kirin.logger
  @@ Kirin.router [
    Kirin.get "/" (fun _ -> Kirin.html "Hello!");
  ]
```

**차이점:** `Dream.run` → `Kirin.start ~port:8080`

## 라우팅

### Dream
```ocaml
Dream.router [
  Dream.get "/users/:id" (fun req ->
    let id = Dream.param req "id" in
    Dream.html ("User: " ^ id));

  Dream.post "/users" (fun req ->
    let%lwt body = Dream.body req in
    Dream.json body);
]
```

### Kirin
```ocaml
Kirin.router [
  Kirin.get "/users/:id" (fun req ->
    let id = Kirin.param "id" req in  (* 순서 다름! *)
    Kirin.html ("User: " ^ id));

  Kirin.post "/users" (fun req ->
    let body = Kirin.Request.body req in  (* let%lwt 불필요 *)
    Kirin.json (`String body));
]
```

**차이점:**
- `Dream.param req "id"` → `Kirin.param "id" req` (인자 순서)
- `let%lwt body = ...` → `let body = ...` (Lwt 불필요)

## JSON 응답

### Dream
```ocaml
Dream.json {|{"status": "ok"}|}

(* 또는 yojson 사용 시 *)
Dream.json (Yojson.Safe.to_string json)
```

### Kirin
```ocaml
Kirin.json (`Assoc [("status", `String "ok")])

(* Yojson.Safe.t를 직접 전달 *)
```

**차이점:** Kirin은 `Yojson.Safe.t`를 직접 받음

## Request 접근

### Dream
```ocaml
let%lwt body = Dream.body req in
let headers = Dream.all_headers req in
let method_ = Dream.method_ req in
let path = Dream.target req in
let query = Dream.query req "key" in
```

### Kirin
```ocaml
let body = Kirin.Request.body req in       (* 동기 *)
let headers = Kirin.Request.headers req in
let method_ = Kirin.Request.meth req in
let path = Kirin.Request.path req in
let query = Kirin.Request.query "key" req in
```

## Response 생성

### Dream
```ocaml
Dream.respond "text"
Dream.html "<h1>Hi</h1>"
Dream.json "{}"
Dream.redirect "/login"
Dream.empty `Not_Found
```

### Kirin
```ocaml
Kirin.text "text"
Kirin.html "<h1>Hi</h1>"
Kirin.json (`Assoc [])
Kirin.redirect "/login"
Kirin.empty ~status:`Not_found ()
```

## 미들웨어

### Dream
```ocaml
let my_middleware inner_handler req =
  Dream.log "Request: %s" (Dream.target req);
  inner_handler req

let () =
  Dream.run
  @@ my_middleware
  @@ Dream.router [...]
```

### Kirin
```ocaml
let my_middleware inner_handler req =
  Printf.printf "Request: %s\n" (Kirin.Request.path req);
  inner_handler req

let () =
  Kirin.start ~port:8080
  @@ my_middleware
  @@ Kirin.router [...]
```

**차이점:** 거의 동일! `@@` 체이닝 그대로 사용

## 쿠키

### Dream
```ocaml
(* 읽기 *)
let session = Dream.cookie req "session" in

(* 쓰기 *)
Dream.set_cookie response req "session" "value"
```

### Kirin
```ocaml
(* 읽기 *)
let session = Kirin.Cookie.get req "session" in

(* 쓰기 - Response에 헤더로 추가 *)
Kirin.html ~headers:[Kirin.Cookie.set "session" "value"] "OK"
```

## WebSocket

### Dream
```ocaml
Dream.websocket (fun ws ->
  let%lwt msg = Dream.receive ws in
  match msg with
  | Some text -> Dream.send ws ("Echo: " ^ text)
  | None -> Lwt.return_unit)
```

### Kirin
```ocaml
Kirin.websocket (fun ws ->
  match Kirin.WebSocket.receive ws with
  | Some text -> Kirin.WebSocket.send ws ("Echo: " ^ text)
  | None -> ())
```

**차이점:** `let%lwt` 불필요, 직접 스타일

## Static 파일

### Dream
```ocaml
Dream.router [
  Dream.get "/static/**" (Dream.static "public/");
]
```

### Kirin
```ocaml
Kirin.router [
  Kirin.get "/static/*" (Kirin.static ~dir:"public/" ());
]
```

## 폼 처리

### Dream
```ocaml
Dream.post "/submit" (fun req ->
  let%lwt form = Dream.form req in
  match form with
  | `Ok fields ->
    let name = List.assoc "name" fields in
    Dream.html ("Hello " ^ name)
  | _ -> Dream.empty `Bad_Request)
```

### Kirin
```ocaml
Kirin.post "/submit" (fun req ->
  let form = Kirin.Request.form req in
  match Kirin.Form.field "name" form with
  | Some name -> Kirin.html ("Hello " ^ name)
  | None -> Kirin.empty ~status:`Bad_request ())
```

## CSRF 보호

### Dream
```ocaml
Dream.router [
  Dream.post "/submit" (fun req ->
    match%lwt Dream.form ~csrf:true req with
    | `Ok form -> ...
    | `Expired _ | `Wrong_session | `Invalid_token _ ->
        Dream.empty `Forbidden)
]
```

### Kirin
```ocaml
(* kirin.auth 모듈 사용 *)
Kirin.router [
  Kirin.post "/submit"
    (Kirin_auth.Csrf.protect (fun req ->
      let form = Kirin.Request.form req in
      ...));
]
```

## SQL (Dream + Caqti)

Dream과 Caqti 조합은 Lwt 기반이라 직접 호환 안 됨.
Kirin에서는 Eio 기반 DB 라이브러리 사용:

```ocaml
(* Kirin은 직접 DB 모듈 제공하지 않음 *)
(* eio-postgres 또는 직접 구현 필요 *)
```

## 마이그레이션 체크리스트

- [ ] `dream` → `kirin` 의존성 변경
- [ ] `Dream.run` → `Kirin.start ~port:N`
- [ ] `Dream.param req "x"` → `Kirin.param "x" req`
- [ ] `let%lwt` 제거 (전부!)
- [ ] `Lwt.return` → 그냥 값 반환
- [ ] `>>=`, `>|=` 연산자 제거
- [ ] JSON 응답을 `Yojson.Safe.t`로 변경
- [ ] 쿠키 API 변경
- [ ] WebSocket API 변경
- [ ] DB 레이어 재검토 (Lwt → Eio)

## 점진적 마이그레이션

한번에 전환이 어려우면:

1. **새 엔드포인트만 Kirin으로** - 기존 Dream 서버와 별도 포트
2. **Nginx 리버스 프록시**로 라우팅 분기
3. **점진적으로 이전** 후 Dream 서버 종료

## 도움이 필요하면

- GitHub Issues: https://github.com/jeong-sik/kirin/issues
- Examples: `examples/` 디렉토리 참고
