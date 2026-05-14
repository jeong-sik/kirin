(** Session Management

    Server-side session storage with cookie-based identification.
    Supports in-memory storage (default) and custom backends.

    {b Example:}
    {[
      let store = Session.create_memory_store () in

      (* In handler *)
      let session_id = Session.get_id req in
      Session.set store session_id "user_id" "123";
      let user_id = Session.get store session_id "user_id"
    ]}
*)

(** {1 Types} *)

(** Session data *)
type data = (string, string) Hashtbl.t

(** Session entry with metadata *)
type entry = {
  data : data;
  created_at : float;
  mutable last_accessed : float;
  mutable expires_at : float option;
}

(** Session store interface *)
type store = {
  get : string -> entry option;
  set : string -> entry -> unit;
  delete : string -> unit;
  cleanup : unit -> unit;
}

(** {1 Session ID} *)

(** Convert bytes to hex string *)
let bytes_to_hex s =
  let hex = Bytes.create (String.length s * 2) in
  String.iteri (fun i c ->
    let byte = Char.code c in
    let hi = byte lsr 4 in
    let lo = byte land 0x0f in
    let to_hex n = if n < 10 then Char.chr (n + 48) else Char.chr (n + 87) in
    Bytes.set hex (i * 2) (to_hex hi);
    Bytes.set hex (i * 2 + 1) (to_hex lo)
  ) s;
  Bytes.to_string hex

(** Generate secure random session ID from CSPRNG bytes *)
let generate_id () =
  bytes_to_hex (Secure_random.random_string 32)

(** Default session cookie name *)
let default_cookie_name = "kirin_session"

(** {1 Memory Store} *)

(** Create in-memory session store *)
let create_memory_store ?(max_age = 86400.0) () =
  let sessions : (string, entry) Hashtbl.t = Hashtbl.create 256 in
  let mutex = Eio.Mutex.create () in

  let with_lock f =
    Eio.Mutex.use_rw ~protect:true mutex f
  in

  let get id =
    with_lock (fun () ->
      match Hashtbl.find_opt sessions id with
      | None -> None
      | Some entry ->
          let now = Kirin.Time_compat.now () in
          (* Check expiration *)
          match entry.expires_at with
          | Some exp when exp < now ->
              Hashtbl.remove sessions id;
              None
          | _ ->
              entry.last_accessed <- now;
              Some entry)
  in

  let set id entry =
    with_lock (fun () -> Hashtbl.replace sessions id entry)
  in

  let delete id =
    with_lock (fun () -> Hashtbl.remove sessions id)
  in

  let cleanup () =
    with_lock (fun () ->
      let now = Kirin.Time_compat.now () in
      let to_remove = ref [] in
      Hashtbl.iter (fun id entry ->
        match entry.expires_at with
        | Some exp when exp < now -> to_remove := id :: !to_remove
        | _ ->
            (* Also remove if not accessed for max_age *)
            if now -. entry.last_accessed > max_age then
              to_remove := id :: !to_remove
      ) sessions;
      List.iter (Hashtbl.remove sessions) !to_remove)
  in

  { get; set; delete; cleanup }

(** {1 Session Operations} *)

(** Create new session *)
let create store ?expires_in () =
  let id = generate_id () in
  let now = Kirin.Time_compat.now () in
  let expires_at = Option.map (fun d -> now +. d) expires_in in
  let entry = {
    data = Hashtbl.create 16;
    created_at = now;
    last_accessed = now;
    expires_at;
  } in
  store.set id entry;
  id

(** Get session entry *)
let get_entry store id = store.get id

(** Set session value *)
let set store id key value =
  match store.get id with
  | None -> false
  | Some entry ->
      Hashtbl.replace entry.data key value;
      store.set id entry;
      true

(** Get session value *)
let get store id key =
  match store.get id with
  | None -> None
  | Some entry -> Hashtbl.find_opt entry.data key

(** Get all session data *)
let get_all store id =
  match store.get id with
  | None -> []
  | Some entry ->
      Hashtbl.fold (fun k v acc -> (k, v) :: acc) entry.data []

(** Remove session value *)
let remove store id key =
  match store.get id with
  | None -> false
  | Some entry ->
      Hashtbl.remove entry.data key;
      store.set id entry;
      true

(** Delete entire session *)
let destroy store id = store.delete id

(** Regenerate session ID (security best practice after login) *)
let regenerate store old_id =
  match store.get old_id with
  | None -> None
  | Some entry ->
      let new_id = generate_id () in
      store.delete old_id;
      store.set new_id entry;
      Some new_id

(** {1 Request Integration} *)

(** Get session ID from request cookie *)
let get_id_from_request ?(cookie_name = default_cookie_name) req =
  Kirin.Cookie.get cookie_name req

(** Set session cookie on response *)
let set_session_cookie ?(cookie_name = default_cookie_name) ?(path = "/")
    ?(http_only = true) ?(secure = true) ?(same_site = `Strict)
    ?max_age session_id resp =
  let attrs = Kirin.Cookie.{
    default_attributes with
    path = Some path;
    http_only;
    secure;
    same_site = Some same_site;
    max_age;
  } in
  Kirin.Cookie.set ~attrs cookie_name session_id resp

(** Clear session cookie *)
let clear_session_cookie ?(cookie_name = default_cookie_name) ?(path = "/") resp =
  let attrs = Kirin.Cookie.{
    default_attributes with
    path = Some path;
    max_age = Some 0;
  } in
  Kirin.Cookie.set ~attrs cookie_name "" resp

(** {1 Middleware} *)

(** Session middleware - ensures session exists.

    The set-cookie decision used to be gated on whether the *cookie*
    was present on the request, not on whether a *new session* was
    created. That left a server-side memory leak: when the cookie was
    present but its session_id had expired (or been evicted by
    [cleanup]), [store.get] returned None, the middleware fell
    through to [create store ()], and a fresh session was created —
    but the response arm saw "cookie was present" and did *not* call
    [set_session_cookie]. The client kept walking around with the
    dead ID, every subsequent request minted yet another fresh
    session, and the in-memory store grew unboundedly. An attacker
    holding any expired ID could pin server memory at request rate.

    Track [is_new] explicitly so the cookie refresh follows the
    "session was just created" event, not the "cookie was missing"
    event. The two conditions used to coincide before expiry was
    possible. *)
let middleware ?(cookie_name = default_cookie_name) ?(create_if_missing = true)
    ?(secure = true) store =
  fun handler req ->
    let cookie_id = get_id_from_request ~cookie_name req in
    let existing_id =
      match cookie_id with
      | Some id when Option.is_some (store.get id) -> Some id
      | _ -> None
    in
    let session_id, is_new =
      match existing_id with
      | Some id -> id, false
      | None when create_if_missing -> create store (), true
      | None -> "", false
    in
    if session_id = "" then
      handler req
    else
      let resp = handler req in
      if is_new then
        set_session_cookie ~cookie_name ~secure session_id resp
      else
        resp

(** Get session ID from request cookie *)
let get_id ?(cookie_name = default_cookie_name) req =
  get_id_from_request ~cookie_name req
