(** Password Hashing

    Secure password hashing using PBKDF2-SHA256.
    Includes timing-safe comparison to prevent timing attacks.

    {b Example:}
    {[
      let hash = Password.hash "my-password" in
      if Password.verify "my-password" hash then
        (* authenticated *)
    ]}
*)

(** {1 Configuration} *)

(** Default iterations for PBKDF2 (OWASP 2023 recommendation: 600,000) *)
let default_iterations = 600_000

(** Salt length in bytes *)
let salt_length = 32

(** Hash output length in bytes *)
let hash_length = 32

(** {1 Helpers} *)

(** Generate random salt using SHA256 of timestamp + random *)
let generate_salt () =
  let now = Unix.gettimeofday () in
  let rand = Random.int 1_000_000_000 in
  let entropy = Printf.sprintf "salt-%.6f-%d-%d" now rand (Unix.getpid ()) in
  let hash = Digestif.SHA256.digest_string entropy in
  String.sub (Digestif.SHA256.to_raw_string hash) 0 salt_length

(** Bytes to hex string *)
let bytes_to_hex bytes =
  let hex = Bytes.create (String.length bytes * 2) in
  String.iteri (fun i c ->
    let byte = Char.code c in
    let hi = byte lsr 4 in
    let lo = byte land 0x0f in
    let to_hex n = if n < 10 then Char.chr (n + 48) else Char.chr (n + 87) in
    Bytes.set hex (i * 2) (to_hex hi);
    Bytes.set hex (i * 2 + 1) (to_hex lo)
  ) bytes;
  Bytes.to_string hex

(** Hex string to bytes *)
let hex_to_bytes hex =
  let len = String.length hex / 2 in
  let bytes = Bytes.create len in
  for i = 0 to len - 1 do
    let hi = String.get hex (i * 2) in
    let lo = String.get hex (i * 2 + 1) in
    let to_int c =
      let code = Char.code c in
      if code >= 48 && code <= 57 then code - 48
      else if code >= 97 && code <= 102 then code - 87
      else if code >= 65 && code <= 70 then code - 55
      else failwith "Invalid hex character"
    in
    Bytes.set bytes i (Char.chr ((to_int hi lsl 4) lor to_int lo))
  done;
  Bytes.to_string bytes

(** PBKDF2-HMAC-SHA256 *)
let pbkdf2 ~password ~salt ~iterations ~dk_len =
  Digestif.SHA256.hmac_string ~key:password salt
  |> fun init_u ->
  let rec iterate u acc count =
    if count >= iterations then acc
    else
      let next_u = Digestif.SHA256.hmac_string ~key:password (Digestif.SHA256.to_raw_string u) in
      let acc_raw = Digestif.SHA256.to_raw_string acc in
      let u_raw = Digestif.SHA256.to_raw_string next_u in
      (* XOR accumulator with current U *)
      let xored = Bytes.create (String.length acc_raw) in
      String.iteri (fun i c ->
        Bytes.set xored i (Char.chr (Char.code c lxor Char.code (String.get u_raw i)))
      ) acc_raw;
      iterate next_u (Digestif.SHA256.of_raw_string (Bytes.to_string xored)) (count + 1)
  in
  let result = iterate init_u init_u 1 in
  String.sub (Digestif.SHA256.to_raw_string result) 0 dk_len

(** Timing-safe string comparison *)
let constant_time_compare a b =
  let len_a = String.length a in
  let len_b = String.length b in
  let result = ref (len_a lxor len_b) in
  for i = 0 to min len_a len_b - 1 do
    result := !result lor (Char.code (String.get a i) lxor Char.code (String.get b i))
  done;
  !result = 0

(** {1 Main API} *)

(** Hash format: $pbkdf2-sha256$iterations$salt$hash *)
type hash_result = string

(** Hash a password *)
let hash ?(iterations = default_iterations) password =
  let salt = generate_salt () in
  let derived = pbkdf2 ~password ~salt ~iterations ~dk_len:hash_length in
  Printf.sprintf "$pbkdf2-sha256$%d$%s$%s"
    iterations
    (bytes_to_hex salt)
    (bytes_to_hex derived)

(** Verify a password against a hash *)
let verify password hash_str =
  match String.split_on_char '$' hash_str with
  | [""; "pbkdf2-sha256"; iter_str; salt_hex; hash_hex] ->
      (try
        let iterations = int_of_string iter_str in
        let salt = hex_to_bytes salt_hex in
        let expected_hash = hex_to_bytes hash_hex in
        let derived = pbkdf2 ~password ~salt ~iterations ~dk_len:hash_length in
        constant_time_compare derived expected_hash
      with _ -> false)
  | _ -> false

(** Check if hash needs rehashing (iterations changed) *)
let needs_rehash ?(iterations = default_iterations) hash_str =
  match String.split_on_char '$' hash_str with
  | [""; "pbkdf2-sha256"; iter_str; _; _] ->
      (try int_of_string iter_str < iterations
       with _ -> true)
  | _ -> true

(** {1 Strength Checking} *)

(** Password strength level *)
type strength = Weak | Fair | Good | Strong

(** Check password strength *)
let check_strength password =
  let len = String.length password in
  let has_lower = String.exists (fun c -> c >= 'a' && c <= 'z') password in
  let has_upper = String.exists (fun c -> c >= 'A' && c <= 'Z') password in
  let has_digit = String.exists (fun c -> c >= '0' && c <= '9') password in
  let has_special = String.exists (fun c ->
    not (c >= 'a' && c <= 'z') &&
    not (c >= 'A' && c <= 'Z') &&
    not (c >= '0' && c <= '9')
  ) password in

  let score =
    (if len >= 8 then 1 else 0) +
    (if len >= 12 then 1 else 0) +
    (if has_lower then 1 else 0) +
    (if has_upper then 1 else 0) +
    (if has_digit then 1 else 0) +
    (if has_special then 1 else 0)
  in

  match score with
  | 0 | 1 | 2 -> Weak
  | 3 -> Fair
  | 4 | 5 -> Good
  | _ -> Strong

(** Get strength as string *)
let strength_to_string = function
  | Weak -> "weak"
  | Fair -> "fair"
  | Good -> "good"
  | Strong -> "strong"
