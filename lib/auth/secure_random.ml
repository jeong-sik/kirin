(** Cryptographically secure randomness helpers for auth flows. *)

let random_bytes len =
  if len < 0 then invalid_arg "Secure_random.random_bytes: negative length";
  let buf = Bytes.create len in
  if len = 0 then buf
  else
    let fd = Unix.openfile "/dev/urandom" [Unix.O_RDONLY] 0 in
    Fun.protect
      ~finally:(fun () -> Unix.close fd)
      (fun () ->
        let rec loop off =
          if off < len then
            let n = Unix.read fd buf off (len - off) in
            if n <= 0 then failwith "Secure_random.random_bytes: short read from /dev/urandom"
            else loop (off + n)
        in
        loop 0;
        buf)

let random_string len = Bytes.unsafe_to_string (random_bytes len)

let random_base64url len =
  Base64.encode_string ~pad:false ~alphabet:Base64.uri_safe_alphabet (random_string len)
