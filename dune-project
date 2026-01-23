(lang dune 3.16)

(name kirin)

(generate_opam_files true)

(source
 (github jeong-sik/kirin))

(authors "Seungji Park <yousleepwhen@gmail.com>")

(maintainers "Seungji Park <yousleepwhen@gmail.com>")

(license MIT)

(documentation https://github.com/jeong-sik/kirin)

(package
 (name kirin)
 (synopsis "OCaml 5.x Eio-native web framework with Dream-like DX")
 (description
  "Kirin is an OCaml 5.x web framework built on Eio for direct-style async.\nIt combines Dream's developer experience with Axum's architecture patterns.")
 (depends
  (ocaml (>= "5.1"))
  (dune (>= "3.16"))
  (eio (>= "1.0"))
  (eio_main (>= "1.0"))
  (cohttp-eio (>= "6.0"))
  (yojson (>= "2.0"))
  (uri (>= "4.0"))
  (mirage-crypto-rng (>= "1.0"))
  (base64 (>= "3.0"))
  (ppx_deriving (>= "5.0"))
  (ppx_deriving_yojson (>= "3.0"))
  (alcotest :with-test))
 (tags
  (web eio http framework)))
