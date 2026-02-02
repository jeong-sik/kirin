(** Fs_compat - Eio-native file I/O with blocking fallback

    This module provides Eio-native file operations when the global fs path
    is set, falling back to blocking Unix I/O when not available (e.g., in tests).

    Usage:
    1. At application startup (in Eio_main.run):
       {[
         Fs_compat.set_fs (Eio.Stdenv.fs env)
       ]}

    2. Throughout the codebase:
       {[
         let content = Fs_compat.load "config.json" in
         Fs_compat.save "output.txt" content
       ]}
*)

(** Global filesystem path - set once at startup *)
let global_fs : Eio.Fs.dir_ty Eio.Path.t option ref = ref None

(** Set the global filesystem path. Call once at startup. *)
let set_fs fs = global_fs := Some fs

(** Get the current filesystem path, if set *)
let get_fs () = !global_fs

(** Load entire file contents as string.
    Uses Eio.Path.load when fs is set, falls back to In_channel. *)
let load path =
  match !global_fs with
  | Some fs ->
    let p = Eio.Path.(fs / path) in
    Eio.Path.load p
  | None ->
    In_channel.(with_open_text path input_all)

(** Load entire file contents as bytes (binary mode).
    Uses Eio.Path.load when fs is set, falls back to In_channel. *)
let load_binary path =
  match !global_fs with
  | Some fs ->
    let p = Eio.Path.(fs / path) in
    Eio.Path.load p
  | None ->
    In_channel.(with_open_bin path input_all)

(** Save string content to file (creates parent dirs if needed).
    Uses Eio.Path.save when fs is set, falls back to Out_channel. *)
let save ?(append=false) path content =
  match !global_fs with
  | Some fs ->
    let p = Eio.Path.(fs / path) in
    let create = `Or_truncate 0o644 in
    Eio.Path.save ~append ~create p content
  | None ->
    let flags = if append then [Open_append; Open_creat] else [Open_wronly; Open_creat; Open_trunc] in
    Out_channel.(with_open_gen flags 0o644 path (fun oc -> output_string oc content))

(** Check if a file exists.
    Uses Eio.Path when fs is set, falls back to Sys.file_exists. *)
let file_exists path =
  match !global_fs with
  | Some fs ->
    let p = Eio.Path.(fs / path) in
    (match Eio.Path.kind ~follow:true p with
     | `Regular_file | `Directory -> true
     | _ -> false
     | exception Eio.Io _ -> false)
  | None ->
    Sys.file_exists path

(** Check if path is a directory.
    Uses Eio.Path when fs is set, falls back to Sys.is_directory. *)
let is_directory path =
  match !global_fs with
  | Some fs ->
    let p = Eio.Path.(fs / path) in
    (match Eio.Path.kind ~follow:true p with
     | `Directory -> true
     | _ -> false
     | exception Eio.Io _ -> false)
  | None ->
    Sys.is_directory path

(** List directory contents.
    Uses Eio.Path when fs is set, falls back to Sys.readdir. *)
let readdir path =
  match !global_fs with
  | Some fs ->
    let p = Eio.Path.(fs / path) in
    Eio.Path.read_dir p
  | None ->
    Array.to_list (Sys.readdir path)
