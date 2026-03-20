(** Fs_compat - Eio-native file I/O with blocking fallback.

    Provides Eio-native file operations when the global fs path is set,
    falling back to blocking Unix I/O when not available (e.g., in tests).

    {b Usage:}
    {[
      (* At startup, in Eio_main.run: *)
      Fs_compat.set_fs (Eio.Stdenv.fs env);

      (* Throughout the codebase: *)
      let content = Fs_compat.load "config.json" in
      Fs_compat.save "output.txt" content
    ]} *)

(** {1 Global Filesystem} *)

(** [set_fs fs] sets the global filesystem path. Call once at startup
    within [Eio_main.run]. *)
val set_fs : Eio.Fs.dir_ty Eio.Path.t -> unit

(** [get_fs ()] returns the current filesystem path, if set. *)
val get_fs : unit -> Eio.Fs.dir_ty Eio.Path.t option

(** {1 Directory Operations} *)

(** [mkdir_p path] creates a directory and all parent directories.
    Does nothing if the directory already exists.
    @raise Invalid_argument if [path] exists but is not a directory. *)
val mkdir_p : string -> unit

(** {1 File Operations} *)

(** [load path] loads entire file contents as a string (text mode).
    Uses Eio when fs is set, falls back to [In_channel]. *)
val load : string -> string

(** [load_binary path] loads entire file contents as a string (binary mode).
    Uses Eio when fs is set, falls back to [In_channel]. *)
val load_binary : string -> string

(** [save ?append path content] writes string content to a file.
    Creates parent directories if needed.
    @param append If [true], appends instead of overwriting (default: [false]). *)
val save : ?append:bool -> string -> string -> unit

(** {1 File Queries} *)

(** [file_exists path] returns [true] if the path exists
    (file or directory). *)
val file_exists : string -> bool

(** [is_directory path] returns [true] if the path is a directory. *)
val is_directory : string -> bool

(** [readdir path] lists the contents of a directory. *)
val readdir : string -> string list
