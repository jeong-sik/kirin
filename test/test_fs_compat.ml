(** Fs_compat tests *)

open Kirin

let test_save_creates_parent_dirs =
  "save creates parent dirs", `Quick, (fun () ->
    Random.self_init ();

    let root =
      Filename.concat
        (Filename.get_temp_dir_name ())
        (Printf.sprintf "kirin_fs_compat_%d_%d" (Unix.getpid ()) (Random.bits ()))
    in
    Unix.mkdir root 0o700;

    let file_path = Filename.concat root "a/b/c.txt" in

    Fun.protect
      ~finally:(fun () ->
        (* Best-effort cleanup in reverse order. *)
        (try Sys.remove file_path with _ -> ());
        (try Unix.rmdir (Filename.concat root "a/b") with _ -> ());
        (try Unix.rmdir (Filename.concat root "a") with _ -> ());
        (try Unix.rmdir root with _ -> ()))
      (fun () ->
        Fs_compat.save file_path "hello";
        Alcotest.(check bool) "file exists" true (Sys.file_exists file_path);
        Alcotest.(check string) "content" "hello" (Fs_compat.load file_path))
  )

let () =
  Alcotest.run "Fs_compat" [
    ("save", [test_save_creates_parent_dirs]);
  ]

