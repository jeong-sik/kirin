let string_contains hay needle =
  let hlen = String.length hay in
  let nlen = String.length needle in
  if nlen = 0 then true
  else if nlen > hlen then false
  else
    let rec loop i =
      if i + nlen > hlen then false
      else if String.sub hay i nlen = needle then true
      else loop (i + 1)
    in
    loop 0

let () =
  if Array.length Sys.argv <> 2 then (
    prerr_endline "usage: test_no_shell_node_worker <path-to-node_worker.ml>";
    exit 2
  );

  let src_path = Sys.argv.(1) in
  let src = In_channel.(with_open_text src_path input_all) in

  (* Forbid shell-based popen usage (uses /bin/sh -c). *)
  let forbidden = [ "Unix.open_process "; "open_process_in"; "Unix.open_process_in" ] in
  match List.find_opt (fun needle -> string_contains src needle) forbidden with
  | None -> ()
  | Some needle ->
      Printf.eprintf "forbidden shell-based API found in node_worker.ml: %s\n" needle;
      exit 1

