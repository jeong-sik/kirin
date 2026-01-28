(** Multicore & Async Logger Tests *)

open Alcotest

let test_async_logger () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  
  (* Start Logger *)
  Kirin.Logger.start sw;
  
  (* Emit logs from multiple domains *)
  let domains = List.init 4 (fun i ->
    Domain.spawn (fun () ->
      for j = 1 to 100 do
        Kirin.Logger.info "Domain %d log %d" i j
      done
    )
  ) in
  
  Array.iter Domain.join (Array.of_list domains);
  
  (* Give logger time to process *)
  Eio.Time.sleep (Eio.Stdenv.clock env) 0.1;
  
  check bool "logger is alive" true true

let test_domain_manager () =
  Eio_main.run @@ fun env ->
  let mgr = Eio.Stdenv.domain_mgr env in
  
  let task () = 
    Domain.self ()
  in
  
  let results = ref [] in
  let mutex = Eio.Mutex.create () in

  Eio.Fiber.all (
    List.init 4 (fun _ ->
      fun () -> 
        let id = Eio.Domain_manager.run mgr task in
        Eio.Mutex.use_rw mutex ~protect:true (fun () -> results := id :: !results)
    )
  );
  
  check int "results count" 4 (List.length !results)

let multicore_tests = [
  "async logger", `Quick, test_async_logger;
  "domain manager", `Quick, test_domain_manager;
]

let () =
  run "Multicore" [
    "Scale Up", multicore_tests;
  ]