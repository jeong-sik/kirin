(** Kirin DB Events - PostgreSQL LISTEN/NOTIFY Integration

    Turns PostgreSQL into a realtime message bus.
    
    {b Features:}
    - Asynchronous LISTEN loop
    - JSON payload support
    - Automatic reconnection
    - Integration with Eio streams
*)

(** Notification type *)
type notification = {
  channel : string;
  payload : string;
  process_id : int;
}

(** Connection configuration *)
type config = {
  uri : string;
  channels : string list;
  reconnect_delay : float;
}

let default_config uri = {
  uri;
  channels = ["kirin_events"];
  reconnect_delay = 1.0;
}

module type DB_DRIVER = sig
  type connection
  val connect : Uri.t -> (connection, [> Caqti_error.connect]) result
  val listen : connection -> string list -> (unit, [> Caqti_error.call_or_retrieve]) result
  val wait_for_notification : connection -> (notification option, [> Caqti_error.call_or_retrieve]) result
  val disconnect : connection -> unit
end

(* Note: Actual Caqti integration requires low-level access or a dedicated driver.
   For this implementation, we simulate the behavior using a placeholder 
   since Caqti's high-level API focuses on request/response. 
   
   In a real implementation, we would use Caqti_driver_postgresql directly
   or a raw PQ connection via Eio.
*)

(** Event Stream *)
type t = {
  stream : notification Eio.Stream.t;
  sw : Eio.Switch.t;
}

(** Create a listener *)
let create ~sw ~env ~config =
  let stream = Eio.Stream.create 1024 in
  let clock = Eio.Stdenv.clock env in
  
  (* Mock listener loop for demonstration - replace with real Caqti loop *)
  let rec listen_loop () =
    try
      (* Simulate waiting for DB notification *)
      Eio.Time.sleep clock 5.0;
      
      let mock_notify = {
        channel = List.hd config.channels;
        payload = Printf.sprintf "{\"time\": %f}" (Unix.gettimeofday ());
        process_id = 1234;
      } in
      
      Eio.Stream.add stream mock_notify;
      listen_loop ()
    with _ -> 
      Eio.Time.sleep clock config.reconnect_delay;
      listen_loop ()
  in

  Eio.Fiber.fork ~sw listen_loop;
  { stream; sw }
(** Subscribe to notifications *)
let subscribe t = t.stream

(** Publish a notification (NOTIFY) *)
let notify pool channel payload =
  let open Caqti_request.Infix in
  let query = (Caqti_type.(t2 string string) ->. Caqti_type.unit) "SELECT pg_notify(?, ?)" in
  
  Db.use pool (fun (module C : Db.CONNECTION) ->
    C.exec query (channel, payload)
  )
