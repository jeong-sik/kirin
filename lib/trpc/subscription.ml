(** tRPC Subscriptions

    WebSocket-based subscriptions for real-time updates. *)

(** {1 Subscription Types} *)

(** Subscription state *)
type 'a state =
  | Active of 'a
  | Completed
  | Error of string

(** Subscription message *)
type message =
  | Data of Yojson.Safe.t
  | Complete
  | Err of { code: int; message: string }

(** Subscription ID *)
type sub_id = string

(** Generate unique subscription ID *)
let generate_id () =
  Printf.sprintf "sub_%d_%d" (Unix.getpid ()) (Random.int 1000000)

(** {1 Message Encoding} *)

(** Encode subscription message to JSON *)
let encode_message ~id msg =
  match msg with
  | Data result ->
    `Assoc [
      ("id", `String id);
      ("result", `Assoc [
        ("type", `String "data");
        ("data", result);
      ])
    ]
  | Complete ->
    `Assoc [
      ("id", `String id);
      ("result", `Assoc [
        ("type", `String "stopped");
      ])
    ]
  | Err { code; message } ->
    `Assoc [
      ("id", `String id);
      ("error", `Assoc [
        ("code", `Int code);
        ("message", `String message);
      ])
    ]

(** {1 Request Types} *)

(** Subscription request *)
type request = {
  id: sub_id;
  method_: string;  (* subscription.start, subscription.stop *)
  params: Yojson.Safe.t;
}

(** Parse subscription request *)
let parse_request json =
  match json with
  | `Assoc fields ->
    let id = match List.assoc_opt "id" fields with
      | Some (`String s) -> s
      | _ -> generate_id ()
    in
    let method_ = match List.assoc_opt "method" fields with
      | Some (`String s) -> s
      | _ -> ""
    in
    let params = match List.assoc_opt "params" fields with
      | Some p -> p
      | None -> `Null
    in
    Some { id; method_; params }
  | _ -> None

(** {1 Subscription Registry} *)

(** Active subscriptions *)
module Registry = struct
  type cancel_fn = unit -> unit

  type t = {
    mutable subscriptions: (sub_id * cancel_fn) list;
  }

  let create () = { subscriptions = [] }

  let add t ~id ~cancel =
    t.subscriptions <- (id, cancel) :: t.subscriptions

  let remove t id =
    match List.assoc_opt id t.subscriptions with
    | Some cancel ->
      cancel ();
      t.subscriptions <- List.filter (fun (i, _) -> i <> id) t.subscriptions;
      true
    | None -> false

  let cancel_all t =
    List.iter (fun (_, cancel) -> cancel ()) t.subscriptions;
    t.subscriptions <- []

  let count t = List.length t.subscriptions

  let has t id = List.mem_assoc id t.subscriptions
end

(** {1 SSE Helpers} *)

(** Format SSE event *)
let sse_event ~event ~data =
  Printf.sprintf "event: %s\ndata: %s\n\n" event data

(** Format SSE message *)
let sse_message msg =
  sse_event ~event:"message" ~data:(Yojson.Safe.to_string msg)

(** {1 WebSocket Protocol} *)

(** tRPC WebSocket message types *)
module WsMessage = struct
  type t =
    | Subscribe of { id: sub_id; path: string; input: Yojson.Safe.t }
    | Unsubscribe of { id: sub_id }
    | Ping
    | Pong

  let parse json =
    match json with
    | `Assoc fields ->
      let method_ = match List.assoc_opt "method" fields with
        | Some (`String s) -> s
        | _ -> ""
      in
      let id = match List.assoc_opt "id" fields with
        | Some (`String s) -> s
        | Some (`Int i) -> string_of_int i
        | _ -> ""
      in
      (match method_ with
       | "subscription.start" ->
         let params = List.assoc_opt "params" fields |> Option.value ~default:`Null in
         let path = match params with
           | `Assoc p -> (match List.assoc_opt "path" p with
               | Some (`String s) -> s
               | _ -> "")
           | _ -> ""
         in
         let input = match params with
           | `Assoc p -> List.assoc_opt "input" p |> Option.value ~default:`Null
           | _ -> `Null
         in
         Some (Subscribe { id; path; input })
       | "subscription.stop" ->
         Some (Unsubscribe { id })
       | "ping" -> Some Ping
       | "pong" -> Some Pong
       | _ -> None)
    | _ -> None

  let encode = function
    | Subscribe { id; path; input } ->
      `Assoc [
        ("id", `String id);
        ("method", `String "subscription.start");
        ("params", `Assoc [
          ("path", `String path);
          ("input", input);
        ])
      ]
    | Unsubscribe { id } ->
      `Assoc [
        ("id", `String id);
        ("method", `String "subscription.stop");
      ]
    | Ping -> `Assoc [("method", `String "ping")]
    | Pong -> `Assoc [("method", `String "pong")]
end

(** {1 Subscription Helpers} *)

(** Create a simple interval subscription *)
let interval ~ms ~f =
  let delay = float_of_int ms /. 1000.0 in
  let running = ref true in
  let cancel () = running := false in
  let rec loop () =
    if !running then begin
      f ();
      Unix.sleepf delay;
      loop ()
    end
  in
  (loop, cancel)

(** Create a subscription from a list *)
let of_list ~delay_ms items emit =
  let delay = float_of_int delay_ms /. 1000.0 in
  List.iter (fun item ->
    emit (Data item);
    Unix.sleepf delay
  ) items;
  emit Complete
