(** Kirin Sync Example (Backend) *)

open Kirin.Sync

(* In-memory Database for demo *)
module MemDb = struct
  let data = Hashtbl.create 10
  let version = Atomic.make 0L
  let client_last_mutation = Hashtbl.create 10

  let process_push client_id mutations = 
    Printf.printf "Received push from %s: %d mutations\n" client_id (List.length mutations);
    List.iter (fun (m : mutation) ->
      match m.name with
      | "put" ->
        let key = Yojson.Safe.Util.(m.args |> member "key" |> to_string) in
        let value = Yojson.Safe.Util.(m.args |> member "value") in
        Hashtbl.replace data key value
      | "del" ->
        let key = Yojson.Safe.Util.(m.args |> member "key" |> to_string) in
        Hashtbl.remove data key
      | _ -> ()
    ) mutations;
    
    (* Update last mutation ID for client *)
    let last_m = List.fold_left (fun acc (m:mutation) -> max acc m.id) 0 mutations in
    Hashtbl.replace client_last_mutation client_id last_m;
    
    (* Increment server version *)
    let old_v = Atomic.get version in
    let new_v = Int64.add old_v 1L in
    Atomic.set version new_v;
    Ok new_v

  let process_pull client_id _from_version = 
    (* In real app, we would query DB for changes > from_version.
       For demo, we just return everything. *)
    let patches = Hashtbl.fold (fun k v acc ->
      { op = `Put; key = k; value = Some v } :: acc
    ) data [] in
    
    let last_m = Hashtbl.find_opt client_last_mutation client_id |> Option.value ~default:0 in
    let v = Atomic.get version in
    
    Ok { cookie = v; patch = patches; last_mutation_id = last_m }
end

module AppSync = Kirin.Sync.Make(MemDb)

let push_handler req =
  match Kirin.json_body req with
  | Error _ -> Kirin.bad_request ~body:"Invalid JSON" ()
  | Ok json ->
    let client_id = Yojson.Safe.Util.(json |> member "clientID" |> to_string) in
    let mutations_json = Yojson.Safe.Util.(json |> member "mutations" |> to_list) in
    
    let mutations = List.map (fun m ->
      let id = Yojson.Safe.Util.(m |> member "id" |> to_int) in
      let name = Yojson.Safe.Util.(m |> member "name" |> to_string) in
      let args = Yojson.Safe.Util.(m |> member "args") in
      { id; name; args }
    ) mutations_json in
    
    match AppSync.handle_push { client_id; mutations } with
    | Ok () -> Kirin.json (`Assoc [("ok", `Bool true)])
    | Error e -> Kirin.bad_request ~body:e ()
let pull_handler req =
  match Kirin.json_body req with
  | Error _ -> Kirin.bad_request ~body:"Invalid JSON" ()
  | Ok json ->
    let client_id = Yojson.Safe.Util.(json |> member "clientID" |> to_string) in
    let cookie = Yojson.Safe.Util.(json |> member "cookie" |> to_int) |> Int64.of_int in
    
    match AppSync.handle_pull client_id cookie with
    | Ok res ->
      let patch_json = List.map (fun p ->
        `Assoc [
          ("op", `String (match p.op with `Put -> "put" | `Del -> "del" | `Clear -> "clear"));
          ("key", `String p.key);
          ("value", Option.value p.value ~default:`Null)
        ]
      ) res.patch in
      
      Kirin.json (`Assoc [
        ("cookie", `Int (Int64.to_int res.cookie));
        ("lastMutationID", `Int res.last_mutation_id);
        ("patch", `List patch_json)
      ])
    | Error e -> Kirin.server_error ~body:e ()
let poke_handler req = 
  let client_id = Kirin.query "clientID" req in
  let version = Kirin.query "version" req |> int_of_string |> Int64.of_int in
  
  (* Long polling - wait for version to change *)
  let new_version = AppSync.handle_poke client_id version in
  Kirin.json (`Assoc [("version", `Int (Int64.to_int new_version))])

let routes = Kirin.router [
  Kirin.get "/" (fun _ -> Kirin.html "<h1>Kirin Sync Server</h1>");
  Kirin.post "/push" push_handler;
  Kirin.post "/pull" pull_handler;
  Kirin.get "/poke" poke_handler;
]

let () = Kirin.start ~port:9000
  @@ Kirin.logger
  @@ Kirin.cors ()
  @@ routes
