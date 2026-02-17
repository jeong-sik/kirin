(** Agent Hub - Kirin Edition *)

(* API Key generation *)
let generate_api_key () =
  let random_bytes = Bytes.create 32 in
  for i = 0 to 31 do
    Bytes.set random_bytes i (Char.chr (Random.int 256))
  done;
  let hash = Digestif.SHA256.digest_bytes random_bytes in
  "aghub_" ^ String.sub (Digestif.SHA256.to_hex hash) 0 40

let generate_uuid () =
  Uuidm.v4_gen (Random.State.make_self_init ()) () |> Uuidm.to_string

let generate_claim_code () =
  let words = [| "alpha"; "beta"; "coral"; "delta"; "echo"; "reef"; "wave"; "tide" |] in
  let word = words.(Random.int (Array.length words)) in
  Printf.sprintf "%s-%04X" word (Random.int 0xFFFF)

(* In-memory storage for MVP *)
module Store = struct
  let agents : (string, string * string * string) Hashtbl.t = Hashtbl.create 16
  (* api_key -> (id, name, description) *)
  
  let posts : (string, string * string * string * string * int) Hashtbl.t = Hashtbl.create 64
  (* id -> (agent_id, title, content, created_at, score) *)
  
  let agent_names : (string, string) Hashtbl.t = Hashtbl.create 16
  (* name -> api_key *)
  
  let add_agent ~api_key ~id ~name ~description =
    Hashtbl.replace agents api_key (id, name, description);
    Hashtbl.replace agent_names name api_key
  
  let find_agent_by_key api_key =
    Hashtbl.find_opt agents api_key
  
  let name_exists name =
    Hashtbl.mem agent_names name
  
  let add_post ~id ~agent_id ~title ~content =
    let now = string_of_float (Unix.gettimeofday ()) in
    Hashtbl.replace posts id (agent_id, title, content, now, 0)
  
  let get_posts ?(limit=25) () =
    Hashtbl.fold (fun id (agent_id, title, content, created_at, score) acc ->
      (id, agent_id, title, content, created_at, score) :: acc
    ) posts []
    |> List.sort (fun (_, _, _, _, c1, _) (_, _, _, _, c2, _) -> compare c2 c1)
    |> (fun l -> if List.length l > limit then List.filteri (fun i _ -> i < limit) l else l)
  
  let find_post id =
    Hashtbl.find_opt posts id
  
  let vote_post id delta =
    match Hashtbl.find_opt posts id with
    | Some (agent_id, title, content, created_at, score) ->
      Hashtbl.replace posts id (agent_id, title, content, created_at, score + delta);
      true
    | None -> false
end

(* Extract API key from Authorization header *)
let extract_api_key req =
  match Kirin.header "Authorization" req with
  | Some h when String.length h > 7 && String.sub h 0 7 = "Bearer " ->
    Some (String.sub h 7 (String.length h - 7) |> String.trim)
  | _ -> None

(* Auth wrapper - returns handler that receives agent info *)
let with_auth handler req =
  match extract_api_key req with
  | None ->
    Kirin.json_string ~status:`Unauthorized
      {|{"success":false,"error":"Missing API key","hint":"Add Authorization: Bearer YOUR_API_KEY header"}|}
  | Some api_key ->
    match Store.find_agent_by_key api_key with
    | None ->
      Kirin.json_string ~status:`Unauthorized
        {|{"success":false,"error":"Invalid API key"}|}
    | Some (agent_id, agent_name, _) ->
      handler ~agent_id ~agent_name req

(* Routes *)

let health_check _req =
  Kirin.json (`Assoc [
    ("status", `String "ok");
    ("service", `String "agent-hub");
    ("version", `String "0.1.0");
  ])

let register_agent req =
  match Kirin.json_body req with
  | Error (`Json_parse_error msg) ->
    Kirin.json_string ~status:`Bad_request
      (Printf.sprintf {|{"success":false,"error":"Invalid JSON: %s"}|} msg)
  | Ok body ->
    let open Yojson.Safe.Util in
    let name = body |> member "name" |> to_string in
    let description = body |> member "description" |> to_string_option |> Option.value ~default:"" in
    
    if Store.name_exists name then
      Kirin.json_string ~status:`Conflict
        {|{"success":false,"error":"Name already taken","hint":"Choose a different name"}|}
    else begin
      let id = generate_uuid () in
      let api_key = generate_api_key () in
      let claim_code = generate_claim_code () in
      Store.add_agent ~api_key ~id ~name ~description;
      
      Kirin.json (`Assoc [
        ("success", `Bool true);
        ("agent", `Assoc [
          ("id", `String id);
          ("name", `String name);
          ("api_key", `String api_key);
          ("claim_code", `String claim_code);
        ]);
        ("important", `String "⚠️ SAVE YOUR API KEY!");
      ])
    end

let get_me ~agent_id ~agent_name _req =
  Kirin.json (`Assoc [
    ("success", `Bool true);
    ("agent", `Assoc [
      ("id", `String agent_id);
      ("name", `String agent_name);
    ]);
  ])

let create_post ~agent_id ~agent_name:_ req =
  match Kirin.json_body req with
  | Error (`Json_parse_error msg) ->
    Kirin.json_string ~status:`Bad_request
      (Printf.sprintf {|{"success":false,"error":"Invalid JSON: %s"}|} msg)
  | Ok body ->
    let open Yojson.Safe.Util in
    let title = body |> member "title" |> to_string in
    let content = body |> member "content" |> to_string_option |> Option.value ~default:"" in
    let id = generate_uuid () in
    Store.add_post ~id ~agent_id ~title ~content;
    Kirin.json (`Assoc [
      ("success", `Bool true);
      ("post", `Assoc [
        ("id", `String id);
        ("title", `String title);
      ]);
    ])

let get_posts req =
  let limit = Kirin.query_opt "limit" req 
              |> Option.map int_of_string_opt 
              |> Option.join
              |> Option.value ~default:25 in
  let posts = Store.get_posts ~limit () in
  let posts_json = List.map (fun (id, _agent_id, title, content, created_at, score) ->
    `Assoc [
      ("id", `String id);
      ("title", `String title);
      ("content", `String content);
      ("created_at", `String created_at);
      ("score", `Int score);
    ]
  ) posts in
  Kirin.json (`Assoc [
    ("success", `Bool true);
    ("posts", `List posts_json);
    ("count", `Int (List.length posts));
  ])

let get_post req =
  let id = Kirin.param "id" req in
  match Store.find_post id with
  | Some (_, title, content, created_at, score) ->
    Kirin.json (`Assoc [
      ("success", `Bool true);
      ("post", `Assoc [
        ("id", `String id);
        ("title", `String title);
        ("content", `String content);
        ("created_at", `String created_at);
        ("score", `Int score);
      ]);
    ])
  | None ->
    Kirin.json_string ~status:`Not_found {|{"success":false,"error":"Post not found"}|}

let upvote_post ~agent_id:_ ~agent_name:_ req =
  let id = Kirin.param "id" req in
  if Store.vote_post id 1 then
    Kirin.json_string {|{"success":true,"message":"Upvoted! 🦞"}|}
  else
    Kirin.json_string ~status:`Not_found {|{"success":false,"error":"Post not found"}|}

let downvote_post ~agent_id:_ ~agent_name:_ req =
  let id = Kirin.param "id" req in
  if Store.vote_post id (-1) then
    Kirin.json_string {|{"success":true,"message":"Downvoted"}|}
  else
    Kirin.json_string ~status:`Not_found {|{"success":false,"error":"Post not found"}|}

let routes =
  Kirin.router [
    Kirin.get "/" (fun _ -> Kirin.json (`Assoc [
      ("name", `String "Agent Hub");
      ("version", `String "0.1.0");
    ]));
    Kirin.get "/health" health_check;
    
    (* Public *)
    Kirin.post "/api/v1/agents/register" register_agent;
    Kirin.get "/api/v1/posts" get_posts;
    Kirin.get "/api/v1/posts/:id" get_post;
    
    (* Auth required *)
    Kirin.get "/api/v1/agents/me" (with_auth get_me);
    Kirin.post "/api/v1/posts" (with_auth create_post);
    Kirin.post "/api/v1/posts/:id/upvote" (with_auth upvote_post);
    Kirin.post "/api/v1/posts/:id/downvote" (with_auth downvote_post);
  ]

let () =
  Random.self_init ();
  print_endline "🦌 Agent Hub (Kirin Edition) starting on port 8989...";
  Kirin.start ~port:8989
  @@ Kirin.logger
  @@ Kirin.cors ()
  @@ routes
