(** Kirin Remix Integration

    Unified API for Remix-style routing, loaders, and actions in Kirin.
    Server-first React meta-framework patterns. *)

(** Re-export modules *)
module Loader = Loader
module Action = Action
module Route = Route
module Ssr = Ssr

(** {1 Quick Loader Helpers} *)

(** Create loader context *)
let loader_context = Loader.context_of_request

(** Pure loader (always returns data) *)
let pure = Loader.pure

(** Redirect loader *)
let redirect = Loader.redirect

(** Not found loader *)
let not_found = Loader.not_found

(** Error loader *)
let error = Loader.error

(** Parallel loaders *)
let parallel = Loader.parallel

(** Optional loader *)
let optional = Loader.optional

(** {1 Quick Action Helpers} *)

(** Create action context *)
let action_context = Action.context_of_request

(** Get form field *)
let field = Action.field

(** Required field *)
let required_field = Action.required_field

(** Succeed action *)
let succeed = Action.succeed

(** Redirect action *)
let redirect_action = Action.redirect_action

(** Validate form *)
let validate = Action.validate

(** {1 Validators} *)

let not_empty = Action.not_empty
let min_length = Action.min_length
let max_length = Action.max_length
let is_email = Action.is_email
let is_numeric = Action.is_numeric

(** {1 Route Helpers} *)

(** Create route *)
let route = Route.create

(** Index route *)
let index = Route.index

(** Layout route *)
let layout = Route.layout

(** Add loader to route *)
let with_loader = Route.with_loader

(** Add action to route *)
let with_action = Route.with_action

(** Add error boundary *)
let with_error_boundary = Route.with_error_boundary

(** Add children *)
let with_children = Route.with_children

(** Find matching routes *)
let find_matches = Route.find_matches

(** Generate URL *)
let generate_url = Route.generate_url

(** {1 SSR Helpers} *)

(** Default SSR config *)
let default_config = Ssr.default_config

(** Create render context *)
let render_context = Ssr.create_context

(** Load data for routes *)
let load_data = Ssr.load_data

(** Render document *)
let render_document = Ssr.render_document

(** Create SSR handler *)
let handler = Ssr.handler

(** Hydration script *)
let hydration_script = Ssr.hydration_script

(** {1 Error Handling} *)

(** Error boundary *)
let error_boundary = Ssr.render_error_boundary

(** Catch boundary (404, etc) *)
let catch_boundary = Ssr.render_catch_boundary

(** {1 Full Example} *)

(**
{[
   (* Define routes with loaders and actions *)
   let routes = Kirin_remix.[
     route "/" ~loader:(Some (fun ctx ->
       let user = get_user ctx in
       Loader.Data (`Assoc [("user", user_to_json user)])
     ));

     route "/posts/:id" ~loader:(Some (fun ctx ->
       match Loader.param ctx "id" with
       | Some id ->
         let post = get_post id in
         Loader.Data (post_to_json post)
       | None -> Loader.NotFound
     ));

     route "/posts/new" ~action:(Some (fun ctx ->
       let title = Action.field ctx "title" in
       let body = Action.field ctx "body" in
       match title, body with
       | Some t, Some b ->
         let post = create_post t b in
         Action.Redirect ("/posts/" ^ post.id, 302)
       | _ ->
         Action.ValidationError [("title", "Required")]
     ));
   ]

   (* Create SSR handler *)
   let ssr_handler = Kirin_remix.handler
     ~config:default_config
     ~routes
     ~render_component:(fun ctx ->
       let data = ctx.loader_data in
       "<h1>My App</h1>..."
     )

   (* Kirin routes *)
   let kirin_routes = [
     Kirin.get "/*" ssr_handler;
     Kirin.post "/*" ssr_handler;
   ]
]}
*)

(** {1 Serialization} *)

(** Loader context to JSON *)
let loader_context_to_json = Loader.context_to_json

(** Action context to JSON *)
let action_context_to_json = Action.context_to_json

(** Route to JSON *)
let route_to_json = Route.to_json

(** SSR config to JSON *)
let config_to_json = Ssr.config_to_json

(** Render context to JSON *)
let render_context_to_json = Ssr.context_to_json
