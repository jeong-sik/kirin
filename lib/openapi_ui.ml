(** OpenAPI UI Templates

    Pure HTML template functions for Swagger UI and ReDoc.
    No dependency on OpenAPI types — takes only [spec_url] and [title] strings.
*)

(** Swagger UI HTML page. *)
let swagger_ui_html ~spec_url ~title =
  Printf.sprintf {|<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>%s - Swagger UI</title>
  <link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/swagger-ui-dist@5/swagger-ui.css">
</head>
<body>
  <div id="swagger-ui"></div>
  <script src="https://cdn.jsdelivr.net/npm/swagger-ui-dist@5/swagger-ui-bundle.js"></script>
  <script>
    window.onload = function() {
      SwaggerUIBundle({
        url: "%s",
        dom_id: '#swagger-ui',
        presets: [
          SwaggerUIBundle.presets.apis,
          SwaggerUIBundle.SwaggerUIStandalonePreset
        ],
        layout: "BaseLayout"
      });
    };
  </script>
</body>
</html>|} title spec_url

(** ReDoc HTML page. *)
let redoc_html ~spec_url ~title =
  Printf.sprintf {|<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>%s - API Documentation</title>
  <link href="https://fonts.googleapis.com/css?family=Roboto:300,400,700" rel="stylesheet">
  <style>body { margin: 0; padding: 0; }</style>
</head>
<body>
  <redoc spec-url="%s"></redoc>
  <script src="https://cdn.redoc.ly/redoc/latest/bundles/redoc.standalone.js"></script>
</body>
</html>|} title spec_url
