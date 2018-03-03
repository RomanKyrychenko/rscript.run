fib = function(request) {
  
  #' ---
  #' description: Calculates Fibonacci number
  #' parameters:
  #'   - name: "n"
  #'     description: "x for Fibonnacci number"
  #'     in: query
  #'     schema:
  #'       type: integer
  #'     example: 10
  #'     required: true
  #' responses:
  #'   200:
  #'     description: API response
  #'     content:
  #'       text/plain:
  #'         schema:
  #'           type: string
  #'           example: 5
  #' ---
  
  n = as.integer( request$query[["n"]] )
  RestRserve::create_response(payload = as.character(calc_fib(n)),
                              content_type = "text/plain",
                              headers = character(0),
                              status_code = 200L)
}

app = RestRserve::RestRserveApplication$new()
app$add_get(path = "/fib", FUN = fib)
app$add_openapi(path = "/openapi.yaml", file_path = "openapi.yaml")
app$add_swagger_ui(path = "/swagger", 
                   path_openapi = "/openapi.yaml", 
                   path_swagger_assets = "/__swagger__")
app$run(http_port = "8001")