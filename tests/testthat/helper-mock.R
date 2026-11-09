# Fakes the API: `routes` maps "METHOD /path" to a response body (a list), or
# to a function of the request returning one. A `.status` element sets the
# HTTP status. Returns an environment whose `log` holds every request.
mock_api <- function(routes, env = parent.frame()) {
  withr::local_envvar(POPULIVA_TOKEN = "pv_test", POPULIVA_API_URL = "https://populiva.test/api/v1",
    .local_envir = env)
  seen <- new.env()
  seen$log <- list()
  httr2::local_mocked_responses(function(req) {
    path <- sub("^https://populiva.test/api/v1", "", sub("\\?.*$", "", req$url))
    seen$log[[length(seen$log) + 1]] <- req
    route <- routes[[paste(req$method %||% "GET", path)]]
    if (is.function(route)) route <- route(req)
    if (is.null(route)) route <- list(.status = 404, error = "not_found")
    status <- route$.status %||% 200
    route$.status <- NULL
    httr2::response_json(status_code = status, body = route)
  }, env = env)
  seen
}

`%||%` <- function(x, y) if (is.null(x)) y else x

request_body <- function(req) req$body$data

# The JSON httr2 would send for a body.
jsonlite_like <- function(x) as.character(jsonlite::toJSON(x, auto_unbox = TRUE))
