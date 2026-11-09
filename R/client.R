# The HTTP layer: authentication, retries, errors and JSON-to-data-frame
# conversion. Every exported function goes through pv_request().

pv_env <- new.env(parent = emptyenv())
pv_env$calls <- 0

pv_base_url <- function() {
  sub("/+$", "", Sys.getenv("POPULIVA_API_URL", "https://populiva.com/api/v1"))
}

pv_token <- function() {
  token <- Sys.getenv("POPULIVA_TOKEN")
  if (!nzchar(token)) {
    stop("No Populiva API token. Create one on populiva.com under Lab > Developer, then set ",
      "POPULIVA_TOKEN in your .Renviron (see ?populiva).", call. = FALSE)
  }
  token
}

# A key unique to this call, reused by httr2's retries, so a retried POST
# (a launch, a bonus) never happens twice. Built without the random number
# generator so it never changes the user's seed.
pv_idempotency_key <- function() {
  pv_env$calls <- pv_env$calls + 1
  paste("r", Sys.getpid(), format(as.numeric(Sys.time()) * 1e6, scientific = FALSE, nsmall = 0),
    pv_env$calls, sep = "-")
}

pv_request <- function(path, method = "GET", body = NULL, query = NULL) {
  req <- httr2::request(pv_base_url())
  req <- httr2::req_url_path_append(req, path)
  req <- httr2::req_auth_bearer_token(req, pv_token())
  req <- httr2::req_user_agent(req, paste0("populiva-r/", utils::packageVersion("populiva")))
  req <- httr2::req_method(req, method)
  query <- Filter(Negate(is.null), as.list(query))
  if (length(query)) req <- do.call(httr2::req_url_query, c(list(req), query))
  if (method != "GET") {
    req <- httr2::req_headers(req, `Idempotency-Key` = pv_idempotency_key())
    req <- httr2::req_body_json(req, if (is.null(body)) structure(list(), names = character()) else body,
      auto_unbox = TRUE, null = "null")
  }
  req <- httr2::req_retry(req, max_tries = 3,
    is_transient = function(resp) httr2::resp_status(resp) %in% c(429, 502, 503, 504))
  req <- httr2::req_error(req, body = pv_error_message)
  httr2::resp_body_json(httr2::req_perform(req), simplifyVector = FALSE)
}

# Turns the API's error JSON into readable lines for the R error.
pv_error_message <- function(resp) {
  body <- tryCatch(httr2::resp_body_json(resp), error = function(e) NULL)
  if (!is.list(body)) return(NULL)
  details <- if (is.list(body$details)) {
    paste0(names(body$details), ": ", vapply(body$details, function(x) paste(unlist(x), collapse = ", "), ""))
  }
  c(
    body$error,
    body$message,
    body$reason,
    details,
    if (!is.null(body$required_scope)) paste0("This needs a token with the '", body$required_scope, "' scope."),
    if (!is.null(body$parameter)) paste0("Missing parameter: ", body$parameter)
  )
}

# Formats dollars for the API, which takes amounts as strings ("2.50").
pv_dollars <- function(x, arg) {
  if (!is.numeric(x) || length(x) != 1 || is.na(x) || x < 0) {
    stop("`", arg, "` must be a single non-negative number of US dollars.", call. = FALSE)
  }
  sprintf("%.2f", x)
}

pv_id <- function(x, arg = "id") {
  if (is.data.frame(x) && "id" %in% names(x) && nrow(x) == 1) x <- x$id
  if (is.list(x) && !is.null(x$id)) x <- x$id
  if (length(x) != 1 || is.na(x) || !nzchar(as.character(x))) {
    stop("`", arg, "` must be a single ID.", call. = FALSE)
  }
  utils::URLencode(as.character(x), reserved = TRUE)
}

pv_time <- function(x) {
  x <- as.character(x)
  x <- sub("Z$", "+0000", x)
  x <- sub("([+-][0-9]{2}):([0-9]{2})$", "\\1\\2", x)
  as.POSIXct(strptime(x, "%Y-%m-%dT%H:%M:%OS%z", tz = "UTC"))
}

# A list of JSON records to a data frame: one row per record. Scalar fields
# become ordinary columns (NULL becomes NA); fields holding lists or vectors
# become list columns. `money` columns become numbers of dollars and `times`
# columns become POSIXct in UTC.
pv_as_df <- function(records, money = character(), times = character()) {
  if (!length(records)) return(data.frame())
  columns <- unique(unlist(lapply(records, names)))
  out <- data.frame(row.names = seq_along(records))
  for (column in columns) {
    values <- lapply(records, function(record) record[[column]])
    scalar <- all(vapply(values, function(v) is.null(v) || (is.atomic(v) && length(v) == 1), logical(1)))
    if (scalar) {
      value <- unlist(lapply(values, function(v) if (is.null(v)) NA else v))
      if (column %in% money) value <- as.numeric(value)
      if (column %in% times) value <- pv_time(value)
      out[[column]] <- value
    } else {
      out[[column]] <- I(values)
    }
  }
  out
}

# One record (a study, a submission) as a one-row data frame.
pv_as_row <- function(record, ...) pv_as_df(list(record), ...)
