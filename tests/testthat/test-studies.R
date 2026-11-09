# NA stands for JSON null.
study <- list(id = 42L, kind = "survey", title = "Choices", status = "draft", language = "en", places = 200L,
  open_places = 200L, reward = "2.00", estimated_minutes = 10L, max_minutes = 30L,
  external_url = "https://example.edu/s", completion_code = "ABC123", screeners = list(),
  ai_use = FALSE, content_categories = list(), expert_panel = NA, launched_at = NA, closed_at = NA,
  project_id = NA, cost = "460.00")

test_that("a study comes back as a one-row data frame with dollars as numbers", {
  mock_api(list("GET /studies/42" = study))
  s <- pv_study(42)
  expect_equal(nrow(s), 1)
  expect_identical(s$reward, 2)
  expect_identical(s$cost, 460)
  expect_true(is.na(s$launched_at))
  expect_identical(s$completion_code, "ABC123")
})

test_that("creating a study sends dollars as strings, whole minutes and a default time limit", {
  api <- mock_api(list("POST /studies" = function(req) c(study, .status = 201)))
  pv_create_study("Choices", "About choices", "https://example.edu/s", estimated_minutes = 10, reward = 2,
    places = 200, screeners = list(age_min = 18, state = c("CA", "TX")))
  req <- api$log[[1]]
  body <- request_body(req)
  expect_identical(body$reward, "2.00")
  expect_identical(body$max_minutes, 30L)
  expect_identical(body$language, "en")
  expect_identical(body$screeners$state, c("CA", "TX"))
  expect_identical(httr2::req_get_headers(req, "reveal")$Authorization, "Bearer pv_test")
  expect_match(req$headers$`Idempotency-Key`, "^r-")
})

test_that("each POST gets its own idempotency key", {
  api <- mock_api(list("POST /studies/42/launch" = study, "POST /studies/42/close" = study))
  pv_launch(42)
  pv_close(42)
  keys <- vapply(api$log, function(r) r$headers$`Idempotency-Key`, "")
  expect_length(unique(keys), 2)
})

test_that("a study row can be passed where an ID is expected", {
  api <- mock_api(list("GET /studies/42" = study, "POST /studies/42/add_places" = study))
  s <- pv_study(42)
  pv_add_places(s, 50)
  expect_identical(request_body(api$log[[2]])$count, 50L)
})

test_that("API errors become R errors with the API's explanation", {
  mock_api(list("POST /studies/42/launch" = list(.status = 402, error = "insufficient_funds"),
    "POST /studies" = list(.status = 422, error = "invalid",
      details = list(reward_cents = list("is below the minimum of $1.67")))))
  expect_error(pv_launch(42), "insufficient_funds")
  expect_error(pv_create_study("t", "d", "https://x.edu", 10, 0.5, 10), "below the minimum")
})

test_that("the completion address uses the study's code unless given one", {
  withr::local_envvar(POPULIVA_API_URL = "https://populiva.com/api/v1")
  expect_identical(pv_completion_url(list(id = 42, completion_code = "ABC123")), "https://populiva.com/c/42?code=ABC123")
  expect_identical(pv_completion_url(42, code = "SCREEN1"), "https://populiva.com/c/42?code=SCREEN1")
})

test_that("a missing token explains how to get one", {
  withr::local_envvar(POPULIVA_TOKEN = NA)
  expect_error(pv_studies(), "Lab > Developer")
})

test_that("bad amounts are refused before any request", {
  mock_api(list())
  expect_error(pv_create_study("t", "d", "https://x.edu", 10, "2", 10), "US dollars")
  expect_error(pv_update_study(42, reward = -1), "US dollars")
})
