test_that("the balance is in dollars", {
  mock_api(list("GET /balance" = list(available = "120.50", reserved = "460.00", currency = "USD")))
  expect_identical(pv_balance(), list(available = 120.5, reserved = 460, currency = "USD"))
})

test_that("group members are sent as a list even when there's only one", {
  api <- mock_api(list("POST /groups/5/members" = list(id = 5L, name = "Wave 1", size = 1L, added = 1L,
    unknown_participant_ids = list())))
  pv_add_to_group(5, "P1")
  expect_identical(jsonlite_like(request_body(api$log[[1]])), '{"participant_ids":["P1"],"remove":false}')
})

test_that("unknown participant IDs are reported", {
  mock_api(list("POST /groups/5/members" = list(id = 5L, name = "Wave 1", size = 1L, added = 1L,
    unknown_participant_ids = list("NOPE"))))
  expect_warning(result <- pv_add_to_group(5, c("P1", "NOPE")), "1 participant IDs")
  expect_identical(result$unknown_participant_ids, "NOPE")
})

test_that("provenance reports carry a citable URL", {
  mock_api(list("POST /studies/42/provenance_reports" = list(.status = 201, id = "abcDEF123456", version = 1L,
    issued_at = "2026-11-09T16:00:00Z", sha256 = "ff", url = "https://populiva.com/provenance/abcDEF123456",
    json_url = "https://populiva.com/provenance/abcDEF123456.json")))
  report <- pv_issue_provenance_report(42)
  expect_identical(report$url, "https://populiva.com/provenance/abcDEF123456")
  expect_s3_class(report$issued_at, "POSIXct")
})

test_that("a message goes to one submission or a study, not both", {
  api <- mock_api(list("POST /messages" = list(.status = 201, sent = 12L)))
  expect_error(pv_message("hi"), "either")
  expect_error(pv_message("hi", submission = 1, study = 2), "either")
  expect_identical(pv_message("Wave 2 opens tomorrow", study = 42, audience = "approved"), 12L)
  expect_identical(request_body(api$log[[1]])$audience, "approved")
})
