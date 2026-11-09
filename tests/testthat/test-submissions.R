# NA stands for JSON null.
submission <- function(id, status = "awaiting_review", ...) {
  record <- list(id = id, participant_id = paste0("P", id), session_id = paste0("S", id), status = status,
    submitted_at = "2026-11-09T15:49:00.000Z", duration_seconds = 512L, rejection_reason = NA,
    quality_flags = list(), verification_level = "L2", shared_answers = list())
  extra <- list(...)
  record[names(extra)] <- extra
  record
}

test_that("submissions come back as a data frame that joins to survey data", {
  api <- mock_api(list("GET /studies/42/submissions" = list(submissions = list(
    submission(1), submission(2, quality_flags = list("paste_heavy", "fast"))))))
  subs <- pv_submissions(42, status = "awaiting_review")
  expect_equal(nrow(subs), 2)
  expect_s3_class(subs$submitted_at, "POSIXct")
  expect_identical(format(subs$submitted_at[1], "%H:%M", tz = "UTC"), "15:49")
  expect_identical(subs$quality_flags[[2]], list("paste_heavy", "fast"))
  expect_match(api$log[[1]]$url, "status=awaiting_review")
  survey <- data.frame(PARTICIPANT_ID = c("P2", "P1"), score = c(7, 9))
  merged <- merge(subs, survey, by.x = "participant_id", by.y = "PARTICIPANT_ID")
  expect_identical(merged$score[merged$id == 1], 9)
})

test_that("an empty study gives an empty data frame", {
  mock_api(list("GET /studies/42/submissions" = list(submissions = list())))
  expect_equal(nrow(pv_submissions(42)), 0)
})

test_that("an unknown status is refused", {
  expect_error(pv_submissions(42, status = "paid"), "should be one of")
})

test_that("approving many keeps going past a failure and reports it", {
  mock_api(list(
    "POST /submissions/1/approve" = submission(1, "approved"),
    "POST /submissions/2/approve" = list(.status = 422, error = "not_allowed", message = "already reviewed"),
    "POST /submissions/3/approve" = submission(3, "approved")))
  subs <- data.frame(id = 1:3)
  expect_warning(result <- pv_approve(subs), "1 of 3 failed")
  expect_identical(result$status, c("approved", NA, "approved"))
  expect_match(result$error[2], "already reviewed")
  expect_identical(result$id, 1:3)
})

test_that("a rejection needs one of the allowed reasons", {
  api <- mock_api(list("POST /submissions/7/reject" = submission(7, "rejected", rejection_reason = "low_effort")))
  expect_error(pv_reject(7, reason = "bad"), "should be one of")
  result <- pv_reject(7, reason = "low_effort")
  expect_identical(request_body(api$log[[1]])$reason, "low_effort")
  expect_identical(result$rejection_reason, "low_effort")
})

test_that("bonuses take one amount or one per submission", {
  api <- mock_api(list(
    "POST /submissions/1/bonus" = c(submission(1, "approved"), bonuses = "1.50"),
    "POST /submissions/2/bonus" = c(submission(2, "approved"), bonuses = "0.25")))
  result <- pv_bonus(c(1, 2), c(1.5, 0.25))
  expect_identical(vapply(api$log, function(r) request_body(r)$amount, ""), c("1.50", "0.25"))
  expect_identical(result$bonuses, c(1.5, 0.25))
  expect_error(pv_bonus(c(1, 2), c(1, 2, 3)), "one per submission")
})
