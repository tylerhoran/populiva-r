SUBMISSION_STATUSES <- c("awaiting_review", "approved", "auto_approved", "rejected", "screened_out")
REJECTION_REASONS <- c("no_completion", "failed_attention_checks", "did_not_follow_instructions",
  "low_effort", "duplicate_or_fraud")

pv_submission_df <- function(records) {
  pv_as_df(records, money = "bonuses", times = "submitted_at")
}

#' A study's submissions
#'
#' One row per participant who submitted. `participant_id` is the
#' `PARTICIPANT_ID` your survey received, and `session_id` the `SESSION_ID`.
#' Join on either to match submissions to your own data.
#'
#' @inheritParams pv_study
#' @param status Only submissions with this status: `"awaiting_review"`,
#'   `"approved"`, `"auto_approved"`, `"rejected"` or `"screened_out"`.
#' @return A data frame. `quality_flags` and `shared_answers` are list
#'   columns.
#' @export
#' @examples
#' \dontrun{
#' subs <- pv_submissions(123, status = "awaiting_review")
#' survey <- read.csv("qualtrics_export.csv")
#' merged <- merge(subs, survey, by.x = "participant_id", by.y = "PARTICIPANT_ID")
#' }
pv_submissions <- function(study, status = NULL) {
  if (!is.null(status)) status <- match.arg(status, SUBMISSION_STATUSES)
  pv_submission_df(pv_request(c("studies", pv_id(study, "study"), "submissions"),
    query = list(status = status))$submissions)
}

#' Get one submission
#'
#' @param submission A submission ID, or a one-row data frame or list with an
#'   `id`.
#' @return A one-row data frame.
#' @export
pv_submission <- function(submission) {
  pv_submission_df(list(pv_request(c("submissions", pv_id(submission, "submission")))))
}

#' Approve or reject submissions
#'
#' Approving pays the participant the study's reward. A rejection needs one
#' of Populiva's reasons; participants see it and can appeal. Both work on
#' submissions awaiting review and take one ID or many. If one fails, the
#' rest still go ahead, and its `error` column says why.
#'
#' @param submissions Submission IDs, or a data frame from [pv_submissions()].
#' @param reason Why you're rejecting: `"no_completion"`,
#'   `"failed_attention_checks"`, `"did_not_follow_instructions"`,
#'   `"low_effort"` or `"duplicate_or_fraud"`.
#' @return A data frame with one row per submission after the change.
#' @export
#' @examples
#' \dontrun{
#' subs <- pv_submissions(123, status = "awaiting_review")
#' passed <- subs$participant_id %in% survey$PARTICIPANT_ID[survey$attention_ok]
#' pv_approve(subs[passed, ])
#' pv_reject(subs[!passed, ], reason = "failed_attention_checks")
#' }
pv_approve <- function(submissions) {
  pv_each(submissions, function(id) pv_request(c("submissions", id, "approve"), "POST"))
}

#' @rdname pv_approve
#' @export
pv_reject <- function(submissions, reason = REJECTION_REASONS) {
  reason <- match.arg(reason)
  pv_each(submissions, function(id) pv_request(c("submissions", id, "reject"), "POST", list(reason = reason)))
}

#' Pay a bonus
#'
#' Bonuses go to approved submissions, up to $100 each. They need a token
#' with the `money` scope and stop at the token's spending caps.
#'
#' @inheritParams pv_approve
#' @param amount Bonus per submission in US dollars: one amount, or one per
#'   submission.
#' @return A data frame with one row per submission. `bonuses` is the total
#'   bonus paid on it so far.
#' @export
#' @examples
#' \dontrun{
#' winners <- subset(pv_submissions(123, status = "approved"), participant_id %in% top_scorers)
#' pv_bonus(winners, 1.50)
#' }
pv_bonus <- function(submissions, amount) {
  ids <- pv_ids(submissions)
  if (length(amount) == 1) amount <- rep(amount, length(ids))
  if (length(amount) != length(ids)) stop("`amount` must be one amount or one per submission.", call. = FALSE)
  amounts <- vapply(amount, pv_dollars, "", arg = "amount")
  pv_each(seq_along(ids), function(i) {
    pv_request(c("submissions", ids[[i]], "bonus"), "POST", list(amount = amounts[[i]]))
  }, labels = ids)
}

pv_ids <- function(x) {
  if (is.data.frame(x)) x <- x$id
  if (!length(x) || anyNA(x)) stop("Give one or more submission IDs.", call. = FALSE)
  vapply(as.character(x), function(id) utils::URLencode(id, reserved = TRUE), "", USE.NAMES = FALSE)
}

# Runs `call` for each submission and keeps going past failures.
pv_each <- function(submissions, call, labels = NULL) {
  items <- if (is.null(labels)) pv_ids(submissions) else submissions
  labels <- if (is.null(labels)) items else labels
  records <- lapply(seq_along(items), function(i) {
    tryCatch(call(items[[i]]), error = function(e) {
      list(id = suppressWarnings(as.integer(labels[[i]])), error = conditionMessage(e))
    })
  })
  failed <- vapply(records, function(r) !is.null(r$error), logical(1))
  if (any(failed)) warning(sum(failed), " of ", length(records), " failed; see the `error` column.", call. = FALSE)
  pv_submission_df(records)
}
