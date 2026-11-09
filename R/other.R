#' Your lab's balance
#'
#' @return A list with `available` and `reserved` amounts in US dollars.
#'   Launched studies reserve their cost until they close.
#' @export
pv_balance <- function() {
  balance <- pv_request("balance")
  list(available = as.numeric(balance$available), reserved = as.numeric(balance$reserved),
    currency = balance$currency)
}

#' Provenance reports
#'
#' A provenance report is a signed, public record of how a study's sample was
#' recruited and checked. Cite its `url` in your methods section, and anyone
#' can verify it. `pv_issue_provenance_report()` issues a new version after
#' the study has launched.
#'
#' @inheritParams pv_study
#' @return A data frame of reports, newest first, or the new report as a
#'   one-row data frame.
#' @export
#' @examples
#' \dontrun{
#' report <- pv_issue_provenance_report(123)
#' report$url
#' }
pv_provenance_reports <- function(study) {
  pv_as_df(pv_request(c("studies", pv_id(study, "study"), "provenance_reports"))$provenance_reports,
    times = "issued_at")
}

#' @rdname pv_provenance_reports
#' @export
pv_issue_provenance_report <- function(study) {
  pv_as_row(pv_request(c("studies", pv_id(study, "study"), "provenance_reports"), "POST"), times = "issued_at")
}

#' Participant groups
#'
#' Groups are lists of your lab's participants, for follow-up waves of a
#' longitudinal study, invitations or exclusions. Participants are identified
#' by the `PARTICIPANT_ID` your lab sees.
#'
#' @param group A group ID, or a one-row data frame or list with an `id`.
#' @param name A name for the new group.
#' @param participant_ids `PARTICIPANT_ID`s to add or remove.
#' @return `pv_groups()` returns a data frame of groups. `pv_group()` returns a
#'   list with the group's `participant_ids`. `pv_create_group()` returns
#'   the new group, and `pv_add_to_group()` and `pv_remove_from_group()`
#'   return the changed group with counts of who was added or removed.
#' @export
#' @examples
#' \dontrun{
#' wave1 <- pv_submissions(123, status = "approved")
#' group <- pv_create_group("Wave 1 completers")
#' pv_add_to_group(group, wave1$participant_id)
#' }
pv_groups <- function() pv_as_df(pv_request("groups")$groups)

#' @rdname pv_groups
#' @export
pv_group <- function(group) {
  group <- pv_request(c("groups", pv_id(group, "group")))
  group$participant_ids <- as.character(unlist(group$participant_ids))
  group
}

#' @rdname pv_groups
#' @export
pv_create_group <- function(name) pv_as_row(pv_request("groups", "POST", list(name = name)))

#' @rdname pv_groups
#' @export
pv_add_to_group <- function(group, participant_ids) {
  pv_group_members(group, participant_ids, remove = FALSE)
}

#' @rdname pv_groups
#' @export
pv_remove_from_group <- function(group, participant_ids) {
  pv_group_members(group, participant_ids, remove = TRUE)
}

pv_group_members <- function(group, participant_ids, remove) {
  ids <- unique(as.character(participant_ids))
  result <- pv_request(c("groups", pv_id(group, "group"), "members"), "POST",
    list(participant_ids = I(ids), remove = remove))
  result$unknown_participant_ids <- as.character(unlist(result$unknown_participant_ids))
  if (length(result$unknown_participant_ids)) {
    warning(length(result$unknown_participant_ids), " participant IDs weren't found among your lab's participants.",
      call. = FALSE)
  }
  result
}

#' Message participants
#'
#' Send a message to the participant behind one submission, or to everyone
#' in a study's audience. Participants read it in their Populiva inbox.
#'
#' @param body The message text.
#' @param submission A submission ID, to message that one participant.
#' @param study A study ID, to message a study's participants.
#' @param audience With `study`, who gets the message: `"all_submitted"`
#'   (the default), `"approved"`, `"awaiting_review"`, `"not_submitted"`, or
#'   `"group"` together with `group`.
#' @param group A group ID, with `audience = "group"`.
#' @return The number of participants messaged, invisibly.
#' @export
#' @examples
#' \dontrun{
#' pv_message("Wave 2 opens tomorrow at 9am ET.", study = 123)
#' }
pv_message <- function(body, submission = NULL, study = NULL, audience = NULL, group = NULL) {
  if (is.null(submission) == is.null(study)) stop("Give either `submission` or `study`.", call. = FALSE)
  payload <- Filter(Negate(is.null), list(
    body = body,
    submission_id = if (!is.null(submission)) pv_id(submission, "submission"),
    study_id = if (!is.null(study)) pv_id(study, "study"),
    audience = if (!is.null(audience)) match.arg(audience, c("all_submitted", "approved", "awaiting_review", "not_submitted", "group")),
    group_id = if (!is.null(group)) pv_id(group, "group")
  ))
  invisible(pv_request("messages", "POST", payload)$sent)
}
