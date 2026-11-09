STUDY_MONEY <- c("reward", "cost")
STUDY_TIMES <- c("launched_at", "closed_at")

pv_study_row <- function(study) pv_as_row(study, money = STUDY_MONEY, times = STUDY_TIMES)

#' List your lab's studies
#'
#' @return A data frame with one row per study, newest first (up to 200).
#'   `reward` and `cost` are in US dollars.
#' @export
#' @examples
#' \dontrun{
#' studies <- pv_studies()
#' studies[studies$status == "active", c("id", "title", "open_places")]
#' }
pv_studies <- function() {
  pv_as_df(pv_request("studies")$studies, money = STUDY_MONEY, times = STUDY_TIMES)
}

#' Get one study
#'
#' @param study A study ID, or a one-row data frame or list with an `id`.
#' @return A one-row data frame.
#' @export
pv_study <- function(study) {
  pv_study_row(pv_request(c("studies", pv_id(study, "study"))))
}

#' Create a draft study
#'
#' Creates a draft. Nothing is charged until you call [pv_launch()].
#' Participants open `external_url` with `PARTICIPANT_ID`, `STUDY_ID` and
#' `SESSION_ID` added to it. Placeholders such as `{{PARTICIPANT_ID}}` in
#' the link's query are filled in too. Send them back to
#' [pv_completion_url()] when they finish.
#'
#' @param title,description What participants see. The description is up to
#'   5,000 characters.
#' @param external_url Your survey or experiment link (https).
#' @param estimated_minutes How long the study takes, in minutes.
#' @param reward Pay per participant, in US dollars. It must meet
#'   Populiva's minimum hourly rate for `estimated_minutes`.
#' @param places How many participants you need.
#' @param max_minutes Time allowed before a place times out. Defaults to
#'   three times `estimated_minutes`, between 10 and 720.
#' @param language `"en"` or `"es"`.
#' @param screen_out Pay for people you screen out, in US dollars (optional).
#' @param screeners A named list of screeners, for example
#'   `list(age_min = 18, age_max = 34, sex = "female", state = c("CA", "TX"))`.
#'   Other screeners include `race_ethnicity`, `education`, `party_id`,
#'   `political_ideology`, `employment_status`, `household_income` and
#'   `children_in_household`; the study form on populiva.com lists their
#'   values.
#' @param ... Other study settings accepted by the API, such as
#'   `device_requirement`, `balance`, `census_matched`, `min_approval_rate`,
#'   `screen_out_minutes`, `allowlist_group_ids` (only these groups),
#'   `blocklist_group_ids` (never these groups) or `excluded_study_ids` (skip
#'   people who took these studies). Give ID lists as `list(...)` so a single ID
#'   is still sent as a list.
#' @return The new study as a one-row data frame, including its `cost`.
#' @export
#' @examples
#' \dontrun{
#' study <- pv_create_study(
#'   title = "Decisions under uncertainty",
#'   description = "A 10-minute study about everyday choices.",
#'   external_url = "https://myuni.qualtrics.com/jfe/form/SV_abc123",
#'   estimated_minutes = 10,
#'   reward = 2,
#'   places = 200
#' )
#' study$cost
#' pv_completion_url(study)
#' }
pv_create_study <- function(title, description, external_url, estimated_minutes, reward, places,
                            max_minutes = NULL, language = c("en", "es"), screen_out = NULL,
                            screeners = NULL, ...) {
  language <- match.arg(language)
  if (is.null(max_minutes)) max_minutes <- min(max(3 * estimated_minutes, 10), 720)
  body <- c(
    list(title = title, description = description, external_url = external_url,
      estimated_minutes = as.integer(estimated_minutes), max_minutes = as.integer(max_minutes),
      reward = pv_dollars(reward, "reward"), places = as.integer(places), language = language),
    if (!is.null(screen_out)) list(screen_out = pv_dollars(screen_out, "screen_out")),
    if (!is.null(screeners)) list(screeners = screeners),
    list(...)
  )
  pv_study_row(pv_request("studies", "POST", body))
}

#' Update a draft study
#'
#' Only drafts can be changed. Pass the settings to change, using the same
#' names as [pv_create_study()].
#'
#' @inheritParams pv_study
#' @param ... Settings to change.
#' @return The updated study as a one-row data frame.
#' @export
pv_update_study <- function(study, ...) {
  body <- list(...)
  for (amount in intersect(c("reward", "screen_out"), names(body))) {
    body[[amount]] <- pv_dollars(body[[amount]], amount)
  }
  pv_study_row(pv_request(c("studies", pv_id(study, "study")), "PATCH", body))
}

#' Launch, pause, resume or close a study
#'
#' `pv_launch()` reserves the study's cost from your lab's balance and opens
#' it to participants. It stops at your token's spending caps. `pv_close()`
#' releases whatever is left of the reserve.
#'
#' @inheritParams pv_study
#' @return The study as a one-row data frame.
#' @export
pv_launch <- function(study) pv_lifecycle(study, "launch")

#' @rdname pv_launch
#' @export
pv_pause <- function(study) pv_lifecycle(study, "pause")

#' @rdname pv_launch
#' @export
pv_resume <- function(study) pv_lifecycle(study, "resume")

#' @rdname pv_launch
#' @export
pv_close <- function(study) pv_lifecycle(study, "close")

pv_lifecycle <- function(study, action) {
  pv_study_row(pv_request(c("studies", pv_id(study, "study"), action), "POST"))
}

#' Add places to a running study
#'
#' @inheritParams pv_study
#' @param count How many places to add. Their cost is reserved from your
#'   balance.
#' @return The study as a one-row data frame.
#' @export
pv_add_places <- function(study, count) {
  pv_study_row(pv_request(c("studies", pv_id(study, "study"), "add_places"), "POST",
    list(count = as.integer(count))))
}

#' The address participants return to when they finish
#'
#' Send participants here at the end of your survey or experiment. Use a
#' different completion code (set up on the study page) for screen-outs or
#' failed attention checks.
#'
#' @inheritParams pv_study
#' @param code A completion code. Defaults to the study's own code.
#' @return A URL.
#' @export
#' @examples
#' pv_completion_url(list(id = 123, completion_code = "C0DE42"))
pv_completion_url <- function(study, code = NULL) {
  if (is.null(code)) {
    if (!is.list(study) || is.null(study$completion_code)) study <- pv_study(study)
    code <- study$completion_code
  }
  site <- sub("/api/v1$", "", pv_base_url())
  paste0(site, "/c/", pv_id(study, "study"), "?code=", utils::URLencode(code, reserved = TRUE))
}
