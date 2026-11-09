#' populiva: client for the Populiva research participant API
#'
#' Run online studies on [Populiva](https://populiva.com) from R.
#'
#' @section Setup:
#' Create an API token on populiva.com under Lab > Developer. Choose the
#' scopes it needs: `read` to look things up, `studies` to create and run
#' studies and review submissions, and `money` for bonuses. Then add it to
#' your `.Renviron` file (for example with `usethis::edit_r_environ()`):
#'
#' ```
#' POPULIVA_TOKEN=pv_...
#' ```
#'
#' Restart R, and check it works with [pv_balance()]. Keep the token out of
#' scripts you share.
#'
#' @section A typical study:
#' 1. [pv_create_study()] with your survey link, then [pv_launch()].
#' 2. In your survey, save `PARTICIPANT_ID` from the link, and send people to
#'    [pv_completion_url()] at the end.
#' 3. [pv_submissions()], join to your data, then [pv_approve()] or
#'    [pv_reject()], and [pv_bonus()] if you promised one.
#' 4. [pv_issue_provenance_report()] and cite its URL in your methods.
#'
#' Setup guides for Qualtrics, jsPsych, Gorilla, oTree, PsychoPy and
#' LimeSurvey are at <https://populiva.com/guides>.
#'
#' @keywords internal
"_PACKAGE"
