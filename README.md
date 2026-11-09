# populiva

Run online studies on [Populiva](https://populiva.com) from R. Create and launch studies, review submissions against your own data, pay bonuses, manage participant groups for follow-up waves, and issue provenance reports you can cite in your methods section.

## Install

```r
# install.packages("remotes")
remotes::install_github("tylerhoran/populiva-r")
```

## Set up

Create an API token on populiva.com under **Lab → Developer**. Give it the scopes it needs:
- `read` to look things up
- `studies` to create and run studies and review submissions
- `money` for bonuses

Then add the token to your `.Renviron`, for example with `usethis::edit_r_environ()`:

```
POPULIVA_TOKEN=pv_...
```

Restart R and check it works:

```r
library(populiva)
pv_balance()
```

Keep the token out of scripts you share. Tokens can have daily and per-call spending caps, so a bug in a loop can't spend your whole balance.

## A study from start to finish

```r
library(populiva)

# 1. Create a draft and check its cost. Nothing is charged yet.
study <- pv_create_study(
  title = "Decisions under uncertainty",
  description = "A 10-minute study about everyday choices.",
  external_url = "https://myuni.qualtrics.com/jfe/form/SV_abc123",
  estimated_minutes = 10,
  reward = 2,
  places = 200,
  screeners = list(age_min = 18, age_max = 34)
)
study$cost

# 2. In your survey, save PARTICIPANT_ID from the link, and send people
#    here at the end:
pv_completion_url(study)

# 3. Launch. This reserves the cost from your balance.
study <- pv_launch(study)

# 4. Match submissions to your data, then approve or reject.
subs <- pv_submissions(study, status = "awaiting_review")
survey <- read.csv("qualtrics_export.csv")
checked <- merge(subs, survey, by.x = "participant_id", by.y = "PARTICIPANT_ID")

pv_approve(checked[checked$attention_check == "pass", ])
pv_reject(checked[checked$attention_check == "fail", ], reason = "failed_attention_checks")

# 5. Pay any bonuses you promised.
pv_bonus(checked[checked$score >= 8, ], 1.00)

# 6. Issue a provenance report, and cite its URL in your methods section.
report <- pv_issue_provenance_report(study)
report$url
```

Rejections need one of Populiva's reasons. Participants see it and can appeal.

## Longitudinal studies

Put a wave's completers in a group and tell them the next wave is coming. When you set up the next wave on populiva.com, limit it to that group.

```r
wave1 <- pv_submissions(study, status = "approved")
group <- pv_create_group("Wave 1 completers")
pv_add_to_group(group, wave1$participant_id)
pv_message("Wave 2 opens tomorrow at 9am ET.", study = study, audience = "approved")
```

## Functions

| Studies | Submissions | Other |
|---|---|---|
| `pv_studies()`, `pv_study()` | `pv_submissions()`, `pv_submission()` | `pv_balance()` |
| `pv_create_study()`, `pv_update_study()` | `pv_approve()`, `pv_reject()` | `pv_groups()`, `pv_group()`, `pv_create_group()` |
| `pv_launch()`, `pv_pause()`, `pv_resume()`, `pv_close()` | `pv_bonus()` | `pv_add_to_group()`, `pv_remove_from_group()` |
| `pv_add_places()`, `pv_completion_url()` | | `pv_message()` |
| `pv_provenance_reports()`, `pv_issue_provenance_report()` | | |

Functions return data frames, with money as numbers of US dollars and times as `POSIXct` in UTC. Functions that act on many submissions keep going if one fails, and report the failure in an `error` column. Retried requests carry an idempotency key, so a network error can't launch a study or pay a bonus twice.

## Setting up your survey tool

Populiva has setup guides for Qualtrics, jsPsych, Gorilla, oTree, PsychoPy and LimeSurvey at <https://populiva.com/guides>. The full API is documented at <https://populiva.com/developers>.

## License

MIT © Digion Inc
