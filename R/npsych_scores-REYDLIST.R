#' @include new_npsych_scores.R
NULL

#' REYDLIST Test Scores
#'
#' @description Create a `REYDLIST` object to hold REYDLIST scores.
#'
#' @param x Numeric scores.
#'
#' @returns
#' An object of class `REYDLIST`.
#'
#' @export
REYDLIST <- new_npsych_scores(
  "REYDLIST",
  label = "RAVLT Distractor List",
  domain = "Memory",
  short_descriptor = "RAVLT Distractor List (not in NACC data)",
  range = c(0, 15),
  codes = c(
    "Physical problem" = 95,
    "Cognitive/behavior problem" = 96,
    "Other problem" = 97,
    "Verbal refusal" = 98,
    "Not available: UDS form submitted did not collect data in this way, or a skip pattern precludes response to this question" = -4
  )
)

#' Setup REYDLIST method versions
#'
#' @description
#' Registers versions for the %s test class,
#' and sets a default method. Meant to be called in .onLoad
#'
#' @returns
#' `NULL`, invisibly. This function is called for its side effects of
#' registering test versions and setting defaults.
#'
#' @keywords internal
.setup_REYDLIST_versions <- function() {
  var_name <- NULL
  # Register norms versions for REYDLIST
  lookup_table <- normative_summaries$ravlt_trials$REYDLIST
  names(lookup_table)[which(
    names(lookup_table) %in% c("age_group")
  )] <- c("age")

  register_norms_version(
    scores = REYDLIST(),
    version = "ravlt_trials",
    lookup_table = lookup_table,
    covar_fns = list(
      age = \(x) {
        out <- .bincode(x, c(0, 20, 30, 40, 50, 60, 70, 80, Inf), right = FALSE)
        attr(out, "levels") <- c(
          "<20",
          "20-29",
          "30-39",
          "40-49",
          "50-59",
          "60-69",
          "70-79",
          ">79"
        )
        class(out) <- "factor"
        return(out)
      }
    )
  )

  # Register regression versions for REYDLIST

  ## Set the default for %s
  # set_std_defaults(
  #   scores = REYDLIST(),
  #   method = "T-score",
  #   version = "NA"
  # )
}
