#' REYDLIST Test Scores
#'
#' @description Create a `REYDLIST` object to hold REYDLIST scores.
#'
#' @param scores Numeric scores.
#'
#' @returns
#' An object of class `REYDLIST`.
#'
#' @export
REYDLIST <- function(scores = numeric()) {
  ts <- npsych_scores(
    scores,
    label = "RAVLT Distractor List",
    range = c(0, 15),
    # codes = c("" = ),
    class = "REYDLIST"
  )

  ts
}

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
  lookup_table <- NpsychBatteryNorms::normative_summaries$ravlt_trials$REYDLIST
  names(lookup_table)[which(
    names(lookup_table) %in% c("age_group")
  )] <- c("age")

  register_norms_version(
    scores = REYDLIST(),
    version = "ravlt_trials",
    lookup_table = lookup_table,
    covar_fns = list(
      age = \(x) NpsychBatteryNorms::get_age_group(x, "ravlt_trials")
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
