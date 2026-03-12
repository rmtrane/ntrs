#' @include new_npsych_scores.R
NULL

#' REYDREC Test Scores
#'
#' @description Create a `REYDREC` object to hold REYDREC scores.
#'
#' @param x Numeric scores.
#'
#' @returns
#' An object of class `REYDREC`.
#'
#' @export
REYDREC <- new_npsych_scores(
  "REYDREC",
  label = "RAVLT Long Delay",
  domain = "Memory",
  short_descriptor = "Rey Auditory Verbal Learning (Delayed)  - Total Recall",
  range = c(0, 15),
  codes = c(
    "Not assessed, optional" = 88,
    "Physical problem" = 95,
    "Cognitive/behavior problem" = 96,
    "Other problem" = 97,
    "Verbal refusal" = 98,
    "Not available: UDS form submitted did not collect data in this way, or a skip pattern precludes response to this question" = -4
  )
)

#' Setup REYDREC method versions
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
.setup_REYDREC_versions <- function() {
  var_name <- NULL
  # Register norms versions for REYDREC
  lookup_table <- normative_summaries$ravlt_trials$REYDREC
  names(lookup_table)[which(
    names(lookup_table) %in% c("age_group")
  )] <- c("age")

  register_norms_version(
    scores = REYDREC(),
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

  # Register regression versions for REYDREC

  coefs <- subset(
    reg_coefs[["updated_2024.06"]],
    var_name == "REYDREC"
  )[, setdiff(
    names(reg_coefs[["updated_2024.06"]]),
    "var_name"
  )]

  register_regression_version(
    scores = REYDREC(),
    version = "updated_2024.06",
    coefs = coefs[names(which(sapply(coefs, \(x) !all(is.na(x)))))],
    covar_fns = list(
      age = \(x) {
        x[x < 0] <- 0
        x[x > 110] <- 110

        x
      },
      sex = \(x) {
        as.numeric(x == 2)
      },
      educ = \(x) {
        x[x < 0] <- 0
        x[x > 31] <- 31

        x
      },
      race = \(x) {
        as.numeric(
          data.table::fcase(
            x == 1  ,
            "White" ,
            x == 99 ,
            NA      ,
            default = "Other"
          ) ==
            "Other"
        )
      }
    )
  )

  coefs <- subset(
    reg_coefs[["updated_2025.06"]],
    var_name == "REYDREC"
  )[, setdiff(
    names(reg_coefs[["updated_2025.06"]]),
    "var_name"
  )]

  register_regression_version(
    scores = REYDREC(),
    version = "updated_2025.06",
    coefs = coefs[names(which(sapply(coefs, \(x) !all(is.na(x)))))],
    covar_fns = list(
      age = \(x) {
        x[x < 0] <- 0
        x[x > 110] <- 110

        x
      },
      sex = \(x) {
        as.numeric(x == 2)
      },
      educ = \(x) {
        x[x < 0] <- 0
        x[x > 31] <- 31

        x
      }
    )
  )

  ## Set the default for %s
  set_std_defaults(
    scores = REYDREC(),
    method = "norms",
    version = "ravlt_trials"
  )
}
