#' @include new_npsych_scores.R
NULL


#' DIGIF Test Scores
#'
#' @description Create a `DIGIF` object to hold DIGIF scores.
#'
#' @param x Numeric scores.
#'
#' @returns
#' An object of class `DIGIF`.
#'
#' @export
DIGIF <- new_npsych_scores(
  "DIGIF",
  label = "Digit Span Forward - Total",
  range = c(0, 12),
  codes = c(
    "Physical problem" = 95,
    "Cognitive/behavior problem" = 96,
    "Other problem" = 97,
    "Verbal refusal" = 98,
    "Not available: UDS form submitted" = -4
  )
)

#' Setup DIGIF method versions
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
.setup_DIGIF_versions <- function() {
  var_name <- NULL
  # Register regression versions for DIGIF
  coefs <- subset(
    reg_coefs[["updated_2024.06"]],
    var_name == "DIGIF"
  )[, setdiff(
    names(reg_coefs[["updated_2024.06"]]),
    "var_name"
  )]

  register_regression_version(
    scores = DIGIF(),
    version = "updated_2024.06",
    coefs = coefs[names(which(unlist(lapply(coefs, \(x) any(!is.na(x))))))],
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
    var_name == "DIGIF"
  )[, setdiff(
    names(reg_coefs[["updated_2025.06"]]),
    "var_name"
  )]

  register_regression_version(
    scores = DIGIF(),
    version = "updated_2025.06",
    coefs = coefs[names(which(unlist(lapply(coefs, \(x) any(!is.na(x))))))],
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
    reg_coefs[["nacc_legacy"]],
    var_name == "DIGIF"
  )[, setdiff(
    names(reg_coefs[["nacc_legacy"]]),
    "var_name"
  )]

  coefs <- stats::setNames(
    as.numeric(coefs),
    names(coefs)
  )

  register_regression_version(
    scores = DIGIF(),
    version = "nacc_legacy",
    coefs = c(stats::na.omit(coefs)),
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
    scores = DIGIF(),
    method = "regression",
    version = "nacc_legacy"
  )
}
