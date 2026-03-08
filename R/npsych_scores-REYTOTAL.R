#' @include new_npsych_scores.R
NULL

#' REYTOTAL Test Scores
#'
#' @description Create a `REYTOTAL` object to hold REYTOTAL scores.
#'
#' @param x Numeric scores.
#'
#' @returns
#' An object of class `REYTOTAL`.
#'
#' @export
REYTOTAL <- new_npsych_scores(
  "REYTOTAL",
  label = "RAVLT Total Learning",
  domain = "Memory",
  short_descriptor = "Sum of REY1REC, ..., REY5REC",
  range = c(0, 75),
  codes = numeric()
)

#' Setup REYTOTAL method versions
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
.setup_REYTOTAL_versions <- function() {
  var_name <- NULL
  # Register norms versions for REYDREC
  lookup_table <- normative_summaries$ravlt_trials$REYTOTAL
  names(lookup_table)[which(
    names(lookup_table) %in% c("age_group")
  )] <- c("age")

  register_norms_version(
    scores = REYTOTAL(),
    version = "ravlt_trials",
    lookup_table = lookup_table,
    covar_fns = list(
      age = \(x) {
        out <- .bincode(x, c(0, 60, 70, 80, 90, Inf), right = FALSE)
        attr(out, "levels") <- c("<60", "60-69", "70-79", "80-89", ">89")
        class(out) <- "factor"
        return(out)
      }
    )
  )

  # Register regression versions for REYTOTAL
  coefs <- subset(
    reg_coefs[["updated_2024.06"]],
    var_name == "REYTOTAL"
  )[, setdiff(
    names(reg_coefs[["updated_2024.06"]]),
    "var_name"
  )]

  register_regression_version(
    scores = REYTOTAL(),
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
    var_name == "REYTOTAL"
  )[, setdiff(
    names(reg_coefs[["updated_2025.06"]]),
    "var_name"
  )]

  register_regression_version(
    scores = REYTOTAL(),
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
      }
    )
  )
  ## Set the default for %s
  # set_std_defaults(
  #   scores = REYTOTAL(),
  #   method = "T-score",
  #   version = "NA"
  # )
}
