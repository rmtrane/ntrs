#' @include new_npsych_scores.R
NULL


#' DIGFORCT Test Scores
#'
#' @description Create a `DIGFORCT` object to hold DIGFORCT scores.
#'
#' @param x Numeric scores.
#'
#' @returns
#' An object of class `DIGFORCT`.
#'
#' @export
DIGFORCT <- new_npsych_scores(
  "DIGFORCT",
  label = "Number Span Forward - Total",
  range = c(0, 14),
  codes = c(
    "Physical problem" = 95,
    "Cognitive/behavior problem" = 96,
    "Other problem" = 97,
    "Verbal refusal" = 98,
    "Not available: UDS form submitted did not collect data in this way, or a skip pattern precludes response to this question" = -4
  )
)

#' Setup DIGFORCT method versions
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
.setup_DIGFORCT_versions <- function() {
  var_name <- NULL
  # Register norms versions for DIGFORCT
  lookup_table <- normative_summaries$nacc$DIGFORCT

  lookup_table$sex <- factor(lookup_table$sex, levels = c("m", "f"))

  register_norms_version(
    scores = DIGFORCT(),
    version = "nacc",
    lookup_table = lookup_table,
    covar_fns = list(
      age = \(x) {
        out <- .bincode(x, c(0, 60, 70, 80, 90, Inf), right = FALSE)
        attr(out, "levels") <- c("<60", "60-69", "70-79", "80-89", ">89")
        class(out) <- "factor"
        return(out)
      },
      educ = \(x) {
        edu_group <- .bincode(x, c(0, 13, 16, 17, Inf), right = FALSE)
        attr(edu_group, "levels") <- c("<13", "13-15", "16", ">16")
        class(edu_group) <- "factor"
        edu_group
      },
      sex = \(x) factor(x, levels = c(1, 2), labels = c("m", "f"))
    )
  )

  lookup_table <- normative_summaries$updated$DIGFORCT

  lookup_table$sex <- factor(lookup_table$sex, levels = c("m", "f"))

  register_norms_version(
    scores = DIGFORCT(),
    version = "updated",
    lookup_table = lookup_table,
    covar_fns = list(
      age = \(x) {
        out <- .bincode(x, c(0, 60, 70, 80, 90, Inf), right = FALSE)
        attr(out, "levels") <- c("<60", "60-69", "70-79", "80-89", ">89")
        class(out) <- "factor"
        return(out)
      },
      educ = \(x) {
        edu_group <- .bincode(x, c(0, 13, 16, 17, Inf), right = FALSE)
        attr(edu_group, "levels") <- c("<13", "13-15", "16", ">16")
        class(edu_group) <- "factor"
        edu_group
      },
      sex = \(x) factor(x, levels = c(1, 2), labels = c("m", "f"))
    )
  )
  # Register regression versions for DIGFORCT
  coefs <- subset(
    reg_coefs[["updated_2024.06"]],
    var_name == "DIGFORCT"
  )[, setdiff(
    names(reg_coefs[["updated_2024.06"]]),
    "var_name"
  )]

  register_regression_version(
    scores = DIGFORCT(),
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
    var_name == "DIGFORCT"
  )[, setdiff(
    names(reg_coefs[["updated_2025.06"]]),
    "var_name"
  )]

  register_regression_version(
    scores = DIGFORCT(),
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
    reg_coefs[["nacc"]],
    var_name == "DIGFORCT"
  )[, setdiff(
    names(reg_coefs[["nacc"]]),
    "var_name"
  )]

  coefs <- stats::setNames(
    as.numeric(coefs),
    names(coefs)
  )

  register_regression_version(
    scores = DIGFORCT(),
    version = "nacc",
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
    scores = DIGFORCT(),
    method = "regression",
    version = "updated_2025.06"
  )
}
