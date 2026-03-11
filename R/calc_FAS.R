#' Calcaulate Functional Assessment Score summary
#'
#' @description
#' Calculates the Functional Assessment Score summary (FAS) from the ten scores of
#' * bills
#' * taxes
#' * shopping
#' * games
#' * stove
#' * meal prep
#' * events
#' * paying attention
#' * remembering appointments
#' * travel
#'
#' Each score is on the scale 0 (Normal), 1 (Has difficulty, but does by self),
#' 2 (Requires assistance), 3 (dependent). Further, error codes of 8 (Not
#' applicable, e.g. never did) and 9 (Unknown) are also allowed, while -4
#' indicates data not collected. The FAS is calculated as the sum of the ten
#' scores where any 8s or 9s are counted as 0.
#'
#' @param BILLS score for ability to write checks, pay bills, or balancing check book.
#' @param TAXES score for ability to assemble tax records, business affairs, or other papers.
#' @param SHOPPING score for shopping alone for clothes, household necessities, or groceries.
#' @param GAMES score for playing a game of skill such as bridge or chess, and working on a hobby
#' @param STOVE score for ability to heat water, make a cup of coffee, turn off the stove
#' @param MEALPREP score for ability to prepare a balanced meal
#' @param EVENTS score for keeping track of current events
#' @param PAYATTN score for ability to pay attention to and understand a TV program, a book, or a magazine
#' @param REMDATES score for remembering appointments, family occasions, holidays, and medications.
#' @param TRAVEL score for traveling out of the neighborhood, driving, or arranging to take public transportation
#'
#' @export
calc_FAS <- function(
  BILLS,
  TAXES,
  SHOPPING,
  GAMES,
  STOVE,
  MEALPREP,
  EVENTS,
  PAYATTN,
  REMDATES,
  TRAVEL
) {
  all_vals <- cbind(
    BILLS,
    TAXES,
    SHOPPING,
    GAMES,
    STOVE,
    MEALPREP,
    EVENTS,
    PAYATTN,
    REMDATES,
    TRAVEL
  )

  ## Check inputs
  for (vals in colnames(all_vals)) {
    if (
      !all(
        is.na(all_vals[, vals]) | all_vals[, vals] %in% ntrs::rdd[[vals]]$codes
      )
    ) {
      cli::cli_abort(
        "{.arg {vals}} must be a numeric vector with entries in {.val {ntrs::rdd[[vals]]$codes}} (see {.val ntrs::rdd${vals}$codes} for details)"
      )
    }
  }

  rs <- rowSums(all_vals == -4 | is.na(all_vals))

  out <- rep(NA, length(rs))

  all_vals[which(
    !(all_vals == 0 | all_vals == 1 | all_vals == 2),
    arr.ind = T
  )] <- 0

  if (sum(rs == 0) == 1) {
    out[rs == 0] <- sum(all_vals[rs == 0, ], na.rm = T)
  } else {
    out[rs == 0] <- rowSums(all_vals[rs == 0, ], na.rm = T)
  }

  out
}

# BILLS <- dat$BILLS
# TAXES <- dat$TAXES
# SHOPPING <- dat$SHOPPING
# GAMES <- dat$GAMES
# STOVE <- dat$STOVE
# MEALPREP <- dat$MEALPREP
# EVENTS <- dat$EVENTS
# PAYATTN <- dat$PAYATTN
# REMDATES <- dat$REMDATES
# TRAVEL <- dat$TRAVEL
