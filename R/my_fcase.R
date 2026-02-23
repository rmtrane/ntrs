#' Apply conditional logic to create a vector
#'
#' @description Quick implementation of `data.table::fcase` to avoid dependency. A function that takes a sequence of paired arguments, where each pair consists of a condition (a logical vector) followed by a value (a vector). The function evaluates the conditions in order and assigns the corresponding values to the output vector. If no conditions are met, it assigns a default value.
#'
#' @param ... A sequence of paired arguments. Each pair should consist of a
#'   condition (a logical vector) followed by a value (a vector).
#' @param default The default value to return if no condition is met. Optional.
#'
#' @returns
#' A vector whose elements are determined by the provided conditions and values.
#' The function will stop with an error if `...` does not contain an even number
#' of arguments.
#'
#' @keywords internal
my_fcase <- function(..., default = NA) {
  args <- list(...)
  n <- length(args)
  if (n %% 2 != 0) {
    stop("args must be paired: condition, value, condition, value, ...")
  }

  len <- length(args[[1]])
  result <- rep(default, len)

  for (i in seq(n, 1, by = -2)) {
    cond <- args[[i - 1]]
    val <- args[[i]]

    if (length(cond) == 1 && isTRUE(cond)) {
      cond <- rep(TRUE, len)
    }

    result[cond] <- val[if (length(val) == len) cond else 1]
  }

  result
}
