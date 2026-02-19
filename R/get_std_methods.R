#' Get standardization methods for test scores
#'
#' @description Get methods that are available for a test class, or all
#'   available methods.
#'
#' @param test_class A `test_scores` object, such as `MOCATOTS()`. When the
#'   argument is supplied, only methods with registered versions for that
#'   specific test class are returned. When called via the `.test_scores`
#'   method with a missing argument, all registered methods are returned.
#'
#' @returns A character vector of standardization method names.
#'
#' @export
get_std_methods <- function(test_class) {
  UseMethod("get_std_methods")
}

#' @export
get_std_methods.test_scores <- function(test_class) {
  cls <- setdiff(class(test_class), "test_scores")

  # Source 1: the version registry
  from_registry <- {
    with_test_class <- lapply(as.list(.std_versions), ls) |>
      sapply(\(x) cls %in% x)
    setdiff(names(which(with_test_class)), "defaults")
  }

  # Source 2: S3 name scan — fix the class, extract the method
  from_s3 <- {
    all_fns <- c(
      unlist(lapply(loadedNamespaces(), \(ns) {
        tryCatch(ls(getNamespace(ns)), error = \(e) character(0))
      })),
      ls(envir = .GlobalEnv)
    )
    pattern <- paste0("^std_using_[^.]+\\.", cls, "$")
    matched <- grep(pattern, all_fns, value = TRUE)
    sub(paste0("^std_using_(.*?)\\.", cls, "$"), "\\1", matched)
  }

  candidates <- unique(c(from_registry, from_s3))
  Filter(\(m) .is_valid_std_method(m, test_class), candidates)
}

#' @keywords internal
.is_valid_std_method <- function(method, test_class) {
  if (!is.character(method) || length(method) != 1 || method == "") {
    cli::cli_abort("{.arg method} must be a non-empty character string.")
  }

  if (!inherits(test_class, "test_scores")) {
    cli::cli_abort("{.arg test_class} must be a {.cls test_scores} object.")
  }

  generic_name <- paste0("std_using_", method)

  fn <- tryCatch(match.fun(generic_name), error = \(e) NULL)
  if (is.null(fn) || !utils::isS3stdGeneric(fn)) {
    return(FALSE)
  }

  call <- rlang::parse_expr(paste0(generic_name, "(test_class)"))
  dispatches <- tryCatch(
    rlang::inject(sloop::s3_dispatch(!!call)),
    error = \(e) NULL
  )

  !is.null(dispatches) && any(dispatches$exists)
}
