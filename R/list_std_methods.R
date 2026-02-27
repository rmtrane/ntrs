#' List available standardization methods for `npsych_scores`
#'
#' @description Get methods that are available for a test class, or all
#'   available methods.
#'
#' @param scores A `npsych_scores` object, such as `MOCATOTS()`. When the
#'   argument is supplied, only methods with registered versions for that
#'   specific test class are returned. When called via the `.npsych_scores`
#'   method with a missing argument, all registered methods are returned.
#'
#' @returns A character vector of standardization method names.
#'
#' @export
list_std_methods <- function(scores) {
  UseMethod("list_std_methods")
}

#' @export
list_std_methods.npsych_scores <- function(scores) {
  cls <- setdiff(class(scores), "npsych_scores")

  # Source 1: the version registry
  from_registry <- {
    with_npsych_scores <- lapply(as.list(.std_versions), ls)

    with_npsych_scores <- sapply(with_npsych_scores, \(x) cls %in% x)

    if (length(with_npsych_scores) > 0) {
      names(which(with_npsych_scores))
    } else {
      NULL
    }
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
  Filter(\(m) .is_valid_std_method(m, scores), candidates)
}

#' @keywords internal
.is_valid_std_method <- function(method, scores) {
  if (!is.character(method) || length(method) != 1 || method == "") {
    cli::cli_abort("{.arg method} must be a non-empty character string.")
  }

  if (!inherits(scores, "npsych_scores")) {
    cli::cli_abort(
      "{.arg npsych_scores} must be a {.cls npsych_scores} object."
    )
  }

  generic_name <- paste0("std_using_", method)

  fn <- tryCatch(match.fun(generic_name), error = \(e) NULL)

  if (is.null(fn) || !sloop::is_s3_generic(generic_name)) {
    return(FALSE)
  }

  call <- rlang::parse_expr(paste0(generic_name, "(scores)"))
  dispatches <- tryCatch(
    rlang::inject(sloop::s3_dispatch(!!call)),
    error = \(e) NULL
  )

  !is.null(dispatches) && any(dispatches$exists)
}
