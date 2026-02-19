#' List `npsych_scores` subclasses
#'
#' @description
#' This function is a helper that lists all available subclasses, i.e. all functions created using `new_class <- function(scores) { npsych_scores(...,subclass = "new_class")}`.
#'
#' @returns
#' A character vector of valid test subclass names.
#'
#' @export
list_npsych_scores <- function() {
  # Source 1: the version registry
  from_registry <- {
    registered_methods <- ls(.std_versions)
    unique(unlist(lapply(registered_methods, \(m) {
      ls(envir = .std_versions[[m]])
    })))
  }

  # Source 2: S3 methods for std_using_* generics
  # We search by naming convention directly rather than using utils::methods(),
  # which only sees formally registered methods (i.e. those in loaded package
  # namespaces) and misses anything defined in .GlobalEnv or local scopes.
  from_s3 <- {
    all_fns <- c(
      # Loaded package namespaces and interanl
      unlist(lapply(c(loadedNamespaces(), "NpsychBatteryNormsS3"), \(ns) {
        tryCatch(ls(getNamespace(ns)), error = \(e) character(0))
      })),
      # Global environment (user-defined methods)
      ls(envir = .GlobalEnv)
    )

    # Direct pattern match on std_using_<generic>.<class> naming convention
    method_fns <- grep("^std_using_[^.]+\\.", all_fns, value = TRUE)
    classes <- sub("^std_using_[^.]+\\.", "", method_fns)
    setdiff(unique(classes), "npsych_scores")
  }

  candidates <- unique(c(from_registry, from_s3))

  Filter(.is_valid_npsych_scores, candidates)
}


#' Check if a class is a valid test subclass
#'
#' @param cls A string or symbol representing a class name.
#'
#' @returns
#' A logical value indicating whether `cls` is a valid subclass of "npsych_scores".
#'
#' @keywords internal
.is_valid_npsych_scores <- function(cls) {
  if (!is.character(cls)) {
    cli::cli_abort(
      "{.arg cls} must be of class {.cls character}, but is of class {.cls {class(cls)}}."
    )
  }

  if (length(cls) != 1) {
    cli::cli_abort(
      "{.arg cls} must be a single string, but is a character vector of length {.val {length(cls)}}."
    )
  }

  fn <- tryCatch(match.fun(cls), error = \(e) NULL)
  if (is.null(fn)) {
    return(FALSE)
  }
  obj <- tryCatch(fn(), error = \(e) NULL)
  inherits(obj, "npsych_scores") && cls %in% class(obj)
}
