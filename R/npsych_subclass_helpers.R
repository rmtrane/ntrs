# Cache lives in a local environment so it's invisible to users
.npsych_cache <- local({
  env <- new.env(parent = emptyenv())
  env$ns_snapshot <- NULL # character vector from loadedNamespaces()
  env$classes <- list() # named list: name -> S7 class object
  env
})

#' Check whether an S7 class inherits from npsych_scores
#'
#' Walks the S7 parent chain. Returns TRUE if npsych_scores appears
#' anywhere (but is not npsych_scores itself — we only want subclasses).
#'
#' @param cls An S7 class object.
#' @returns Logical.
#' @keywords internal
.is_npsych_subclass <- function(cls) {
  inherits(cls, "S7_class") &&
    inherits(cls@parent, "S7_class") &&
    cls@parent@name == "npsych_scores"
}

#' Discover all npsych_scores subclasses across loaded namespaces
#'
#' Scans every loaded namespace (and optionally .GlobalEnv) for exported
#' or internal objects that are S7 classes inheriting from npsych_scores.
#' Results are cached and invalidated when loadedNamespaces() changes
#' (i.e., when a new package is loaded or unloaded).
#'
#' @returns A named list of S7 class objects (name -> class object).
#' @keywords internal
.find_npsych_classes <- function() {
  current_ns <- loadedNamespaces()

  # Return cache if namespace set hasn't changed
  if (identical(current_ns, .npsych_cache$ns_snapshot)) {
    return(.npsych_cache$classes)
  }

  result <- list()

  for (ns_name in current_ns) {
    ns <- asNamespace(ns_name)

    for (obj_name in ls(ns)) {
      obj <- get(obj_name, envir = ns)

      if (.is_npsych_subclass(obj)) {
        result[[obj@name]] <- obj
      }
    }
  }

  # Also check .GlobalEnv for interactive use / devtools::load_all()
  for (obj_name in ls(.GlobalEnv)) {
    obj <- get(obj_name, envir = .GlobalEnv)
    if (.is_npsych_subclass(obj)) {
      result[[obj@name]] <- obj
    }
  }

  # Update cache
  .npsych_cache$ns_snapshot <- current_ns
  .npsych_cache$classes <- result

  result
}
