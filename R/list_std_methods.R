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
list_std_methods <- function(scores, debug = FALSE) {
  cls <- S7::S7_class(scores)@name

  # Source 1: version registry (methods with registered data)
  # from_registry <- {
  #   with_class <- vapply(
  #     ls(.std_versions),
  #     \(m) exists(cls, envir = .std_versions[[m]], inherits = FALSE),
  #     logical(1)
  #   )
  #   names(which(with_class))
  # }

  # Source 2: S7 method check for known std_using_* generics
  # from_s7 <- {
  # Scan for std_using_* generics in loaded namespaces

  if (debug) {
    browser()
  }

  all_generics <- .find_std_generics()
  all_generics <- Filter(
    \(g) {
      tryCatch(
        {
          gg <- S7::method(get(g), S7::S7_class(scores))

          if ("version" %in% names(formals(gg))) {
            # Remove prefix
            g <- sub("^std_using_", "", g)

            # If method has version argument, check if any version is registered for class
            vers_exists <- exists(g, .std_versions, inherits = FALSE) &&
              exists(
                cls,
                envir = .std_versions[[sub("^std_using_", "", g)]],
                inherits = FALSE
              )

            return(vers_exists)
          }

          TRUE
        },
        error = \(e) FALSE
      )
    },
    all_generics
  )
  # }

  # Extract method names from generic names
  return(sub("^std_using_", "", all_generics))

  # unique(c(from_registry, methods_from_s7))
}


#' Look for generics matching std_using_* in all loaded namespaces
#'
#' @keywords internal
.find_std_generics <- function() {
  all_ns <- c(loadedNamespaces(), ".GlobalEnv")

  fns <- unlist(lapply(all_ns, function(ns) {
    env <- if (ns == ".GlobalEnv") {
      .GlobalEnv
    } else {
      asNamespace(ns)
    }
    nms <- grep("^std_using_", ls(env), value = TRUE)
    Filter(\(nm) inherits(get(nm, envir = env), "S7_generic"), nms)
  }))

  unique(fns)
}
