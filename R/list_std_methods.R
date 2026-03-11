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
  cls <- S7::S7_class(scores)@name

  all_generics <- .find_std_generics()

  all_generics <- Filter(
    \(g) {
      tryCatch(
        {
          gg <- S7::method(g, S7::S7_class(scores))

          if ("version" %in% names(formals(gg))) {
            # Remove prefix
            g <- sub("^std_using_", "", attr(g, "name") %||% "")

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

  # Extract method names from generic names
  sub("^std_using_", "", names(all_generics))
}


#' Look for generics matching std_using_* in all loaded namespaces
#'
#' @keywords internal
.find_std_generics <- function() {
  all_ns <- c(loadedNamespaces(), ".GlobalEnv")

  result <- list()

  #fns <- unlist(lapply(all_ns, function(ns) {
  for (ns in all_ns) {
    env <- if (ns == ".GlobalEnv") .GlobalEnv else asNamespace(ns)
    nms <- grep("^std_using_", ls(env), value = TRUE)

    # Filter(\(nm) inherits(get(nm, envir = env), "S7_generic"), nms)
    for (nm in nms) {
      obj <- get(nm, envir = env)
      if (inherits(obj, "S7_generic") && !nm %in% names(result)) {
        result[[nm]] <- obj
      }
    }
  }

  # unique(fns)
  result
}
