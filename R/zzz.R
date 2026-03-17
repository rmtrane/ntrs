.datatable.aware <- TRUE

.onLoad <- function(libname, pkgname) {
  # nocov start
  S7::methods_register()

  ## Run all .setup_CLASS_versions()
  ns <- getNamespace("ntrs")

  invisible(lapply(
    grep(pattern = "^\\.setup_.+_versions", x = names(ns), value = T),
    \(x) {
      # message(x)
      do.call(x, args = list())
    }
  ))
} # nocov end
