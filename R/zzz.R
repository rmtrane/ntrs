.datatable.aware <- TRUE

.set_defaults <- function(overwrite = T) {
  ## Run all .setup_CLASS_versions()
  ns <- getNamespace("ntrs")

  invisible(lapply(
    grep(pattern = "^\\.setup_.+_versions", x = names(ns), value = T),
    \(x) {
      # message(x)
      do.call(x, args = list(overwrite = overwrite))
    }
  ))
}

.onLoad <- function(libname, pkgname) {
  # nocov start
  S7::methods_register()

  ## S7 sets S3 classes to "ntrs::<name>" (namespace-qualified),
  ## so roxygen2's S3method() directives don't match. Register manually.
  registerS3method("c", "ntrs::npsych_scores", c.npsych_scores)
  registerS3method("[<-", "ntrs::npsych_scores", `[<-.npsych_scores`)
  registerS3method("c", "ntrs::std_npsych_scores", c.std_npsych_scores)
  registerS3method("[<-", "ntrs::std_npsych_scores", `[<-.std_npsych_scores`)

  .set_defaults()
} # nocov end
