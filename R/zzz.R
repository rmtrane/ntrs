.datatable.aware <- TRUE

.set_defaults <- function() {
  ## Run all .setup_CLASS_versions()
  ntrs:::.setup_ANIMALS_versions()
  ntrs:::.setup_BOSTON_versions()
  ntrs:::.setup_CRAFTDRE_versions()
  ntrs:::.setup_CRAFTDVR_versions()
  ntrs:::.setup_CRAFTURS_versions()
  ntrs:::.setup_CRAFTVRS_versions()
  ntrs:::.setup_DIGBACCT_versions()
  ntrs:::.setup_DIGBACLS_versions()
  ntrs:::.setup_DIGFORCT_versions()
  ntrs:::.setup_DIGFORSL_versions()
  ntrs:::.setup_DIGIB_versions()
  ntrs:::.setup_DIGIBLEN_versions()
  ntrs:::.setup_DIGIF_versions()
  ntrs:::.setup_DIGIFLEN_versions()
  ntrs:::.setup_LOGIMEM_versions()
  ntrs:::.setup_MEMUNITS_versions()
  ntrs:::.setup_MINTTOTS_versions()
  ntrs:::.setup_MOCATOTS_versions()
  ntrs:::.setup_NACCMMSE_versions()
  ntrs:::.setup_OTRAILA_versions()
  ntrs:::.setup_OTRAILB_versions()
  ntrs:::.setup_OTRLARR_versions()
  ntrs:::.setup_OTRLBRR_versions()
  ntrs:::.setup_REY6REC_versions()
  ntrs:::.setup_REYAREC_versions()
  ntrs:::.setup_REYDLIST_versions()
  ntrs:::.setup_REYDREC_versions()
  ntrs:::.setup_REYTOTAL_versions()
  ntrs:::.setup_TRAILA_versions()
  ntrs:::.setup_TRAILB_versions()
  ntrs:::.setup_UDSBENTC_versions()
  ntrs:::.setup_UDSBENTD_versions()
  ntrs:::.setup_UDSVERFC_versions()
  ntrs:::.setup_UDSVERLC_versions()
  ntrs:::.setup_UDSVERTN_versions()
  ntrs:::.setup_VEG_versions()
  ntrs:::.setup_WAIS_versions()
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

  ntrs:::.set_defaults()
} # nocov end
