.datatable.aware <- TRUE

.set_defaults <- function() {
  ## Run all .setup_CLASS_versions()
  .setup_ANIMALS_versions()
  .setup_BOSTON_versions()
  .setup_CRAFTDRE_versions()
  .setup_CRAFTDVR_versions()
  .setup_CRAFTURS_versions()
  .setup_CRAFTVRS_versions()
  .setup_DIGBACCT_versions()
  .setup_DIGBACLS_versions()
  .setup_DIGFORCT_versions()
  .setup_DIGFORSL_versions()
  .setup_DIGIB_versions()
  .setup_DIGIBLEN_versions()
  .setup_DIGIF_versions()
  .setup_DIGIFLEN_versions()
  .setup_LOGIMEM_versions()
  .setup_MEMUNITS_versions()
  .setup_MINTTOTS_versions()
  .setup_MOCATOTS_versions()
  .setup_NACCMMSE_versions()
  .setup_OTRAILA_versions()
  .setup_OTRAILB_versions()
  .setup_OTRLARR_versions()
  .setup_OTRLBRR_versions()
  .setup_REY6REC_versions()
  .setup_REYAREC_versions()
  .setup_REYDLIST_versions()
  .setup_REYDREC_versions()
  .setup_REYTOTAL_versions()
  .setup_TRAILA_versions()
  .setup_TRAILB_versions()
  .setup_UDSBENTC_versions()
  .setup_UDSBENTD_versions()
  .setup_UDSVERFC_versions()
  .setup_UDSVERLC_versions()
  .setup_UDSVERTN_versions()
  .setup_VEG_versions()
  .setup_WAIS_versions()
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
