test_that(".register_std_version errors when version_obj is not correct class", {
  vers_obj <- std_version(
    scores_class = "NEWSCORES",
    method_name = "regression",
    version_id = "test_version_obj_001",
    description = ""
  )

  expect_error(
    .register_std_version(vers_obj),
    "No npsych_scores subclass.+NEWSCORES.+ found in any loaded namespace."
  )
})


test_that(".register_std_version errors method_name not available for scores_class", {
  testthat::local_reproducible_output()

  assign(
    "std_using_new_method",
    S7::new_generic("std_using_new_method", "scores"),
    envir = .GlobalEnv
  )

  withr::defer({
    rm(std_using_new_method, envir = .GlobalEnv)
  })

  S7::method(std_using_new_method, MOCATOTS) <- function(scores, ...) {
    scores
  }

  vers_obj <- std_version(
    scores_class = "WAIS",
    method_name = "new_method",
    version_id = "test_version_obj_001",
    description = ""
  )

  expect_error(
    .register_std_version(version_obj = vers_obj),
    "Method.+new_method.+not implemented for .+WAIS.+."
  )
})

test_that(".register_std_version creates new environment when necessary", {
  testthat::local_reproducible_output()

  assign(
    "std_using_new_method",
    S7::new_generic("std_using_new_method", "scores"),
    envir = .GlobalEnv
  )

  withr::defer({
    rm(std_using_new_method, envir = .GlobalEnv)
  })

  S7::method(std_using_new_method, WAIS) <- function(scores, ...) {
    scores
  }

  vers_obj <- std_version(
    scores_class = "WAIS",
    method_name = "new_method",
    version_id = "test_version_obj_001",
    description = ""
  )

  expect_no_error(
    .register_std_version(version_obj = vers_obj)
  )

  expect_true(
    exists("new_method", envir = .std_versions, inherits = FALSE)
  )

  expect_true(
    "new_method" %in% list_std_methods(WAIS())
  )

  expect_true(
    "test_version_obj_001" %in% list_method_versions(WAIS(), "new_method")
  )
})
