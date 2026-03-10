# Test file for internal registry functions

# .register_std_version() tests ----

test_that(".register_std_version() is a function", {
  expect_true(is.function(.register_std_version))
})

test_that(".register_std_version() rejects non-std_version objects", {
  testthat::local_reproducible_output()

  expect_error(
    .register_std_version(list(foo = 1)),
    regexp = "std_version"
  )

  expect_error(
    .register_std_version("not a version"),
    regexp = "std_version"
  )
})

test_that(".register_std_version() errors when no S7 generic exists for method", {
  testthat::local_reproducible_output()

  v <- norms_version(
    scores_class = "MOCATOTS",
    version_id = "fake_generic_test",
    lookup_table = data.frame(m = 1, sd = 1, age = 60),
    covar_fns = list(age = identity)
  )
  # Temporarily rename method_name to something with no generic
  v@method_name <- "nonexistent_method"

  expect_error(
    .register_std_version(v),
    regexp = "generic"
  )
})

test_that(".register_std_version() creates method environment when needed", {
  v <- norms_version(
    scores_class = "MOCATOTS",
    version_id = "env_creation_test_v1",
    lookup_table = data.frame(m = 1, sd = 1, age = 60),
    covar_fns = list(age = identity)
  )

  .register_std_version(v)

  expect_true(exists("norms", envir = .std_versions, inherits = FALSE))
})

test_that(".register_std_version() creates scores class environment when needed", {
  # Use a freshly-created subclass to guarantee a new scores env
  test_cls <- new_npsych_scores(
    "RegTestClass1",
    label = "Reg Test",
    range = c(0, 100)
  )

  ## Assign to global environment so test works
  assign("RegTestClass1", test_cls, envir = .GlobalEnv)
  withr::defer(rm("RegTestClass1", envir = .GlobalEnv))

  v <- regression_version(
    scores_class = "RegTestClass1",
    version_id = "env_test_v1",
    coefs = c(intercept = 10, rmse = 5, age = 0.5),
    covar_fns = list(age = identity)
  )

  .register_std_version(v)

  expect_true(
    exists(
      "RegTestClass1",
      envir = .std_versions[["regression"]],
      inherits = FALSE
    )
  )
})

test_that(".register_std_version() registers version and stores S7 object", {
  v <- norms_version(
    scores_class = "MOCATOTS",
    version_id = "register_test_v1",
    lookup_table = data.frame(m = 25, sd = 3, age = 60),
    covar_fns = list(age = identity)
  )

  .register_std_version(v)

  stored <- .std_versions[["norms"]][["MOCATOTS"]][["register_test_v1"]]
  expect_true(S7::S7_inherits(stored, norms_version))
  expect_equal(stored@version_id, "register_test_v1")
})

test_that(".register_std_version() prevents duplicate registration by default", {
  v <- norms_version(
    scores_class = "MOCATOTS",
    version_id = "dup_test_v1",
    lookup_table = data.frame(m = 25, sd = 3, age = 60),
    covar_fns = list(age = identity)
  )

  .register_std_version(v)

  testthat::local_reproducible_output()

  expect_error(
    .register_std_version(v),
    regexp = "already exists"
  )
})

test_that(".register_std_version() allows overwrite when specified", {
  v1 <- norms_version(
    scores_class = "MOCATOTS",
    version_id = "overwrite_test_v1",
    lookup_table = data.frame(m = 25, sd = 3, age = 60),
    covar_fns = list(age = identity)
  )

  .register_std_version(v1)

  v2 <- norms_version(
    scores_class = "MOCATOTS",
    version_id = "overwrite_test_v1",
    lookup_table = data.frame(m = 30, sd = 4, age = 60),
    covar_fns = list(age = identity)
  )

  expect_warning(
    .register_std_version(v2, overwrite = TRUE),
    "Overwriting"
  )
})
