# Test file for register_regression_version() / register_regression_version.npsych_scores()

# Helpers ----

valid_coefs_vec <- c(intercept = 20, age = -0.1, educ = 0.5, rmse = 3.2)

valid_coefs_df <- data.frame(
  intercept = 20,
  age = -0.1,
  educ = 0.5,
  rmse = 3.2
)

# Test 1: Delegation to .validate_registration_params() ----

test_that("register_regression_version() rejects invalid version parameter", {
  testthat::local_reproducible_output()
  expect_error(
    register_regression_version(
      scores = MOCATOTS(),
      version = NULL,
      coefs = valid_coefs_vec
    ),
    "must be a non-empty character string"
  )

  testthat::local_reproducible_output()
  expect_error(
    register_regression_version(
      scores = MOCATOTS(),
      version = 123,
      coefs = valid_coefs_vec
    ),
    "must be a non-empty character string"
  )

  testthat::local_reproducible_output()
  expect_error(
    register_regression_version(
      scores = MOCATOTS(),
      version = "",
      coefs = valid_coefs_vec
    ),
    "must be a non-empty character string"
  )

  testthat::local_reproducible_output()
  expect_error(
    register_regression_version(
      scores = MOCATOTS(),
      version = c("v1", "v2"),
      coefs = valid_coefs_vec
    ),
    "must be a non-empty character string"
  )
})

test_that("register_regression_version() rejects invalid description parameter", {
  testthat::local_reproducible_output()
  expect_error(
    register_regression_version(
      scores = MOCATOTS(),
      version = "v1",
      coefs = valid_coefs_vec,
      description = NULL
    ),
    "must be a character string"
  )

  testthat::local_reproducible_output()
  expect_error(
    register_regression_version(
      scores = MOCATOTS(),
      version = "v1",
      coefs = valid_coefs_vec,
      description = 123
    ),
    "must be a character string"
  )

  testthat::local_reproducible_output()
  expect_error(
    register_regression_version(
      scores = MOCATOTS(),
      version = "v1",
      coefs = valid_coefs_vec,
      description = c("a", "b")
    ),
    "must be a character string"
  )
})

# Test 2: coefs class validation ----

test_that("register_regression_version() rejects coefs that are not numeric or data.frame", {
  testthat::local_reproducible_output()
  expect_error(
    register_regression_version(
      scores = MOCATOTS(),
      version = "v1",
      coefs = list(intercept = 20, rmse = 3)
    ),
    "must be a.*numeric.*data.frame"
  )

  testthat::local_reproducible_output()
  expect_error(
    register_regression_version(
      scores = MOCATOTS(),
      version = "v1",
      coefs = "intercept=20"
    ),
    "must be a.*numeric.*data.frame"
  )
})

# Test 3: numeric coefs cannot contain NAs ----

test_that("register_regression_version() rejects numeric coefs with NA values", {
  coefs_with_na <- c(intercept = 20, age = NA, rmse = 3.2)

  testthat::local_reproducible_output()
  expect_error(
    register_regression_version(
      scores = MOCATOTS(),
      version = "v1",
      coefs = coefs_with_na
    ),
    "cannot contain missing values"
  )
})

# Test 4: data.frame coefs cannot have all-NA columns ----

test_that("register_regression_version() rejects data.frame coefs with all-NA column", {
  coefs_all_na_col <- data.frame(
    intercept = 20,
    age = NA_real_,
    rmse = 3.2
  )

  testthat::local_reproducible_output()
  expect_error(
    register_regression_version(
      scores = MOCATOTS(),
      version = "v1",
      coefs = coefs_all_na_col
    ),
    "only missing values"
  )
})

test_that("register_regression_version() accepts data.frame coefs with partial NAs (multi-row)", {
  # A data.frame with NAs is fine as long as each column has at least one non-NA
  coefs_partial_na <- data.frame(
    intercept = c(20, 21),
    age = c(-0.1, NA),
    rmse = c(3.2, 3.5)
  )

  expect_no_error(
    suppressMessages(
      register_regression_version(
        scores = MOCATOTS(),
        version = "test_partial_na_df_001",
        coefs = coefs_partial_na
      )
    )
  )
})

# Test 5: coefs must be named ----

test_that("register_regression_version() rejects unnamed coefs", {
  unnamed_vec <- c(20, -0.1, 3.2)

  testthat::local_reproducible_output()
  expect_error(
    register_regression_version(
      scores = MOCATOTS(),
      version = "v1",
      coefs = unnamed_vec
    ),
    "must be named"
  )
})

# Test 6: coefs must include rmse ----

test_that("register_regression_version() rejects coefs missing rmse", {
  no_rmse <- c(intercept = 20, age = -0.1, educ = 0.5)

  testthat::local_reproducible_output()
  expect_error(
    register_regression_version(
      scores = MOCATOTS(),
      version = "v1",
      coefs = no_rmse
    ),
    "must include.*rmse"
  )
})

# Test 7: coefs names must all be from the allowed set ----

test_that("register_regression_version() rejects coefs with disallowed names", {
  bad_names <- c(intercept = 20, weight = 0.3, rmse = 3.2)

  testthat::local_reproducible_output()
  expect_error(
    register_regression_version(
      scores = MOCATOTS(),
      version = "v1",
      coefs = bad_names
    ),
    "must only contain a subset"
  )
})

# Test 8: covar_fns validation ----

test_that("register_regression_version() rejects non-list covar_fns", {
  testthat::local_reproducible_output()
  expect_error(
    register_regression_version(
      scores = MOCATOTS(),
      version = "v1",
      coefs = valid_coefs_vec,
      covar_fns = function(x) x
    ),
    "must be a.*list"
  )
})

test_that("register_regression_version() rejects covar_fns names not in coefs", {
  bad_fns <- list(age = identity, weight = identity)

  testthat::local_reproducible_output()
  expect_error(
    register_regression_version(
      scores = MOCATOTS(),
      version = "v1",
      coefs = valid_coefs_vec,
      covar_fns = bad_fns
    ),
    "must be a subset"
  )
})

# Test 9: successful registration (numeric coefs, no covar_fns) ----

test_that("register_regression_version() registers successfully with numeric coefs", {
  expect_no_error(
    suppressMessages(
      register_regression_version(
        scores = MOCATOTS(),
        version = "test_reg_vec_001",
        coefs = valid_coefs_vec
      )
    )
  )

  versions <- list_method_versions(MOCATOTS(), "regression")
  expect_true("test_reg_vec_001" %in% versions)
})

# Test 10: successful registration (data.frame coefs) ----

test_that("register_regression_version() registers successfully with data.frame coefs", {
  expect_no_error(
    suppressMessages(
      register_regression_version(
        scores = MOCATOTS(),
        version = "test_reg_df_001",
        coefs = valid_coefs_df
      )
    )
  )

  versions <- list_method_versions(MOCATOTS(), "regression")
  expect_true("test_reg_df_001" %in% versions)
})

# Test 11: successful registration with covar_fns ----

test_that("register_regression_version() registers successfully with valid covar_fns", {
  fns <- list(
    age = function(x) pmin(pmax(x, 0), 110),
    educ = function(x) pmin(pmax(x, 0), 31)
  )

  expect_no_error(
    suppressMessages(
      register_regression_version(
        scores = MOCATOTS(),
        version = "test_reg_fns_001",
        coefs = valid_coefs_vec,
        covar_fns = fns
      )
    )
  )

  versions <- list_method_versions(MOCATOTS(), "regression")
  expect_true("test_reg_fns_001" %in% versions)
})

# Test 12: stored data integrity ----

test_that("register_regression_version() stores coefs correctly in registry", {
  suppressMessages(
    register_regression_version(
      scores = MOCATOTS(),
      version = "test_reg_integrity_001",
      coefs = valid_coefs_vec
    )
  )

  stored <- .std_versions[["regression"]][["MOCATOTS"]][[
    "test_reg_integrity_001"
  ]]

  expect_equal(stored$subclass, "MOCATOTS")
  expect_equal(stored$method, "regression")
  expect_equal(stored$version, "test_reg_integrity_001")
  expect_equal(stored$data$coefs, valid_coefs_vec)
})

test_that("register_regression_version() stores covar_fns in registry when provided", {
  fns <- list(age = identity)

  suppressMessages(
    register_regression_version(
      scores = MOCATOTS(),
      version = "test_reg_integrity_fns_001",
      coefs = valid_coefs_vec,
      covar_fns = fns
    )
  )

  stored <- .std_versions[["regression"]][["MOCATOTS"]][[
    "test_reg_integrity_fns_001"
  ]]
  expect_true("covar_fns" %in% names(stored$data))
  expect_equal(stored$data$covar_fns, fns)
})

test_that("register_regression_version() does not store covar_fns when omitted", {
  withr::defer({
    rm(
      "test_reg_no_fns_001",
      envir = .std_versions[["regression"]][["MOCATOTS"]]
    )
  })
  suppressMessages(
    register_regression_version(
      scores = MOCATOTS(),
      version = "test_reg_no_fns_001",
      coefs = valid_coefs_vec
    )
  )

  stored <- .std_versions[["regression"]][["MOCATOTS"]][["test_reg_no_fns_001"]]
  expect_false("covar_fns" %in% names(stored$data))
})

# Test 13: S3 generic exists ----

test_that("register_regression_version() is an S3 generic", {
  skip_on_covr()
  expect_true(is.function(register_regression_version))
  expect_true(utils::isS3stdGeneric(register_regression_version))
})

# Test 14: returns invisibly ----

test_that("register_regression_version() returns invisibly", {
  result <- withVisible(
    suppressMessages(
      register_regression_version(
        scores = MOCATOTS(),
        version = "test_reg_invisible_001",
        coefs = valid_coefs_vec
      )
    )
  )

  expect_false(result$visible)
  expect_null(result$value)
})
