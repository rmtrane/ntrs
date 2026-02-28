# Test file for register_regression_version()

# Helpers ----

valid_coefs_vec <- c(intercept = 20, age = -0.1, educ = 0.5, rmse = 3.2)

valid_coefs_df <- data.frame(
  intercept = 20,
  age = -0.1,
  educ = 0.5,
  rmse = 3.2
)


# ---------------------------------------------------------------------------
# Input validation: scores
# ---------------------------------------------------------------------------

test_that("register_regression_version() rejects non-npsych_scores object", {
  testthat::local_reproducible_output()

  expect_error(
    register_regression_version(
      scores = 42,
      version = "v1",
      coefs = valid_coefs_vec,
      covar_fns = list(age = identity, educ = identity)
    ),
    regexp = "npsych_scores"
  )
})

# ---------------------------------------------------------------------------
# Input validation: version (via std_version S7 validator)
# ---------------------------------------------------------------------------

test_that("register_regression_version() rejects empty version", {
  testthat::local_reproducible_output()

  expect_error(
    register_regression_version(
      scores = MOCATOTS(),
      version = "",
      coefs = valid_coefs_vec,
      covar_fns = list(age = identity, educ = identity)
    )
  )
})

# ---------------------------------------------------------------------------
# Input validation: coefs (via regression_version S7 class)
# ---------------------------------------------------------------------------

test_that("register_regression_version() rejects coefs missing rmse", {
  no_rmse <- c(intercept = 20, age = -0.1, educ = 0.5)

  testthat::local_reproducible_output()

  expect_error(
    register_regression_version(
      scores = MOCATOTS(),
      version = "v1",
      coefs = no_rmse,
      covar_fns = list(age = identity, educ = identity)
    ),
    regexp = "rmse"
  )
})

test_that("register_regression_version() rejects coefs missing intercept", {
  no_intercept <- c(age = -0.1, educ = 0.5, rmse = 3.2)

  testthat::local_reproducible_output()

  expect_error(
    register_regression_version(
      scores = MOCATOTS(),
      version = "v1",
      coefs = no_intercept,
      covar_fns = list(age = identity, educ = identity)
    ),
    regexp = "intercept"
  )
})

# ---------------------------------------------------------------------------
# Input validation: covar_fns (via regression_version S7 class)
# ---------------------------------------------------------------------------

test_that("register_regression_version() rejects non-function entries in covar_fns", {
  testthat::local_reproducible_output()

  expect_error(
    register_regression_version(
      scores = MOCATOTS(),
      version = "v1",
      coefs = valid_coefs_vec,
      covar_fns = list(age = "not a function", educ = identity)
    ),
    regexp = "covar_fns"
  )
})

# ---------------------------------------------------------------------------
# Successful registration: numeric coefs
# ---------------------------------------------------------------------------

test_that("register_regression_version() registers successfully with numeric coefs", {
  expect_no_error(
    register_regression_version(
      scores = MOCATOTS(),
      version = "test_reg_vec_001",
      coefs = valid_coefs_vec,
      covar_fns = list(age = identity, educ = identity)
    )
  )

  versions <- list_method_versions(MOCATOTS(), "regression")
  expect_true("test_reg_vec_001" %in% versions)
})

# ---------------------------------------------------------------------------
# Successful registration: data.frame coefs
# ---------------------------------------------------------------------------

test_that("register_regression_version() registers successfully with data.frame coefs", {
  expect_no_error(
    register_regression_version(
      scores = MOCATOTS(),
      version = "test_reg_df_001",
      coefs = valid_coefs_df,
      covar_fns = list(age = identity, educ = identity)
    )
  )

  versions <- list_method_versions(MOCATOTS(), "regression")
  expect_true("test_reg_df_001" %in% versions)
})

# ---------------------------------------------------------------------------
# Stored data integrity
# ---------------------------------------------------------------------------

test_that("register_regression_version() stores a regression_version S7 object", {
  stored <- get_version_data(MOCATOTS(), "regression", "test_reg_vec_001")

  expect_true(S7::S7_inherits(stored, regression_version))
  expect_equal(stored@scores_class, "MOCATOTS")
  expect_equal(stored@method_name, "regression")
  expect_equal(stored@version_id, "test_reg_vec_001")
})

test_that("register_regression_version() stores coefs correctly", {
  stored <- get_version_data(MOCATOTS(), "regression", "test_reg_vec_001")
  expect_equal(stored@coefs, valid_coefs_vec)
})

test_that("register_regression_version() stores covar_fns correctly", {
  stored <- get_version_data(MOCATOTS(), "regression", "test_reg_vec_001")
  expect_true("age" %in% names(stored@covar_fns))
  expect_true("educ" %in% names(stored@covar_fns))
  expect_true(is.function(stored@covar_fns$age))
})

# ---------------------------------------------------------------------------
# Duplicate registration / overwrite
# ---------------------------------------------------------------------------

test_that("register_regression_version() errors when re-registering without overwrite", {
  register_regression_version(
    scores = MOCATOTS(),
    version = "test_reg_dup",
    coefs = valid_coefs_vec,
    covar_fns = list(age = identity, educ = identity)
  )

  testthat::local_reproducible_output()

  expect_error(
    register_regression_version(
      scores = MOCATOTS(),
      version = "test_reg_dup",
      coefs = valid_coefs_vec,
      covar_fns = list(age = identity, educ = identity)
    ),
    regexp = "already exists"
  )
})

test_that("register_regression_version() warns and succeeds with overwrite = TRUE", {
  register_regression_version(
    scores = MOCATOTS(),
    version = "test_reg_ow",
    coefs = valid_coefs_vec,
    covar_fns = list(age = identity, educ = identity)
  )

  new_coefs <- c(intercept = 30, age = -0.2, educ = 0.6, rmse = 4.0)

  expect_warning(
    register_regression_version(
      scores = MOCATOTS(),
      version = "test_reg_ow",
      coefs = new_coefs,
      covar_fns = list(age = identity, educ = identity),
      overwrite = TRUE
    ),
    regexp = "Overwriting"
  )

  stored <- get_version_data(MOCATOTS(), "regression", "test_reg_ow")
  expect_equal(stored@coefs[["intercept"]], 30)
})

# ---------------------------------------------------------------------------
# Returns invisibly
# ---------------------------------------------------------------------------

test_that("register_regression_version() returns invisibly", {
  result <- withVisible(
    register_regression_version(
      scores = MOCATOTS(),
      version = "test_reg_invisible_001",
      coefs = valid_coefs_vec,
      covar_fns = list(age = identity, educ = identity)
    )
  )

  expect_false(result$visible)
})

# ---------------------------------------------------------------------------
# Multi-row data.frame coefs (partial NAs)
# ---------------------------------------------------------------------------

test_that("register_regression_version() accepts data.frame coefs with partial NAs", {
  coefs_partial_na <- data.frame(
    intercept = c(20, 21),
    age = c(-0.1, NA),
    rmse = c(3.2, 3.5)
  )

  expect_no_error(
    register_regression_version(
      scores = MOCATOTS(),
      version = "test_partial_na_df_001",
      coefs = coefs_partial_na,
      covar_fns = list(age = identity)
    )
  )
})
