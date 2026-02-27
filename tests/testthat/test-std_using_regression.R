# Test file for std_using_regression() / std_using_regression.npsych_scores()

# Helpers ----

# Simple numeric coefs: intercept + age + educ + rmse
simple_coefs <- c(intercept = 30.0, age = -0.1, educ = 0.2, rmse = 2.5)

# covar_fns: clamp age and educ to sensible ranges
simple_covar_fns <- list(
  age = function(x) {
    x[x < 0] <- 0
    x[x > 110] <- 110
    x
  },
  educ = function(x) {
    x[x < 0] <- 0
    x[x > 31] <- 31
    x
  }
)

# Register a minimal regression version on MOCATOTS for use across tests
suppressMessages(
  register_regression_version(
    scores = MOCATOTS(),
    version = "test_reg_str_001",
    coefs = simple_coefs,
    covar_fns = simple_covar_fns
  )
)

# Test 1: S3 generic ----

test_that("std_using_regression() is an S3 generic", {
  expect_true(sloop::is_s3_generic("std_using_regression"))
})

# Test 2: unnamed dots are rejected ----

test_that("std_using_regression() errors when a ... argument is unnamed", {
  testthat::local_reproducible_output()
  expect_error(
    std_using_regression(
      MOCATOTS(25),
      60, # unnamed
      educ = 12,
      version = "test_reg_str_001"
    ),
    "named"
  )
})

# Test 3: non-numeric covariates are rejected ----

test_that("std_using_regression() errors when a covariate is not numeric", {
  testthat::local_reproducible_output()
  expect_error(
    std_using_regression(
      MOCATOTS(25),
      age = "old",
      educ = 12,
      version = "test_reg_str_001"
    ),
    class = "rlang_error"
  )
})

# Test 4: missing required covariates are caught ----

test_that("std_using_regression() errors when a required covariate is missing", {
  testthat::local_reproducible_output()
  expect_error(
    std_using_regression(
      MOCATOTS(25),
      age = 70, # educ is missing
      version = "test_reg_str_001"
    ),
    "missing"
  )
})

# Test 5: mismatched covariate length is caught ----

test_that("std_using_regression() errors when covariate length mismatches scores", {
  testthat::local_reproducible_output()
  expect_error(
    std_using_regression(
      MOCATOTS(c(25, 24)),
      age = c(60, 70, 80), # length 3, scores length 2
      educ = 12,
      version = "test_reg_str_001"
    ),
    "length"
  )
})

# Test 6: invalid version is caught ----

test_that("std_using_regression() errors on unregistered version", {
  testthat::local_reproducible_output()
  expect_error(
    std_using_regression(
      MOCATOTS(25),
      age = 70,
      educ = 12,
      version = "nonexistent_version_xyz"
    ),
    "nonexistent_version_xyz"
  )
})

# Test 7: returns numeric output ----

test_that("std_using_regression() returns a numeric vector", {
  result <- std_using_regression(
    MOCATOTS(25),
    age = 70,
    educ = 12,
    version = "test_reg_str_001"
  )

  expect_type(result, "double")
})

# Test 8: output length matches input length ----

test_that("std_using_regression() output length equals input length", {
  scores <- MOCATOTS(c(24, 25, 26))

  result <- std_using_regression(
    scores,
    age = c(60, 70, 80),
    educ = 12,
    version = "test_reg_str_001"
  )

  expect_length(result, length(scores))
})

# Test 9: scalar covariate is recycled across scores ----

test_that("std_using_regression() accepts scalar covariates recycled across scores", {
  scores <- MOCATOTS(c(24, 26))

  expect_no_error(
    std_using_regression(
      scores,
      age = 70, # scalar — recycled
      educ = 12, # scalar — recycled
      version = "test_reg_str_001"
    )
  )
})

# Test 10: correct z-score calculation ----

test_that("std_using_regression() computes the correct standardized score", {
  # intercept=30, age=-0.1, educ=0.2, rmse=2.5
  # predicted = 30 + (-0.1)*70 + 0.2*12 = 30 - 7 + 2.4 = 25.4
  # z = (25 - 25.4) / 2.5 = -0.4 / 2.5 = -0.16
  result <- std_using_regression(
    MOCATOTS(25),
    age = 70,
    educ = 12,
    version = "test_reg_str_001"
  )

  expect_equal(result, -0.16)
})

# Test 11: covar_fns are applied before computing the prediction ----

test_that("std_using_regression() applies covar_fns before computing prediction", {
  # age=200 is clamped to 110 by the registered age_fn
  # predicted = 30 + (-0.1)*110 + 0.2*12 = 30 - 11 + 2.4 = 21.4
  # z = (25 - 21.4) / 2.5 = 3.6 / 2.5 = 1.44
  result <- std_using_regression(
    MOCATOTS(25),
    age = 200,
    educ = 12,
    version = "test_reg_str_001"
  )

  expect_equal(result, 1.44)
})

# Test 12: vectorised scores and covariates all compute correctly ----

test_that("std_using_regression() correctly standardizes multiple scores", {
  # Score 1: age=60, educ=16 → predicted = 30 - 6 + 3.2 = 27.2 → z = (28 - 27.2) / 2.5 =  0.32
  # Score 2: age=80, educ=8  → predicted = 30 - 8 + 1.6 = 23.6 → z = (22 - 23.6) / 2.5 = -0.64
  result <- std_using_regression(
    MOCATOTS(c(28, 22)),
    age = c(60, 80),
    educ = c(16, 8),
    version = "test_reg_str_001"
  )

  expect_equal(result, c(0.32, -0.64))
})

# Test 13: multi-covariate (age + sex + educ) z-score calculation ----

# Register a three-covariate regression version
multi_coefs <- c(
  intercept = 30.0,
  age = -0.1,
  sex = -1.0,
  educ = 0.2,
  rmse = 2.5
)

multi_covar_fns <- list(
  age = function(x) {
    x[x < 0] <- 0
    x[x > 110] <- 110
    x
  },
  sex = function(x) x,
  educ = function(x) {
    x[x < 0] <- 0
    x[x > 31] <- 31
    x
  }
)

suppressMessages(
  register_regression_version(
    scores = MOCATOTS(),
    version = "test_reg_multi_001",
    coefs = multi_coefs,
    covar_fns = multi_covar_fns
  )
)

test_that("std_using_regression() computes correct z-score with three covariates", {
  # predicted = 30 + (-0.1)*70 + (-1.0)*1 + 0.2*16 = 30 - 7 - 1 + 3.2 = 25.2
  # z = (25 - 25.2) / 2.5 = -0.2 / 2.5 = -0.08
  result <- std_using_regression(
    MOCATOTS(25),
    age = 70,
    sex = 1,
    educ = 16,
    version = "test_reg_multi_001"
  )

  expect_equal(result, -0.08)
})

test_that("std_using_regression() vectorises correctly with three covariates", {
  # Person 1: predicted = 30 + (-0.1)*60 + (-1.0)*1 + 0.2*16 = 30 - 6 - 1 + 3.2 = 26.2
  #   z = (28 - 26.2) / 2.5 = 1.8 / 2.5 = 0.72
  # Person 2: predicted = 30 + (-0.1)*80 + (-1.0)*2 + 0.2*8 = 30 - 8 - 2 + 1.6 = 21.6
  #   z = (22 - 21.6) / 2.5 = 0.4 / 2.5 = 0.16
  result <- std_using_regression(
    MOCATOTS(c(28, 22)),
    age = c(60, 80),
    sex = c(1, 2),
    educ = c(16, 8),
    version = "test_reg_multi_001"
  )

  expect_equal(result, c(0.72, 0.16))
})

# Test 14: integration test with real registered regression version ----

test_that("std_using_regression() returns a finite numeric with real MOCATOTS version", {
  result <- std_using_regression(
    scores = MOCATOTS(25),
    age = 70,
    sex = 1,
    educ = 16,
    race = 2,
    version = "updated_2025.06"
  )

  expect_type(result, "double")
  expect_length(result, 1L)
  expect_true(is.finite(result))
})

test_that("std_using_regression() returns correct length with vectorised real version", {
  result <- std_using_regression(
    scores = MOCATOTS(c(20, 25, 30)),
    age = c(60, 70, 80),
    sex = c(1, 2, 1),
    educ = c(12, 16, 20),
    race = c(1, 2, 1),
    version = "updated_2025.06"
  )

  expect_length(result, 3L)
  expect_true(all(is.finite(result)))
})

# Test 15: error codes are converted to NA in output ----

test_that("std_using_regression() returns NA for a score that is an error code", {
  # -4 is a MOCATOTS error code; remove_error_codes() converts it to NA
  result <- std_using_regression(
    MOCATOTS(-4),
    age = 70,
    educ = 12,
    version = "test_reg_str_001"
  )

  expect_length(result, 1L)
  expect_true(is.na(result))
})

test_that("std_using_regression() returns NA only at error-code positions in vectorized input", {
  # Score 2 is error code -4; scores 1 and 3 are computed normally
  result <- std_using_regression(
    MOCATOTS(c(25, -4, 26)),
    age = c(70, 70, 70),
    educ = 12,
    version = "test_reg_str_001"
  )

  expect_length(result, 3L)
  expect_false(is.na(result[1]))
  expect_true(is.na(result[2]))
  expect_false(is.na(result[3]))
})

# Test 16: multi-submodel (data.frame coefs) ----

# Register a two-row coefs data.frame:
#   Row 1 (full model):    intercept=30, age=-0.10, educ=0.2,  rmse=2.5
#   Row 2 (educ submodel): intercept=28, age=-0.15, educ=NA,   rmse=3.0
submodel_coefs <- data.frame(
  intercept = c(30.0, 28.0),
  age = c(-0.1, -0.15),
  educ = c(0.2, NA),
  rmse = c(2.5, 3.0)
)

suppressMessages(
  register_regression_version(
    scores = MOCATOTS(),
    version = "test_reg_submodel_001",
    coefs = submodel_coefs,
    covar_fns = simple_covar_fns
  )
)

test_that("std_using_regression() uses full model row when all covariates are present", {
  # predicted = 30 + (-0.1)*70 + 0.2*12 = 25.4; z = (25 - 25.4) / 2.5 = -0.16
  result <- std_using_regression(
    MOCATOTS(25),
    age = 70,
    educ = 12,
    version = "test_reg_submodel_001"
  )

  expect_equal(result, -0.16)
})

test_that("std_using_regression() uses submodel row when a covariate is NA", {
  # educ=NA → submodel: predicted = 28 + (-0.15)*70 = 17.5; z = (25 - 17.5) / 3.0 = 2.5
  result <- std_using_regression(
    scores = MOCATOTS(25),
    age = 70,
    educ = NA_real_,
    version = "test_reg_submodel_001"
  )

  expect_equal(result, 2.5)
})

test_that("std_using_regression() handles mixed covariate missingness across observations", {
  # Person 1: age=70, educ=12  → full model: z = (25 - 25.4) / 2.5 = -0.16
  # Person 2: age=60, educ=NA  → submodel:   predicted = 28 + (-0.15)*60 = 19.0
  #                                           z = (22 - 19.0) / 3.0 = 1.0
  result <- std_using_regression(
    MOCATOTS(c(25, 22)),
    age = c(70, 60),
    educ = c(12, NA_real_),
    version = "test_reg_submodel_001"
  )

  expect_equal(result, c(-0.16, 1.0))
})

test_that("std_using_regression() returns NA when no submodel matches the missingness pattern", {
  # Both age and educ are NA → no model row has that pattern → NA
  result <- std_using_regression(
    MOCATOTS(25),
    age = NA_real_,
    educ = NA_real_,
    version = "test_reg_submodel_001"
  )

  expect_true(is.na(result))
})

# Test 17: all scores are error codes → all NA output ----

test_that("std_using_regression() returns all NA when all scores are error codes", {
  result <- std_using_regression(
    MOCATOTS(c(-4, 88, -4)),
    age = c(60, 70, 80),
    educ = 12,
    version = "test_reg_str_001"
  )

  expect_length(result, 3L)
  expect_true(all(is.na(result)))
})

# Test 18: version without covar_fns ----

# Register a version without covar_fns — covariates are used as-is
suppressMessages(
  register_regression_version(
    scores = MOCATOTS(),
    version = "test_reg_no_fns_001",
    coefs = simple_coefs
  )
)

test_that("std_using_regression() works when no covar_fns are registered", {
  # No covar_fns, so covariates pass through unchanged
  # intercept=30, age=-0.1, educ=0.2, rmse=2.5
  # predicted = 30 + (-0.1)*70 + 0.2*12 = 25.4; z = (25 - 25.4) / 2.5 = -0.16
  result <- std_using_regression(
    MOCATOTS(25),
    age = 70,
    educ = 12,
    version = "test_reg_no_fns_001"
  )

  expect_equal(result, -0.16)
})

test_that("std_using_regression() with no covar_fns uses raw covariate values", {
  # With covar_fns, age=200 is clamped to 110 → predicted=21.4
  # Without covar_fns, age=200 is used as-is → predicted = 30 + (-0.1)*200 + 0.2*12 = 12.4
  # z = (25 - 12.4) / 2.5 = 5.04
  result <- std_using_regression(
    MOCATOTS(25),
    age = 200,
    educ = 12,
    version = "test_reg_no_fns_001"
  )

  expect_equal(result, 5.04)
})

# Test 19: extra covariates not in the model are silently ignored ----

test_that("std_using_regression() ignores covariates not present in the model", {
  # 'delay' is not in simple_coefs; the result should match the call without it
  result_base <- std_using_regression(
    MOCATOTS(25),
    age = 70,
    educ = 12,
    version = "test_reg_str_001"
  )

  result_extra <- std_using_regression(
    MOCATOTS(25),
    age = 70,
    educ = 12,
    delay = 5,
    version = "test_reg_str_001"
  )

  expect_equal(result_extra, result_base)
})

# Test 20: missing version argument ----

test_that("std_using_regression() errors when version argument is not supplied", {
  expect_error(
    std_using_regression(
      MOCATOTS(25),
      age = 70,
      educ = 12
    )
  )
})

# Tests for lin_reg_resids() ----
#
# lin_reg_resids() is internal but accessible under devtools::load_all().
# Tests are organised by: input validation, single-model computation,
# NA handling, and multi-submodel matching.

# Shared fixtures
lr_coefs_vec <- c(intercept = 30.0, age = -0.1, educ = 0.2, rmse = 2.5)

lr_coefs_df <- data.frame(
  intercept = c(30.0, 28.0),
  age = c(-0.1, -0.15),
  educ = c(0.2, NA),
  rmse = c(2.5, 3.0)
)

# Input validation ----

test_that("lin_reg_resids() errors when coefs_to_use is unnamed", {
  X <- data.frame(intercept = 1, age = 70, educ = 12)
  coefs_unnamed <- c(30.0, -0.1, 0.2, 2.5) # no names

  expect_error(lin_reg_resids(25, X, coefs_unnamed), "named")
})

test_that("lin_reg_resids() errors when rmse is absent from coefs_to_use", {
  X <- data.frame(intercept = 1, age = 70, educ = 12)
  coefs_no_rmse <- c(intercept = 30.0, age = -0.1, educ = 0.2)

  expect_error(lin_reg_resids(25, X, coefs_no_rmse), "rmse")
})

test_that("lin_reg_resids() errors when cov_cols exceeds 53", {
  # intercept + 53 x-covariates = 54 cov_cols, which exceeds the limit
  extra_names <- paste0("x", seq_len(53L))
  coefs_big <- setNames(
    c(0, rep(1, 53L), 1.0),
    c("intercept", extra_names, "rmse")
  )
  X_big <- as.data.frame(
    matrix(
      1,
      nrow = 1L,
      ncol = 54L,
      dimnames = list(NULL, c("intercept", extra_names))
    )
  )

  expect_error(lin_reg_resids(0, X_big, coefs_big), "53")
})

# Single-model computation ----

test_that("lin_reg_resids() returns correct z-score from a named-vector model", {
  # predicted = 30 + (-0.1)*70 + 0.2*12 = 25.4; z = (25 - 25.4) / 2.5 = -0.16
  X <- data.frame(intercept = 1, age = 70, educ = 12)

  expect_equal(lin_reg_resids(25, X, lr_coefs_vec), -0.16)
})

test_that("lin_reg_resids() auto-adds an intercept column when absent from X", {
  X_no_int <- data.frame(age = 70, educ = 12)

  expect_equal(lin_reg_resids(25, X_no_int, lr_coefs_vec), -0.16)
})

test_that("lin_reg_resids() returns correct z-scores for vectorised input", {
  # Person 1: age=60, educ=16 → pred = 30 - 6 + 3.2 = 27.2 → z = (28 - 27.2) / 2.5 =  0.32
  # Person 2: age=80, educ=8  → pred = 30 - 8 + 1.6 = 23.6 → z = (22 - 23.6) / 2.5 = -0.64
  X <- data.frame(intercept = 1, age = c(60, 80), educ = c(16, 8))

  expect_equal(lin_reg_resids(c(28, 22), X, lr_coefs_vec), c(0.32, -0.64))
})

# NA handling ----

test_that("lin_reg_resids() returns all NA when all raw_scores are NA", {
  X <- data.frame(intercept = 1, age = c(60, 70, 80), educ = 12)

  result <- lin_reg_resids(c(NA_real_, NA_real_, NA_real_), X, lr_coefs_vec)

  expect_length(result, 3L)
  expect_true(all(is.na(result)))
})

test_that("lin_reg_resids() returns NA only at NA score positions", {
  # Person 1: z = (25 - 25.4) / 2.5 = -0.16
  # Person 2: NA (raw score is NA)
  # Person 3: pred = 30 - 7 + 2.4 = 25.4; z = (26 - 25.4) / 2.5 = 0.24
  X <- data.frame(intercept = 1, age = c(70, 70, 70), educ = c(12, 12, 12))

  result <- lin_reg_resids(c(25, NA_real_, 26), X, lr_coefs_vec)

  expect_equal(result[1], -0.16)
  expect_true(is.na(result[2]))
  expect_equal(result[3], 0.24)
})

# Multi-submodel matching ----

test_that("lin_reg_resids() uses the full model row when all covariates are present", {
  # Full model (row 1): pred = 30 - 7 + 2.4 = 25.4; z = (25 - 25.4) / 2.5 = -0.16
  X <- data.frame(intercept = 1, age = 70, educ = 12)

  expect_equal(lin_reg_resids(25, X, lr_coefs_df), -0.16)
})

test_that("lin_reg_resids() uses a submodel when a covariate column is entirely NA", {
  # educ entirely NA → column dropped; row 1 (educ=0.2) pruned; row 2 used
  # pred = 28 + (-0.15)*70 = 17.5; z = (25 - 17.5) / 3.0 = 2.5
  X <- data.frame(intercept = 1, age = 70, educ = NA_real_)

  expect_equal(lin_reg_resids(25, X, lr_coefs_df), 2.5)
})

test_that("lin_reg_resids() routes each observation to the correct submodel", {
  # Person 1: educ=12 → full model: z = (25 - 25.4) / 2.5 = -0.16
  # Person 2: educ=NA → submodel: pred = 28 + (-0.15)*60 = 19.0; z = (22 - 19.0) / 3.0 = 1.0
  X <- data.frame(intercept = 1, age = c(70, 60), educ = c(12, NA_real_))

  expect_equal(lin_reg_resids(c(25, 22), X, lr_coefs_df), c(-0.16, 1.0))
})

test_that("lin_reg_resids() returns NA when no submodel matches the missingness pattern", {
  # Both age and educ entirely NA → both columns dropped; both model rows pruned
  # (row 1 requires age, row 2 also requires age) → no model survives → NA
  X <- data.frame(intercept = 1, age = NA_real_, educ = NA_real_)

  expect_true(is.na(lin_reg_resids(25, X, lr_coefs_df)))
})
