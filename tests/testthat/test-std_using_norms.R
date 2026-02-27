# Test file for std_using_norms() / std_using_norms.npsych_scores()

# Helpers ----

# Age groups as a factor — levels must exactly match sort(unique(covar_fn(0:110)))
age_levels <- c("60", "70", "80")

# A minimal lookup table: one covariate (age), with mean and sd
simple_lookup <- data.frame(
  age = factor(age_levels, levels = age_levels),
  m = c(26.0, 25.0, 24.0),
  sd = c(2.0, 2.5, 3.0)
)

# covar_fns$age must return a factor with the same levels as simple_lookup$age
# sort(unique(age_fn(0:110))) must be identical() to sort(unique(simple_lookup$age))
age_fn <- function(x) {
  bucketed <- ifelse(x < 65, "60", ifelse(x < 75, "70", "80"))
  factor(bucketed, levels = age_levels)
}

# Register a minimal norms version on MOCATOTS for use across tests
suppressMessages(
  register_norms_version(
    scores = MOCATOTS(),
    version = "test_norms_stn_001",
    lookup_table = simple_lookup,
    covar_fns = list(age = age_fn)
  )
)

# Test 1: S3 generic ----

test_that("std_using_norms() is an S3 generic", {
  expect_true(sloop::is_s3_generic("std_using_norms"))
})

# Test 2: non-numeric covariates are rejected ----

test_that("std_using_norms() errors when a covariate is not numeric", {
  testthat::local_reproducible_output()
  expect_error(
    std_using_norms(
      MOCATOTS(25),
      age = "old",
      version = "test_norms_stn_001"
    ),
    class = "rlang_error"
  )
})

# Test 3: missing required covariates are caught ----
test_that("std_using_norms() errors when a required covariate is missing", {
  testthat::local_reproducible_output()
  expect_error(
    std_using_norms(
      MOCATOTS(25),
      version = "test_norms_stn_001"
    ),
    "missing"
  )
})

# Test 4: mismatched covariate length is caught ----
test_that("std_using_norms() errors when covariate length mismatches scores", {
  testthat::local_reproducible_output()
  expect_error(
    std_using_norms(
      MOCATOTS(c(25, 24)),
      age = c(60, 70, 80), # length 3, scores length 2
      version = "test_norms_stn_001"
    ),
    "length"
  )
})

# Test 5: invalid version is caught ----

test_that("std_using_norms() errors on unregistered version", {
  testthat::local_reproducible_output()
  expect_error(
    std_using_norms(
      MOCATOTS(25),
      age = 60,
      version = "nonexistent_version_xyz"
    ),
    "nonexistent_version_xyz"
  )
})

# Test 6: returns numeric output ----

test_that("std_using_norms() returns a numeric vector", {
  result <- std_using_norms(
    scores = MOCATOTS(25),
    age = 60,
    version = "test_norms_stn_001"
  )

  expect_type(result, "double")
})

# Test 7: output length matches input length ----

test_that("std_using_norms() output length equals input length", {
  scores <- MOCATOTS(c(24, 25, 26))

  result <- std_using_norms(
    scores,
    age = c(60, 70, 80),
    version = "test_norms_stn_001"
  )

  expect_length(result, length(scores))
})

# Test 8: scalar covariate is recycled across scores ----

test_that("std_using_norms() accepts scalar covariate recycled across scores", {
  scores <- MOCATOTS(c(24, 26))

  expect_no_error(
    std_using_norms(
      scores,
      age = 60, # scalar — should be recycled
      version = "test_norms_stn_001"
    )
  )
})

# Test 9: correct z-score calculation ----

test_that("std_using_norms() computes the correct z-score", {
  # For age=60: m=26, sd=2 → z = (25 - 26) / 2 = -0.5
  result <- std_using_norms(
    MOCATOTS(25),
    age = 60,
    version = "test_norms_stn_001"
  )

  expect_equal(result, -0.5)
})

# Test 10: covar_fns are applied before lookup ----

test_that("std_using_norms() applies covar_fns before matching to lookup table", {
  # age=63 is bucketed to "60" by the registered age_fn (x < 65 → "60")
  # For age group "60": m=26, sd=2 → z = (25 - 26) / 2 = -0.5
  result <- std_using_norms(
    MOCATOTS(25),
    age = 63,
    version = "test_norms_stn_001"
  )

  expect_equal(result, -0.5)
})

# Test 11: vectorised scores and covariates ----

test_that("std_using_norms() correctly standardizes multiple scores", {
  # age=60: m=26, sd=2  → z = (24 - 26) / 2  = -1.0
  # age=70: m=25, sd=2.5 → z = (27 - 25) / 2.5 =  0.8
  result <- std_using_norms(
    MOCATOTS(c(24, 27)),
    age = c(60, 70),
    version = "test_norms_stn_001"
  )

  expect_equal(result, c(-1.0, 0.8))
})

# Test 12: multi-covariate (age + sex + educ) z-score calculation ----

# Register a three-covariate norms version
multi_age_levels <- c("young", "old")
multi_sex_levels <- c("m", "f")
multi_educ_levels <- c("low", "high")

multi_lookup <- expand.grid(
  age = factor(multi_age_levels, levels = multi_age_levels),
  sex = factor(multi_sex_levels, levels = multi_sex_levels),
  educ = factor(multi_educ_levels, levels = multi_educ_levels),
  stringsAsFactors = FALSE
)

multi_lookup$m <- c(24, 22, 25, 23, 26, 24, 27, 25)
multi_lookup$sd <- rep(2.0, 8)

multi_age_fn <- function(x) {
  factor(ifelse(x < 70, "young", "old"), levels = multi_age_levels)
}
multi_sex_fn <- function(x) {
  factor(x, levels = c(1, 2), labels = c("m", "f"))
}
multi_educ_fn <- function(x) {
  factor(ifelse(x < 12, "low", "high"), levels = multi_educ_levels)
}

suppressMessages(
  register_norms_version(
    scores = MOCATOTS(),
    version = "test_norms_multi_001",
    lookup_table = multi_lookup,
    covar_fns = list(
      age = multi_age_fn,
      sex = multi_sex_fn,
      educ = multi_educ_fn
    )
  )
)

test_that("std_using_norms() computes correct z-score with three covariates", {
  # age=60 → "young", sex=1 → "m", educ=16 → "high"
  # Row: young, m, high → m=26, sd=2
  # z = (25 - 26) / 2 = -0.5
  result <- std_using_norms(
    MOCATOTS(25),
    age = 60,
    sex = 1,
    educ = 16,
    version = "test_norms_multi_001"
  )

  expect_equal(result, -0.5)
})

test_that("std_using_norms() vectorises correctly with three covariates", {
  # Person 1: age=60→young, sex=1→m, educ=16→high → m=26, sd=2
  #   z = (28 - 26) / 2 = 1.0
  # Person 2: age=75→old, sex=2→f, educ=8→low → m=23, sd=2
  #   z = (21 - 23) / 2 = -1.0
  result <- std_using_norms(
    MOCATOTS(c(28, 21)),
    age = c(60, 75),
    sex = c(1, 2),
    educ = c(16, 8),
    version = "test_norms_multi_001"
  )

  expect_equal(result, c(1.0, -1.0))
})

# Test 13: integration test with real registered norms version ----

test_that("std_using_norms() returns a finite numeric with real 'nacc' MOCATOTS version", {
  result <- std_using_norms(
    MOCATOTS(25),
    age = 70,
    sex = 1,
    educ = 16,
    version = "nacc"
  )

  expect_type(result, "double")
  expect_length(result, 1L)
  expect_true(is.finite(result))
})

test_that("std_using_norms() returns correct length with vectorised real norms", {
  result <- std_using_norms(
    MOCATOTS(c(20, 25, 30)),
    age = c(60, 70, 80),
    sex = c(1, 2, 1),
    educ = c(12, 16, 20),
    version = "nacc"
  )

  expect_length(result, 3L)
  expect_true(all(is.finite(result)))
})

# Test 14: unneccessary covariates are ignored with messages ----

test_that("std_using_norms() ignores unneccessary covariates with a message", {
  testthat::local_reproducible_output()

  expect_warning(
    res <- std_using_norms(
      MOCATOTS(25),
      age = 70,
      sex = 1,
      educ = 16,
      race = 1, # not needed for 'nacc' version
      version = "nacc"
    ),
    regexp = "is not needed to standardize using"
  )

  expect_equal(
    res,
    std_using_norms(
      MOCATOTS(25),
      age = 70,
      sex = 1,
      educ = 16,
      version = "nacc"
    )
  )
})

# Test 15: make sure covariates are simply passed through for version without covar_fns ----

test_that("std_using_norms() passes covariates through when version has no covar_fns", {
  # Register a norms version without covar_fns
  suppressMessages(
    register_norms_version(
      scores = MOCATOTS(),
      version = "test_no_covar_fns_001",
      lookup_table = data.frame(age = factor(60), m = 25, sd = 2),
      covar_fns = list(),
      overwrite = TRUE
    )
  )

  withr::defer(
    rm("test_no_covar_fns_001", envir = .std_versions[["norms"]]),
    envir = .GlobalEnv
  )

  expect_equal(
    std_using_norms(
      MOCATOTS(c(25, 27)),
      age = 60,
      version = "test_no_covar_fns_001"
    ),
    c(0, 1)
  )
})
