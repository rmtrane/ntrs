# Test file for register_norms_version()

# ---------------------------------------------------------------------------
# Input validation: scores
# ---------------------------------------------------------------------------

test_that("register_norms_version() rejects non-npsych_scores object", {
  testthat::local_reproducible_output()

  expect_error(
    register_norms_version(
      scores = 42,
      version = "v1",
      lookup_table = data.frame(m = 50, sd = 10),
      covar_fns = list()
    ),
    regexp = "npsych_scores"
  )
})

# ---------------------------------------------------------------------------
# Input validation: version (via std_version S7 validator)
# ---------------------------------------------------------------------------

test_that("register_norms_version() rejects empty version", {
  testthat::local_reproducible_output()

  expect_error(
    register_norms_version(
      scores = MOCATOTS(),
      version = "",
      lookup_table = data.frame(m = 50, sd = 10),
      covar_fns = list()
    )
  )
})

# ---------------------------------------------------------------------------
# Input validation: lookup_table (via norms_version S7 class)
# ---------------------------------------------------------------------------

test_that("register_norms_version() rejects non-data.frame lookup_table", {
  testthat::local_reproducible_output()

  expect_error(
    register_norms_version(
      scores = MOCATOTS(),
      version = "test_v",
      lookup_table = list(m = 50, sd = 10),
      covar_fns = list()
    )
  )
})

test_that("register_norms_version() rejects lookup_table missing column m", {
  testthat::local_reproducible_output()

  expect_error(
    register_norms_version(
      scores = MOCATOTS(),
      version = "test_v",
      lookup_table = data.frame(sd = 10),
      covar_fns = list()
    ),
    regexp = "missing required columns"
  )
})

test_that("register_norms_version() rejects lookup_table missing column sd", {
  testthat::local_reproducible_output()

  expect_error(
    register_norms_version(
      scores = MOCATOTS(),
      version = "test_v",
      lookup_table = data.frame(m = 50),
      covar_fns = list()
    ),
    regexp = "missing required columns"
  )
})

# ---------------------------------------------------------------------------
# Input validation: covar_fns (via norms_version S7 class)
# ---------------------------------------------------------------------------

test_that("register_norms_version() rejects covar_fns with names not matching lookup_table", {
  testthat::local_reproducible_output()

  expect_error(
    register_norms_version(
      scores = MOCATOTS(),
      version = "test_v",
      lookup_table = data.frame(age = factor(60), m = 50, sd = 10),
      covar_fns = list(race = identity)
    ),
    regexp = "covar_fns"
  )
})

test_that("register_norms_version() rejects non-function entries in covar_fns", {
  testthat::local_reproducible_output()

  expect_error(
    register_norms_version(
      scores = MOCATOTS(),
      version = "test_v",
      lookup_table = data.frame(age = factor(60), m = 50, sd = 10),
      covar_fns = list(age = "not a function")
    ),
    regexp = "covar_fns"
  )
})

# ---------------------------------------------------------------------------
# Successful registration
# ---------------------------------------------------------------------------

test_that("register_norms_version() registers a version successfully", {
  valid_lt <- data.frame(
    age = factor(c("60-69", "70-79")),
    m = c(25, 26),
    sd = c(3, 4)
  )

  covar_fns <- list(
    age = \(x) {
      cut(x, breaks = c(60, 70, 80), right = FALSE,
          labels = c("60-69", "70-79"))
    }
  )

  expect_no_error(
    register_norms_version(
      scores = MOCATOTS(),
      version = "test_norms_reg_v1",
      lookup_table = valid_lt,
      covar_fns = covar_fns
    )
  )

  expect_true("test_norms_reg_v1" %in% list_method_versions(MOCATOTS(), "norms"))
})

test_that("register_norms_version() stores a norms_version S7 object", {
  stored <- get_version_data(MOCATOTS(), "norms", "test_norms_reg_v1")

  expect_true(S7::S7_inherits(stored, norms_version))
  expect_equal(stored@scores_class, "MOCATOTS")
  expect_equal(stored@method_name, "norms")
  expect_equal(stored@version_id, "test_norms_reg_v1")
})

test_that("register_norms_version() stores lookup_table and covar_fns in S7 object", {
  stored <- get_version_data(MOCATOTS(), "norms", "test_norms_reg_v1")

  expect_true("m" %in% names(stored@lookup_table))
  expect_true("sd" %in% names(stored@lookup_table))
  expect_true("age" %in% names(stored@covar_fns))
  expect_true(is.function(stored@covar_fns$age))
})

test_that("register_norms_version() accepts lookup_table with n column", {
  lt_with_n <- data.frame(
    age = factor(c("young", "old")),
    m = c(26, 24),
    sd = c(2, 3),
    n = c(100L, 150L)
  )

  register_norms_version(
    scores = MOCATOTS(),
    version = "test_norms_with_n",
    lookup_table = lt_with_n,
    covar_fns = list(
      age = \(x) factor(ifelse(x < 70, "young", "old"))
    )
  )

  stored <- get_version_data(MOCATOTS(), "norms", "test_norms_with_n")
  expect_true("n" %in% names(stored@lookup_table))
})

# ---------------------------------------------------------------------------
# Duplicate registration / overwrite
# ---------------------------------------------------------------------------

test_that("register_norms_version() errors when re-registering without overwrite", {
  valid_lt <- data.frame(m = 50, sd = 10)

  register_norms_version(
    scores = MOCATOTS(),
    version = "test_norms_dup",
    lookup_table = valid_lt,
    covar_fns = list()
  )

  testthat::local_reproducible_output()

  expect_error(
    register_norms_version(
      scores = MOCATOTS(),
      version = "test_norms_dup",
      lookup_table = data.frame(m = 55, sd = 12),
      covar_fns = list()
    ),
    regexp = "already exists"
  )
})

