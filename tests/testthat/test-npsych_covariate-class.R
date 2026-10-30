# Tests for npsych_covariate S7 class, new_npsych_covariate(), and the
# covariate subclasses NACCAGE, SEX, EDUC, RACE, MEMTIME
#
# Covers:
# 1. Class construction and validation (valid values, codes, NA, errors)
# 2. valid_values() for every code of every subclass
# 3. Subsetting with [ and subset-assignment with [<-
# 4. Concatenation with c()
# 5. data.table column survival
# 6. new_npsych_covariate() definition-time checks

# ---------------------------------------------------------------------------
# Construction and properties
# ---------------------------------------------------------------------------

test_that("covariate subclasses inherit from npsych_covariate, not npsych_scores", {
  for (cls in list(NACCAGE, SEX, EDUC, RACE, MEMTIME)) {
    x <- cls()
    expect_true(S7::S7_inherits(x, npsych_covariate))
    expect_true(is_npsych_covariate(x))
    expect_false(is_npsych_scores(x))
    expect_length(x, 0)
  }
})

test_that("is_npsych_covariate() is FALSE for other objects", {
  expect_false(is_npsych_covariate(c(1, 2)))
  expect_false(is_npsych_covariate(MOCATOTS(25)))
})

test_that("covariate properties are accessible via @", {
  x <- EDUC(c(12, 16))
  expect_equal(x@label, "Education")
  expect_equal(x@range, c(0, 36))
  expect_length(x@values, 0)
  expect_equal(x@codes, c("Unknown" = 99))

  r <- RACE(1)
  expect_length(r@range, 0)
  expect_equal(unname(r@values), c(1, 2, 3, 4, 5, 50))
  expect_equal(names(r@values)[1], "White")
})

test_that("covariates accept valid values, codes, and NA", {
  expect_equal(as.numeric(NACCAGE(c(18, 72.5, 120, NA))), c(18, 72.5, 120, NA))
  expect_equal(as.numeric(SEX(c(1, 2, 8, 9, NA))), c(1, 2, 8, 9, NA))
  expect_equal(as.numeric(EDUC(c(0, 16, 36, 99, NA))), c(0, 16, 36, 99, NA))
  expect_equal(
    as.numeric(RACE(c(1, 2, 3, 4, 5, 50, 99, NA))),
    c(1, 2, 3, 4, 5, 50, 99, NA)
  )
  expect_equal(as.numeric(MEMTIME(c(0, 30, 85, 99, -4, NA))), c(0, 30, 85, 99, -4, NA))
})

test_that("covariate constructors accept integers and all-NA logicals", {
  expect_type(S7::S7_data(SEX(c(1L, 2L))), "double")
  expect_equal(as.numeric(RACE(NA)), NA_real_)
  expect_equal(as.numeric(EDUC(c(NA, NA))), c(NA_real_, NA_real_))
})

test_that("covariate constructors accept an object of the same class", {
  x <- EDUC(c(12, 99))
  expect_identical(EDUC(x), x)
})

test_that("covariate constructors error on invalid values", {
  expect_error(NACCAGE(17), "invalid value 17")
  expect_error(NACCAGE(121), "invalid value 121")
  expect_error(SEX(3), "invalid value 3")
  expect_error(EDUC(40), "invalid value 40")
  expect_error(EDUC(-1), "invalid value -1")
  expect_error(RACE(c(6, 1, 7)), "invalid values 6 and 7")
  expect_error(RACE(1.5), "invalid value 1.5")
  expect_error(MEMTIME(86), "invalid value 86")
})

test_that("invalid-value errors list what is allowed", {
  expect_error(EDUC(40), "Allowed: 0.36 and 99 \\(Unknown\\)")
  expect_error(NACCAGE(10), "Allowed: 18.120\\.")
})

test_that("covariate constructors error on non-numeric input", {
  expect_error(EDUC("12"))
})

# ---------------------------------------------------------------------------
# valid_values()
# ---------------------------------------------------------------------------

test_that("valid_values() replaces codes with NA and keeps valid values", {
  expect_equal(valid_values(NACCAGE(c(18, 70, NA))), c(18, 70, NA))
  expect_equal(valid_values(SEX(c(1, 2, 8, 9, NA))), c(1, 2, NA, NA, NA))
  expect_equal(valid_values(EDUC(c(0, 16, 36, 99, NA))), c(0, 16, 36, NA, NA))
  expect_equal(
    valid_values(RACE(c(1, 2, 3, 4, 5, 50, 99, NA))),
    c(1, 2, 3, 4, 5, 50, NA, NA)
  )
  expect_equal(
    valid_values(MEMTIME(c(0, 30, 85, 99, -4, NA))),
    c(0, 30, 85, NA, NA, NA)
  )
})

test_that("valid_values() returns a plain numeric vector", {
  out <- valid_values(EDUC(c(12, 99)))
  expect_type(out, "double")
  expect_false(is_npsych_covariate(out))
})

test_that("valid_values() errors for non-covariates", {
  expect_error(valid_values(c(1, 2)), "npsych_covariate")
  expect_error(valid_values(MOCATOTS(25)), "npsych_covariate")
})

# ---------------------------------------------------------------------------
# Subsetting and subset-assignment
# ---------------------------------------------------------------------------

test_that("[ preserves the covariate class", {
  x <- EDUC(c(12, 16, 99))[2:3]
  expect_true(S7::S7_inherits(x, EDUC))
  expect_equal(as.numeric(x), c(16, 99))
})

test_that("[<- accepts valid values and codes", {
  x <- EDUC(c(12, 16))
  x[2] <- 99
  expect_true(S7::S7_inherits(x, EDUC))
  expect_equal(valid_values(x), c(12, NA))
})

test_that("[<- errors on invalid values", {
  x <- EDUC(c(12, 16))
  expect_error(x[1] <- 50, "invalid value 50")
})

# ---------------------------------------------------------------------------
# Concatenation
# ---------------------------------------------------------------------------

test_that("c() combines covariates of the same class", {
  x <- c(RACE(1), RACE(c(2, 99)))
  expect_true(S7::S7_inherits(x, RACE))
  expect_equal(as.numeric(x), c(1, 2, 99))
})

test_that("c() errors when combining different covariate classes", {
  expect_error(c(EDUC(12), SEX(1)), "Cannot combine")
})

# ---------------------------------------------------------------------------
# data.table columns
# ---------------------------------------------------------------------------

test_that("covariate columns survive data.table row subsetting and rbindlist", {
  dt <- data.table::data.table(id = 1:3, EDUC = EDUC(c(12, 16, 99)))

  expect_true(S7::S7_inherits(dt[2:3]$EDUC, EDUC))
  expect_true(S7::S7_inherits(data.table::rbindlist(list(dt, dt))$EDUC, EDUC))
})

# ---------------------------------------------------------------------------
# new_npsych_covariate()
# ---------------------------------------------------------------------------

test_that("new_npsych_covariate() creates a working subclass", {
  HANDED <- new_npsych_covariate(
    "HANDED",
    label = "Handedness",
    values = c("Left" = 1, "Right" = 2, "Ambidextrous" = 3),
    codes = c("Unknown" = 9)
  )

  x <- HANDED(c(1, 2, 9))
  expect_true(S7::S7_inherits(x, npsych_covariate))
  expect_equal(valid_values(x), c(1, 2, NA))
  expect_error(HANDED(4), "invalid value 4")
})

test_that("new_npsych_covariate() validates its arguments", {
  expect_error(new_npsych_covariate("", label = "x", range = c(0, 1)), "name")
  expect_error(new_npsych_covariate("X", label = 1, range = c(0, 1)), "label")
  expect_error(new_npsych_covariate("X", label = "x", range = 1), "range")
  expect_error(new_npsych_covariate("X", label = "x"), "range.*values")
  expect_error(
    new_npsych_covariate("X", label = "x", range = c(0, 1), codes = 9),
    "named"
  )
  expect_error(
    new_npsych_covariate(
      "X",
      label = "x",
      range = c(0, 100),
      codes = c("Unknown" = 99)
    ),
    "must not overlap"
  )
  expect_error(
    new_npsych_covariate(
      "X",
      label = "x",
      values = c(1, 2),
      codes = c("Unknown" = 2)
    ),
    "must not overlap"
  )
})
