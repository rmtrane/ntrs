# Tests for npsych_scores S7 class and related methods
#
# Covers:
# 1. Class construction and validation
# 2. Subsetting with [ (preserves class)
# 3. Subset-assignment with [<-
# 4. Concatenation with c()
# 5. Helper functions: remove_error_codes, replace_codes, is_npsych_scores
# 6. data.frame / data.table column survival

# ---------------------------------------------------------------------------
# Construction and properties
# ---------------------------------------------------------------------------

test_that("npsych_scores subclass can be constructed via factory", {
  x <- MOCATOTS(c(25, 28, 30))
  expect_true(S7::S7_inherits(x, npsych_scores))
  expect_true(S7::S7_inherits(x, MOCATOTS))
})

test_that("npsych_scores properties are accessible via @", {
  x <- MOCATOTS(c(25, 28))
  expect_equal(x@label, "MoCA")
  expect_equal(x@domain, "General Cognition")
  expect_equal(x@range, c(0, 30))
  expect_true(length(x@codes) > 0)
})

test_that("npsych_scores stores numeric data correctly", {
  vals <- c(10, 20, 30)
  x <- MOCATOTS(vals)
  expect_equal(as.numeric(x), vals)
  expect_type(S7::S7_data(x), "double")
})

test_that("npsych_scores has correct length", {
  x <- MOCATOTS(c(25, 28, 30, NA))
  expect_length(x, 4)
})

test_that("npsych_scores accepts NA values", {
  x <- MOCATOTS(c(25, NA, 28))
  expect_length(x, 3)
  expect_true(is.na(x[2]))
})

test_that("npsych_scores accepts empty numeric vector", {
  x <- MOCATOTS()
  expect_length(x, 0)
  expect_true(S7::S7_inherits(x, npsych_scores))
})

test_that("npsych_scores accepts error codes as valid values", {
  # 88 and -4 are codes for MOCATOTS
  x <- MOCATOTS(c(25, 88, -4, 28))
  expect_length(x, 4)
  expect_equal(as.numeric(x), c(25, 88, -4, 28))
})

# ---------------------------------------------------------------------------
# Validation
# ---------------------------------------------------------------------------

test_that("npsych_scores rejects scores outside range and not in codes", {
  expect_error(MOCATOTS(c(25, 99)))
  expect_error(MOCATOTS(c(-10, 20)))
})

test_that("npsych_scores rejects non-numeric input", {
  expect_error(MOCATOTS(c("a", "b")))
})

# ---------------------------------------------------------------------------
# Subsetting with [
# ---------------------------------------------------------------------------

test_that("subsetting npsych_scores with [ preserves class and properties", {
  x <- MOCATOTS(c(10, 20, 30))
  sub <- x[1:2]

  expect_true(S7::S7_inherits(sub, MOCATOTS))
  expect_true(S7::S7_inherits(sub, npsych_scores))
  expect_equal(sub@label, "MoCA")
  expect_equal(sub@range, c(0, 30))
  expect_length(sub, 2)
  expect_equal(as.numeric(sub), c(10, 20))
})

test_that("single element subsetting preserves class", {
  x <- MOCATOTS(c(10, 20, 30))
  sub <- x[2]

  expect_true(S7::S7_inherits(sub, MOCATOTS))
  expect_equal(as.numeric(sub), 20)
})

test_that("logical subsetting preserves class", {
  x <- MOCATOTS(c(10, 20, 30))
  sub <- x[c(TRUE, FALSE, TRUE)]

  expect_true(S7::S7_inherits(sub, MOCATOTS))
  expect_equal(as.numeric(sub), c(10, 30))
})

# ---------------------------------------------------------------------------
# Subset-assignment with [<-
# ---------------------------------------------------------------------------

test_that("[<- modifies values in-place and preserves class", {
  x <- MOCATOTS(c(10, 20, 30))
  x[2] <- 25

  expect_true(S7::S7_inherits(x, MOCATOTS))
  expect_equal(as.numeric(x), c(10, 25, 30))
})

test_that("[<- can set multiple values", {
  x <- MOCATOTS(c(10, 20, 30))
  x[c(1, 3)] <- c(5, 15)

  expect_true(S7::S7_inherits(x, MOCATOTS))
  expect_equal(as.numeric(x), c(5, 20, 15))
})

test_that("[<- can assign NA", {
  x <- MOCATOTS(c(10, 20, 30))
  x[2] <- NA

  expect_true(S7::S7_inherits(x, MOCATOTS))
  expect_true(is.na(as.numeric(x)[2]))
})

# ---------------------------------------------------------------------------
# Concatenation with c()
# ---------------------------------------------------------------------------

test_that("c() combines npsych_scores of the same subclass", {
  x <- MOCATOTS(c(10, 20))
  y <- MOCATOTS(c(25, 30))

  combined <- c(x, y)

  expect_true(S7::S7_inherits(combined, MOCATOTS))
  expect_equal(as.numeric(combined), c(10, 20, 25, 30))
  expect_length(combined, 4)
})

test_that("c() preserves properties", {
  x <- MOCATOTS(c(10, 20))
  y <- MOCATOTS(c(25, 30))

  combined <- c(x, y)

  expect_equal(combined@label, "MoCA")
  expect_equal(combined@range, c(0, 30))
})

test_that("c() errors when combining different subclasses", {
  x <- MOCATOTS(c(10, 20))
  y <- TRAILA(c(30, 40))

  expect_error(c(x, y), "Cannot combine")
})

test_that("c() works with more than two objects", {
  a <- MOCATOTS(c(10))
  b <- MOCATOTS(c(20))
  d <- MOCATOTS(c(30))

  combined <- c(a, b, d)

  expect_true(S7::S7_inherits(combined, MOCATOTS))
  expect_equal(as.numeric(combined), c(10, 20, 30))
})

# ---------------------------------------------------------------------------
# Helper: remove_error_codes
# ---------------------------------------------------------------------------

test_that("remove_error_codes replaces error codes with NA", {
  x <- MOCATOTS(c(25, 88, 28, -4))
  result <- remove_error_codes(x)

  expect_equal(result, c(25, NA, 28, NA))
  expect_type(result, "double")
})

test_that("remove_error_codes returns plain numeric, not npsych_scores", {
  x <- MOCATOTS(c(25, 88))
  result <- remove_error_codes(x)

  expect_false(S7::S7_inherits(result, npsych_scores))
})

test_that("remove_error_codes leaves valid scores untouched", {
  x <- MOCATOTS(c(10, 20, 30))
  result <- remove_error_codes(x)

  expect_equal(result, c(10, 20, 30))
})

test_that("remove_error_codes errors on non-npsych_scores input", {
  expect_error(remove_error_codes(c(1, 2, 3)), "npsych_scores")
})

# ---------------------------------------------------------------------------
# Helper: replace_codes
# ---------------------------------------------------------------------------

test_that("replace_codes replaces codes with their labels", {
  x <- MOCATOTS(c(25, 88, 28))
  result <- replace_codes(x)

  # 88 should be replaced with the label string, coercing the vector to character
  expect_true(is.character(result))
  expect_equal(result[1], "25")
  expect_equal(result[3], "28")
  # The 88 position should contain the code label, not "88"
  expect_false(result[2] == "88")
})

test_that("replace_codes errors on non-npsych_scores input", {
  expect_error(replace_codes(c(1, 2, 3)), "npsych_scores")
})

# ---------------------------------------------------------------------------
# Helper: is_npsych_scores
# ---------------------------------------------------------------------------

test_that("is_npsych_scores returns TRUE for npsych_scores objects", {
  expect_true(is_npsych_scores(MOCATOTS(c(25, 28))))
})

test_that("is_npsych_scores returns FALSE for plain numeric", {
  expect_false(is_npsych_scores(c(25, 28)))
})

test_that("is_npsych_scores returns FALSE for character", {
  expect_false(is_npsych_scores("hello"))
})

# ---------------------------------------------------------------------------
# data.frame column survival
# ---------------------------------------------------------------------------

test_that("npsych_scores survives as a data.frame column", {
  x <- MOCATOTS(c(25, 28, 30))
  df <- data.frame(id = 1:3)
  df$moca <- x

  expect_true(S7::S7_inherits(df$moca, MOCATOTS))
  expect_equal(df$moca@label, "MoCA")
})

test_that("npsych_scores survives data.frame row subsetting", {
  df <- data.frame(id = 1:3)
  df$moca <- MOCATOTS(c(25, 28, 30))

  sub_df <- df[1:2, ]

  expect_true(S7::S7_inherits(sub_df$moca, MOCATOTS))
  expect_length(sub_df$moca, 2)
})

test_that("npsych_scores survives as a data.table column", {
  dt <- data.table::data.table(id = 1:3)
  dt[, moca := MOCATOTS(c(25, 28, 30))]

  expect_true(S7::S7_inherits(dt$moca, MOCATOTS))
  expect_equal(dt$moca@label, "MoCA")
})

test_that("npsych_scores survives rbind of data.frames", {
  make_df <- function(ids, vals) {
    df <- data.frame(id = ids)
    df$moca <- MOCATOTS(vals)
    df
  }

  df1 <- make_df(1:2, c(25, 28))
  df2 <- make_df(3:4, c(20, 30))

  combined <- rbind(df1, df2)

  expect_true(S7::S7_inherits(combined$moca, MOCATOTS))
  expect_length(combined$moca, 4)
})
