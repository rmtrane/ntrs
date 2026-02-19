# # ---------------------------------------------------------------------------
# # OTRLARR()
# # ---------------------------------------------------------------------------

# # NOTE: .setup_OTRLARR_versions() has all version registrations commented
# # out. No version or default tests are included until the function is
# # implemented.

# test_that("OTRLARR returns an object inheriting both OTRLARR and test_scores", {
#   result <- OTRLARR()

#   expect_s3_class(result, "OTRLARR")
#   expect_s3_class(result, "test_scores")
# })

# test_that("OTRLARR sets class with OTRLARR first, test_scores second", {
#   result <- OTRLARR()

#   expect_equal(class(result), c("OTRLARR", "test_scores"))
# })

# test_that("OTRLARR returns an object with the correct values", {
#   result <- OTRLARR(c(0, 0, 77))

#   expect_equal(as.numeric(result), c(0, 0, 77))
# })

# test_that("OTRLARR sets label to 'Oral Trailmaking Part A - Errors'", {
#   result <- OTRLARR()

#   expect_equal(attr(result, "label"), "Oral Trailmaking Part A - Errors")
# })

# test_that("OTRLARR sets range to c(0, 77)", {
#   result <- OTRLARR()

#   expect_equal(attr(result, "range"), c(0, 77))
# })

# test_that("OTRLARR sets codes with the correct values", {
#   result <- OTRLARR()
#   codes  <- attr(result, "codes")

#   expect_true(88 %in% codes)
#   expect_true(95 %in% codes)
#   expect_true(96 %in% codes)
#   expect_true(97 %in% codes)
#   expect_true(98 %in% codes)
#   expect_true(-4 %in% codes)
# })

# test_that("OTRLARR codes are named", {
#   result <- OTRLARR()

#   expect_false(is.null(names(attr(result, "codes"))))
# })

# test_that("OTRLARR accepts scores at the range boundaries (0 and 77)", {
#   expect_no_error(OTRLARR(c(0, 77)))
# })

# test_that("OTRLARR accepts valid error code 88", {
#   expect_no_error(OTRLARR(c(0, 88)))
# })

# test_that("OTRLARR accepts valid error code 95", {
#   expect_no_error(OTRLARR(c(0, 95)))
# })

# test_that("OTRLARR accepts valid error code 96", {
#   expect_no_error(OTRLARR(c(0, 96)))
# })

# test_that("OTRLARR accepts valid error code 97", {
#   expect_no_error(OTRLARR(c(0, 97)))
# })

# test_that("OTRLARR accepts valid error code 98", {
#   expect_no_error(OTRLARR(c(0, 98)))
# })

# test_that("OTRLARR accepts valid error code -4", {
#   expect_no_error(OTRLARR(c(0, -4)))
# })

# test_that("OTRLARR errors when scores are out of range and not a code", {
#   testthat::local_reproducible_output()

# expect_error(OTRLARR(c(0, 78)), regexp = "scores")
# })

# test_that("OTRLARR errors when scores is not numeric", {
#   testthat::local_reproducible_output()

# expect_error(OTRLARR(c("a", "b")), regexp = "scores")
# })

# test_that("OTRLARR with no arguments returns an empty OTRLARR object", {
#   result <- OTRLARR()

#   expect_s3_class(result, "OTRLARR")
#   expect_equal(length(result), 0L)
# })

# # ---------------------------------------------------------------------------
# # .setup_OTRLARR_versions()
# #
# # .setup_*_versions() writes to the package-level .std_versions environment
# # and errors on duplicate registration. The function is called once here
# # outside any test_that() block; all tests below only read the registry.
# # ---------------------------------------------------------------------------

# test_that(".setup_OTRLARR_versions runs without error", {
#   expect_no_error(.setup_OTRLARR_versions())
# })

# test_that(".setup_OTRLARR_versions registers no methods", {
#   .setup_OTRLARR_versions()
#   expect_equal(length(get_std_methods(OTRLARR())), 0L)
# })
