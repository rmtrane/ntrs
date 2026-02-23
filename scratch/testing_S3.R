## Main goals:
## - be able to "tag" columns as test scores
## - use data.table or tidyverse (via across) to std all npsych_scores columns easily.
## - get all methods available for a npsych_scores object
##     - should include S3 generics, both those defined internally in the package, in third party packages, and those defined by the user.
## - get all versions available for a npsych_scores object, for a given method

## Testing S3
devtools::load_all()

## Create MOCATOTS score vector
moca_scores <- MOCATOTS(c(15, 28, 19))

get_std_defaults(moca_scores)
get_std_defaults(MOCATOTS())

## Check std methods availabe
list_std_methods(MOCATOTS())

## Check versions available
list_method_versions(moca_scores, "regression")
list_method_versions(MOCATOTS(), "regression")

get_std_defaults("MOCATOTS")
get_std_defaults(moca_scores)

std_using_norms(
  x = moca_scores,
  age = 62,
  sex = "m",
  educ = 15,
  version = "nacc"
)

do.call(
  "std_using_regression",
  args = list(
    npsych_scores = moca_scores,
    age = 62,
    sex = "m",
    educ = 15,
    race = "Other",
    version = "nacc"
  )
)


std_using_regression(
  npsych_scores = moca_scores,
  age = 62,
  sex = 1,
  educ = 15,
  race = 1,
  version = "nacc"
)
