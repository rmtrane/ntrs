# Neuropsychological Test Result Standardization (`ntrs`)


<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/rmtrane/ntrs/graph/badge.svg)](https://app.codecov.io/gh/rmtrane/ntrs)
[![R-CMD-check](https://github.com/rmtrane/ntrs/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/rmtrane/ntrs/actions/workflows/R-CMD-check.yaml)
[![pkgdown](https://github.com/rmtrane/ntrs/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/rmtrane/ntrs/actions/workflows/pkgdown.yaml)
<!-- badges: end -->

The goal of `ntrs` is to provide a streamlined framework for working
with neuropsychological test results and standardization of these. It is
build around the NACC UDS battery, but is designed to make extension
relatively painless for interested parties.

## Installation

You can install the development version of `ntrs` from
[GitHub](https://github.com/rmtrane/ntrs) with

``` r
# install.packages("pak")
pak::pak("rmtrane/ntrs")
```

or

``` r
# install.packages("remotes")
remotes::install_github("rmtrane/ntrs")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
suppressPackageStartupMessages(library(ntrs))

moca_scores <- MOCATOTS(c(25, 28, 18))

std(
  moca_scores,
  age = c(62, 59, 61),
  educ = c(15, 12, 13),
  sex = c(1, 2, 2),
  race = c(1, 4, 2)
)
```

    <ntrs::std_npsych_scores> num [1:3] -0.431 1.598 -2.689
     @ scores_subclass: chr "MOCATOTS"
     @ description    : chr "Standardized using regression, version updated_2025.06. Adjusted for covariates age, sex, educ, race."
     @ method         : chr "regression"
     @ version        : chr "updated_2025.06"
