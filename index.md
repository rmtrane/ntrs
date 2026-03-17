# Neuropsychological Test Result Standardization (`ntrs`)

The goal of `ntrs` is to provide a streamlined framework for working
with neuropsychological test results and standardization of these. It is
build around the NACC UDS battery, but is designed to make extension
relatively painless for interested parties.

## Installation

You can install the development version of `ntrs` from
[GitHub](https://github.com/rmtrane/ntrs) with:

``` r
# install.packages("pak")
pak::pak("rmtrane/ntrs")
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

``` R
[1] -0.431067  1.597505 -2.689024
```
