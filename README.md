# Neuropsychological Test Result Standardization (`ntrs`)


<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/rmtrane/ntrs/graph/badge.svg)](https://app.codecov.io/gh/rmtrane/ntrs)
[![R-CMD-check](https://github.com/rmtrane/ntrs/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/rmtrane/ntrs/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of `ntrs` is to provide a streamlined framework for working
with neuropsychological test results and standardization of these. It is
build around the NACC UDS battery, but is designed to make extension
relatively painless for interested parties.

## Installation

You can install the development version of ntrs from
[GitHub](https://github.com/rmtrane/ntrs) with:

``` r
# install.packages("pak")
pak::pak("rmtrane/ntrs")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(ntrs)
```

    ✔ Set "updated_2025.06" as default for regression method on <UDSVERLC>
    ✔ Set "nacc_legacy" as default for regression method on <NACCMMSE>
    ✔ Set "nacc_legacy" as default for regression method on <MEMUNITS>
    ✔ Set "updated_2025.06" as default for regression method on <MINTTOTS>
    ✔ Set "updated_2025.06" as default for regression method on <UDSVERTN>
    ✔ Set "updated_2025.06" as default for regression method on <VEG>
    ✔ Set "nacc_legacy" as default for regression method on <DIGIFLEN>
    ✔ Set "nacc_legacy" as default for regression method on <DIGIB>
    ✔ Set "updated_2025.06" as default for regression method on <DIGFORSL>
    ✔ Set "updated_2025.06" as default for regression method on <CRAFTVRS>
    ✔ Set "nacc_legacy" as default for regression method on <DIGIBLEN>
    ✔ Set "nacc_legacy" as default for regression method on <LOGIMEM>
    ✔ Set "updated_2025.06" as default for regression method on <CRAFTURS>
    ✔ Set "nacc_legacy" as default for regression method on <DIGIF>
    ✔ Set "updated_2025.06" as default for regression method on <DIGBACCT>
    ✔ Set "nacc_legacy" as default for regression method on <BOSTON>
    ✔ Set "updated_2025.06" as default for regression method on <UDSBENTD>
    ✔ Set "updated_2025.06" as default for regression method on <OTRAILB>
    ✔ Set "updated_2025.06" as default for regression method on <CRAFTDVR>
    ✔ Set "updated_2025.06" as default for regression method on <DIGBACLS>
    ✔ Set "updated_2025.06" as default for regression method on <UDSBENTC>
    ✔ Set "updated_2025.06" as default for regression method on <OTRAILA>
    ✔ Set "updated_2025.06" as default for regression method on <DIGFORCT>
    ✔ Set "updated_2025.06" as default for regression method on <CRAFTDRE>
    ✔ Set "updated_2025.06" as default for regression method on <ANIMALS>
    ✔ Set "updated_2025.06" as default for regression method on <UDSVERFC>
    ✔ Set "updated_2025.06" as default for regression method on <TRAILB>
    ✔ Set "updated_2025.06" as default for regression method on <OTRLBRR>
    ✔ Set "updated_2025.06" as default for regression method on <MOCATOTS>
    ✔ Set "updated_2025.06" as default for regression method on <TRAILA>

``` r
moca_scores <- MOCATOTS(c(25, 28, 18))

std(
  moca_scores,
  age = c(62, 59, 61),
  educ = c(15, 12, 13),
  sex = c(1, 2, 2),
  race = c(1, 4, 2)
)
```

    [1]  0.2024372  1.0233537 -3.2244596
