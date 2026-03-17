# Replace codes with their labels

Replaces numeric codes in a `npsych_scores` object with their
corresponding named labels from the `codes` attribute.

## Usage

``` r
replace_codes(x)
```

## Arguments

- x:

  An object of class `npsych_scores`.

## Value

A numeric vector with code values replaced by their named labels.

## Examples

``` r
moca <- MOCATOTS(c(25, 88, -4, 28))
replace_codes(moca)
#> [1] "25"                                                                                                                       
#> [2] "Item(s) or whole test not administered"                                                                                   
#> [3] "Not available: UDS form submitted did not collect data in this way, or a skip pattern precludes response to this question"
#> [4] "28"                                                                                                                       
```
