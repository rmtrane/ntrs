# Remove error codes

Removes error codes from the `npsych_scores` object using the `codes`
attribute.

## Usage

``` r
remove_error_codes(x)
```

## Arguments

- x:

  An object of class `npsych_scores`

## Value

An object with error codes removed.

## Examples

``` r
# 88 is an error code for MOCATOTS (outside range 0-30)
moca <- MOCATOTS(c(25, 88, 28))
remove_error_codes(moca) # 25 NA 28
#> [1] 25 NA 28
```
