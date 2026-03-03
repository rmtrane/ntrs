# Researchers Data Dictionary in List Form

Researchers Data Dictionary in List Form

## Usage

``` r
rdd
```

## Format

### `rdd`

- range:

  For numeric variables, the smallest and largest values allowable (that
  are not error codes). For non-numeric variables, NULL

- codes:

  Named vector where entries are allowed codes and names are labels. For
  example, for the total score for copy of Benson figure, the value of
  `95` is in the vector with the name "Physical problem".

- short_descriptor:

  The short descriptor as given in the PDF linked above.
