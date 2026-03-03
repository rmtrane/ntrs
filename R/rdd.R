#' Researchers Data Dictionary in List Form
#'
#' @format ## `rdd`
#'
#' \describe{A list of entries for NACC variables with short descriptions,
#'   allowable values, and codes. This is created from
#'   from the .csv version of [this PDF](https://files.alz.washington.edu/documentation/uds3-rdd.pdf),
#'   which is made available to researcher upon acceptance of the data request,
#'   with the addition of some entries calculated based on the NACC variables.
#'   Each entry of the list has three entries:
#'   \item{range}{For numeric variables, the smallest and largest values allowable (that are not error codes). For non-numeric variables, NULL}
#'   \item{codes}{Named vector where entries are allowed codes and names are
#'     labels. For example, for the total score for copy of Benson figure, the
#'     value of `95` is in the vector with the name "Physical problem".}
#'   \item{short_descriptor}{The short descriptor as given in the PDF linked above.}
#' }
#'
"rdd"
