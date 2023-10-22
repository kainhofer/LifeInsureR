#' @import LifeInsureR
#  Prevent spurious imports warnings in CRAN checks:
#' @importFrom rmarkdown render
NULL

.onAttach <- function(libname, pkgname) {
  warning("The LifeInsuranceContracts package has been renamed to LifeInsureR. \nPlease use library(LifeInsureR) instead!")
}
