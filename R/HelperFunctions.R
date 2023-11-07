#' @import LifeInsureR
NULL

.onAttach <- function(libname, pkgname) {
  warning("The LifeInsuranceContracts package has been renamed to LifeInsureR. \nPlease use library(LifeInsureR) instead!")
}
