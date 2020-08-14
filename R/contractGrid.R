#' Create a multi-dimensional grid of InsuranceContract objects, where the axes
#' ranges are given with the axes argument.
#' This function will return the full InsuranceContract objects, so apply can
#' later be used to extract premiums, reserves and other values to display in
#' a grid.
#'
#' @param axes List of paramters spanning the dimensions of the grid.
#' @param YOB optional year of bith. If missing, the \code{observationYear} and the contract's age
#' @param observationYear The observation year, for which the grid shall be calculated. If given, the YOB is calculated from it, otherwise the contract's YOB is used
#' @param ... Additional parameters to be passed to [InsuranceContract$new]
#' @export
contractGrid = function(axes = list(age = seq(20, 60, 10), policyPeriod = seq(5, 35, 5)), YOB = NULL, observationYear = NULL, ...) {

    obsYear = observationYear;
    # Create all combinations of the variables given for the axes:
    gridByRow = expand.grid(axes, KEEP.OUT.ATTRS = FALSE);
    # Apply InsuranceContract$new to each combination (and add the additional arguments)
    vals = apply(gridByRow, 1, function(axisVals) {
        args = c(as.list(axisVals), list(...));
        if (!is.null(observationYear)) {
            args$YOB = obsYear - args$age;
        }
        do.call(InsuranceContract$new, args)
    })
    dimnames = makeContractGridDimnames(axes)
    array(vals, dim = sapply(axes, length), dimnames = dimnames)
}

#' @export
makeContractGridDimname.InsuranceTarif = function(value) { value$name }
#' @export
makeContractGridDimname.R6 = function(value) { value$name }
#' @export
makeContractGridDimname.mortalityTable = function(value) { value@name }
#' @export
makeContractGridDimname.numeric = function(value) { value }
#' @export
makeContractGridDimname.double = function(value) { value }
#' @export
makeContractGridDimname.default = function(value) { value }
#' Generate a dimension label for the object passed as \code{value}, to be used in [contractGrid]
#' @param value the value along the axis, for which a name should be generated
#' @export
makeContractGridDimname = function(value) { UseMethod("makeContractGridDimname", value) }
#' Generate proper dimnames for all exntries of the axes of a [contractGrid]
#' @param axes the axes with all names, for which a name should be generated
#' @export
makeContractGridDimnames = function(axes) {
    lapply(axes, function(axis) { lapply(axis, makeContractGridDimname); } )
}

#' Create a multi-dimensional grid of premiums for insurance contracts, where the axes
#' ranges are given with the axes argument.
#' This function will return the full InsuranceContract objects, so apply can
#' later be used to extract premiums, reserves and other values to display in
#' a grid.
#'
#' @param contractGrid (optional) existing contract grid from which to derive
#' premiums. If not given, [contractGrid] is called with all parameters, so
#' \code{...} should contain an \code{axes} argument in that case.
#' @param premium The type of premium to derive (key of the \code{contract$Values$premiums} list.
#' @param ... Arguments pass on to [contractGrid] if no conract grid is given.
#' @export
contractGridPremium = function(contractGrid = NULL, premium="written", ...) {
    if (missing(contractGrid) || is.null(contractGrid)) {
        contractGrid = contractGrid(...)
    }
    apply(contractGrid, 1:length(dim(contractGrid)), function(c) { c[[1]]$Values$premiums[[premium]] })
}

if (FALSE) {
    gg = contractGrid(tarif = Generali.U188, axes = list(age = seq(20, 60, 10), policyPeriod = seq(5, 35, 5)), sumInsured = 100000)

    contractGridPremium(gg)
    apply(gg, 1:2, function(c) { c[[1]]$Parameters$ContractData$sumInsured })

    contractGridPremium(tarif = Generali.U188, axes = list(age = seq(20, 60, 10), policyPeriod = seq(5, 35, 5)), sumInsured = 100000)

    contractGridPremium(age = 30, axes = list(tarif = c(Generali.U188, Generali.U180, Generali.U3.1_2015), policyPeriod = seq(5, 35, 5)), sumInsured = 100000)
}


