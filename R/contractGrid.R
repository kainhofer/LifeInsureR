#' Create a multi-dimensional grid of InsuranceContract objects, where the axes
#' ranges are given with the axes argument.
#' This function will return the full InsuranceContract objects, so apply can
#' later be used to extract premiums, reserves and other values to display in
#' a grid.
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

makeContractGridDimname.InsuranceTarif = function(tarif) { tarif$name }
makeContractGridDimname.R6 = function(tarif) { tarif$name }
makeContractGridDimname.mortalityTable = function(table) { table@name }
makeContractGridDimname.numeric = function(value) { value }
makeContractGridDimname.double = function(value) { value }
makeContractGridDimname.default = function(value) { value }
makeContractGridDimname = function(value) { UseMethod("makeContractGridDimname", value) }
makeContractGridDimnames = function(axes) {
    lapply(axes, function(axis) { lapply(axis, makeContractGridDimname); } )
}

#' Create a multi-dimensional grid of premiums for insurance contracts, where the axes
#' ranges are given with the axes argument.
#' This function will return the full InsuranceContract objects, so apply can
#' later be used to extract premiums, reserves and other values to display in
#' a grid.
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


