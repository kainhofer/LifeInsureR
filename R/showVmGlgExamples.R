#' @include InsuranceContract.R
#'
#' @import MortalityTables
#' @import scales
NULL

#' Display insurance contract calculation example
#'
#' Display the values of the example calculation of the given insurance contract
#' as required by the Austrian regulation (LV-VMGLV, "LV
#' Versicherungsmathematische Grundlagen Verordnung").
#'
#' @param contract The insurance contract to calculate and show
#' @param t Time for which to show all values (except premium-free values)
#' @param prf Time of premium waiver (premium-free)
#' @param t_prf Time for which to show all values after the premium waiver
#' @param file If given, outputs all information to the file rather than the console
#' @param ... Further parameters (currently unused)
#'
#' @examples
#' library(MortalityTables)
#' mortalityTables.load("Austria_Annuities_AVOe2005R")
#' # A trivial deferred annuity tariff with no costs:
#' tariff = InsuranceTarif$new(name="Test Annuity", type="annuity",
#'     mortalityTable = AVOe2005R.unisex, i=0.01)
#' contract = InsuranceContract$new(
#'     tariff,
#'     age = 35, YOB = 1981,
#'     policyPeriod = 30, premiumPeriod = 15, deferralPeriod = 15,
#'     sumInsured = 1000,
#'     contractClosing = as.Date("2016-10-01")
#' );
#' showVmGlgExamples(contract)
#'
#' # Optionally output to a file rather than the console:
#' \dontrun{
#' showVmGlgExamples(contract, file = "annuity-example.txt")
#' }
#' @export
showVmGlgExamples = function(contract, prf = 10, t = 10, t_prf = 12, file = "", ...) {

    has.prf = prf < contract$Parameters$ContractData$premiumPeriod;

    if (!("InsuranceContract" %in% class(contract))) {
        stop("First argument to function showVmGlgExamples need to be an InsuranceContract object! ",
             "Given object is of class: ",
             paste(class(contract), collapse = ", "));
    }
    if (has.prf) {
        contract.prf = contract$clone();
        contract.prf$premiumWaiver(t = prf)
    }

    output = c(
        sprintf("Tarif: %s", contract$tarif$name),
        contract$tarif$desc,
        sprintf("Typ: %s", contract$tarif$tariffType),
        sprintf("VS: %.2f", contract$Parameters$ContractData$sumInsured),
        sprintf("Alter: %d, LZ: %d, Pr\u00e4mienzahlung: %d, Aufschubzeit: %d, Garantiezeit: %d",
                contract$Parameters$ContractData$age,
                contract$Parameters$ContractData$policyPeriod,
                contract$Parameters$ContractData$premiumPeriod,
                contract$Parameters$ContractData$deferralPeriod,
                contract$Parameters$ContractData$guaranteedPeriod),

        sprintf("Rechenzins: %s, Sterbetafel: %s",
                percent(contract$Parameters$ActuarialBases$i),
                contract$Parameters$ActuarialBases$mortalityTable@name),
        "",

        "Pr\u00e4mien:",
        "========",
        sprintf("Nettopr\u00e4mie:         %8.2f", contract$Values$premiums["net"]),
        sprintf("Zillmerpr\u00e4mie:       %8.2f", contract$Values$premiums["Zillmer"]),
        sprintf("Bruttopr\u00e4mie:        %8.2f", contract$Values$premiums["gross"]),
        sprintf("Vorgeschr.Pr.:       %8.2f", contract$Values$premiums["written"]),
        "",

        sprintf("Sparpr\u00e4mie (t=%d):   %8.2f", t, contract$Values$premiumComposition[t + 1, "Zillmer.savings"]),
        sprintf("Risikopr\u00e4mie (t=%d): %8.2f", t, contract$Values$premiumComposition[t + 1, "Zillmer.risk"]),
        "",

        "Reserven:",
        "=========",
        sprintf("Pr\u00e4mienpflichtig (t=%d):             %8.2f", t, contract$Values$reserves[t + 1, "Zillmer"]),
        ifelse(has.prf, sprintf("Pr.Frei (mangels Zahlung) (tprf=%d): %8.2f", t_prf, contract.prf$Values$reserves[t_prf + 1, "Zillmer"]), ""),
        sprintf("VwKostenreserve (t=%d):              %8.2f", t, contract$Values$reserves[t + 1, "gamma"]),
        ifelse(has.prf, sprintf("VwKostenreserve (Pr.Frei) (tprf=%d): %8.2f", t_prf, contract.prf$Values$reserves[t_prf + 1, "gamma"]), ""),
        "",

        sprintf("Bilanzreserve (t=%.2f):             %8.2f",
                contract$Values$reservesBalanceSheet[t + 1, "time"],
                contract$Values$reservesBalanceSheet[t + 1, "Balance Sheet Reserve"]),
        sprintf("Pr\u00e4mien\u00fcbertrag (BM=%2d):             %8.2f", month(contract$Parameters$ContractData$contractClosing), contract$Values$reservesBalanceSheet[t + 1, "unearned Premiums"]),
        "",

        "R\u00fcckkauf und Pr\u00e4mienfreistellung:",
        "=================================",
        sprintf("R\u00fcckkaufsreserve (t=%d):          %8.2f", t, contract$Values$reserves[t + 1, "reduction"]),
        sprintf("R\u00fcckkaufswert (t=%d):             %8.2f", t, contract$Values$reserves[t + 1, "Surrender"]),
        sprintf("Abschlusskostenr\u00fccktrag (t=%d):   %8.2f", t, contract$Values$reserves[t + 1, "alphaRefund"]),
        "",

        ifelse(has.prf, sprintf("R\u00fcckkaufswert (Prf.) (t=%d):      %8.2f (VS: %.2f)",
                t_prf,
                contract.prf$Values$reserves[t_prf + 1, "Surrender"],
                contract.prf$Values$reserves[t_prf + 1, "PremiumFreeSumInsured"]), ""),
        "",

        ifelse(has.prf, sprintf("Pr\u00e4mienfreie VS (t=%d):           %8.2f",
                t, contract$Values$reserves[t + 1, "PremiumFreeSumInsured"]), "")
    );
    cat(paste(output, collapse = '\r\n'), file = file)

}


#' Perform unit tests of given standard values of the insurance contract example
#'
#' Check the values of the example calculation of the given insurance contract
#' as required by the Austrian regulation (LV-VMGLV, "LV
#' Versicherungsmathematische Grundlagen Verordnung").
#' Missing params not passed to the function call will be silently ignored and
#' not cause unit test failures.
#'
#' @param contract The insurance contract to calculate and check
#' @param t Time for which to check all values (except premium-free values)
#' @param prf Time of premium waiver (premium-free)
#' @param t_prf Time for which to check all values after the premium waiver
#' @param net, Zillmer, gross, written, savings, risk, ZillmerRes, ZillmerRes.prf, VwKostenRes, VwKostenRes.prf, Bilanzreserve, Prämienübertrag, Rückkaufsreserve, Rückkaufswert, Abschlusskostenrücktrag, Rückkaufswert.prf, VS.prf Values as printed out by showVmGlgExamples
#' @param tolerance If non-NULL, will ignore small floating point differences. It uses same algorithm as all.equal()
#'
#' @examples
#' library(MortalityTables)
#' mortalityTables.load("Austria_Annuities_AVOe2005R")
#'
#' \dontrun{
#' test_that("Testtarif", {
#'     # A trivial deferred annuity tariff with no costs:
#'     tariff = InsuranceTarif$new(name="Test Annuity", type="annuity",
#'         mortalityTable = AVOe2005R.unisex, i=0.01)
#'     contract = InsuranceContract$new(
#'         tariff,
#'         age = 35, YOB = 1981,
#'         policyPeriod = 30, premiumPeriod = 15, deferralPeriod = 15,
#'         sumInsured = 1000,
#'         contractClosing = as.Date("2016-10-01")
#'     );
#'
#'     testVmGlgExample(
#'         contract, t = 10,
#'         net = 850.09, # NOT_CHECKED: Zillmer = 950.09,
#'         gross = 850.09,
#'         written = 884.09,
#'         savings = 857.09, risk = -7.00,
#'         ZillmerRes = 9011.40,
#'         ZillmerRes.prf = 9205.96,
#'         VwKostenRes = 0.00,
#'         VwKostenRes.prf = 0.00,
#'         Bilanzreserve = 9250.35,
#'         Prämienübertrag = 212.52,
#'         Rückkaufsreserve = 9011.40,
#'         Rückkaufswert = 9011.40,
#'         Abschlusskostenrücktrag = 0.00,
#'         Rückkaufswert.prf = 9205.96,
#'         VS.prf = 685.12
#'     )
#' })
#'}
#'
#' @export
testVmGlgExample = function(contract, prf = 10, t = 10, t_prf = 12, net, Zillmer, gross, written, savings, risk,
                            ZillmerRes, ZillmerRes.prf, VwKostenRes, VwKostenRes.prf,
                            Bilanzreserve, Prämienübertrag,
                            Rückkaufsreserve, Rückkaufswert, Abschlusskostenrücktrag,
                            Rückkaufswert.prf, VS.prf, tolerance = 0.01
) {
    has.prf = prf < contract$Parameters$ContractData$premiumPeriod;

    if (has.prf) {
        contract.prf = contract$clone();
        contract.prf$premiumWaiver(t = prf)
    }
    if (!missing(net)) {
        eval(bquote(expect_equal(contract$Values$premiums[["net"]], .(net), tolerance = tolerance)))
    }
    if (!missing(Zillmer)) {
        eval(bquote(expect_equal(contract$Values$premiums[["Zillmer"]], .(Zillmer), tolerance = tolerance)))
    }
    if (!missing(gross)) {
        eval(bquote(expect_equal(contract$Values$premiums[["gross"]], .(gross), tolerance = tolerance)))
    }
    if (!missing(written)) {
        eval(bquote(expect_equal(contract$Values$premiums[["written"]], .(written), tolerance = tolerance)))
    }
    if (!missing(savings)) {
        eval(bquote(expect_equal(contract$Values$premiumComposition[[t + 1, "Zillmer.savings"]], .(savings), tolerance = tolerance)))
    }
    if (!missing(risk)) {
        eval(bquote(expect_equal(contract$Values$premiumComposition[[t + 1, "Zillmer.risk"]], .(risk), tolerance = tolerance)))
    }

    if (!missing(ZillmerRes)) {
        eval(bquote(expect_equal(contract$Values$reserves[[t + 1, "Zillmer"]], .(ZillmerRes), tolerance = tolerance)))
    }
    if (!missing(ZillmerRes.prf)) {
        eval(bquote(expect_equal(contract.prf$Values$reserves[[t_prf + 1, "Zillmer"]], .(ZillmerRes.prf), tolerance = tolerance)))
    }
    if (!missing(VwKostenRes)) {
        eval(bquote(expect_equal(contract$Values$reserves[[t + 1, "gamma"]], .(VwKostenRes), tolerance = tolerance)))
    }
    if (!missing(VwKostenRes.prf)) {
        eval(bquote(expect_equal(contract.prf$Values$reserves[[t_prf + 1, "gamma"]], .(VwKostenRes.prf), tolerance = tolerance)))
    }


    if (!missing(Bilanzreserve)) {
        eval(bquote(expect_equal(contract$Values$reservesBalanceSheet[[t + 1, "Balance Sheet Reserve"]], .(Bilanzreserve), tolerance = tolerance)))
    }
    if (!missing(Prämienübertrag)) {
        eval(bquote(expect_equal(contract$Values$reservesBalanceSheet[[t + 1, "unearned Premiums"]], .(Prämienübertrag), tolerance = tolerance)))
    }

    if (!missing(Rückkaufsreserve)) {
        eval(bquote(expect_equal(contract$Values$reserves[[t + 1, "reduction"]], .(Rückkaufsreserve), tolerance = tolerance)))
    }
    if (!missing(Rückkaufswert)) {
        eval(bquote(expect_equal(contract$Values$reserves[[t + 1, "Surrender"]], .(Rückkaufswert), tolerance = tolerance)))
    }
    if (!missing(Abschlusskostenrücktrag)) {
        eval(bquote(expect_equal(contract$Values$reserves[[t + 1, "alphaRefund"]], .(Abschlusskostenrücktrag), tolerance = tolerance)))
    }
    if (!missing(Rückkaufswert.prf)) {
        eval(bquote(expect_equal(contract.prf$Values$reserves[t_prf + 1, "Surrender"], .(Rückkaufswert.prf), tolerance = tolerance)))
    }
    if (!missing(VS.prf)) {
        eval(bquote(expect_equal(contract.prf$Values$reserves[t_prf + 1, "PremiumFreeSumInsured"], .(VS.prf), tolerance = tolerance)))
        # OR: contract$Values$reserves[t + 1, "PremiumFreeSumInsured"]
    }
}
