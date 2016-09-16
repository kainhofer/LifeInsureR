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
#' mortalityTables.load("Austria_Annuities_AVOe2005R")
#' # A trivial deferred annuity tariff with no costs:
#' tariff = InsuranceTarif$new(name="Test Annuity", type="annuity", mortalityTable = AVOe2005R.unisex, i=0.01)
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
#' showVmGlgExamples(contract, file = "annuity-example.txt")
#'
#' @export
showVmGlgExamples = function(contract, prf=10, t=10, t_prf=12, file="", ...) {

    has.prf = prf <= contract$Parameters$ContractData$premiumPeriod;

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
        sprintf("Alter: %d, LZ: %d, Prämienzahlung: %d, Aufschubzeit: %d, Garantiezeit: %d",
                contract$Parameters$ContractData$age,
                contract$Parameters$ContractData$policyPeriod,
                contract$Parameters$ContractData$premiumPeriod,
                contract$Parameters$ContractData$deferralPeriod,
                contract$Parameters$ContractData$guaranteedPeriod),

        sprintf("Rechenzins: %s, Sterbetafel: %s",
                percent(contract$Parameters$ActuarialBases$i),
                contract$Parameters$ActuarialBases$mortalityTable@name),
        "",

        "Prämien:",
        "========",
        sprintf("Nettoprämie:         %8.2f", contract$Values$premiums["net"]),
        sprintf("Zillmerprämie:       %8.2f", contract$Values$premiums["Zillmer"]),
        sprintf("Bruttoprämie:        %8.2f", contract$Values$premiums["gross"]),
        sprintf("Vorgeschr.Pr.:       %8.2f", contract$Values$premiums["written"]),
        "",

        sprintf("Sparprämie (t=%d):   %8.2f", t, contract$Values$premiumComposition[t + 1, "Zillmer.savings"]),
        sprintf("Risikoprämie (t=%d): %8.2f", t, contract$Values$premiumComposition[t + 1, "Zillmer.risk"]),
        "",

        "Reserven:",
        "=========",
        sprintf("Prämienpflichtig (t=%d):             %8.2f", t, contract$Values$reserves[t + 1, "Zillmer"]),
        ifelse(has.prf, sprintf("Pr.Frei (mangels Zahlung) (tprf=%d): %8.2f", t_prf, contract.prf$Values$reserves[t_prf + 1, "Zillmer"]), ""),
        sprintf("VwKostenreserve (t=%d):              %8.2f", t, contract$Values$reserves[t + 1, "gamma"]),
        ifelse(has.prf, sprintf("VwKostenreserve (Pr.Frei) (tprf=%d): %8.2f", t_prf, contract.prf$Values$reserves[t_prf + 1, "gamma"]), ""),
        "",

        sprintf("Bilanzreserve (t=%.2f):             %8.2f",
                contract$Values$reservesBalanceSheet[t + 1, "time"],
                contract$Values$reservesBalanceSheet[t + 1, "Balance Sheet Reserve"]),
        sprintf("Prämienübertrag (t=%.2f): NICHT IMPLEMENTIERT", contract$Values$reservesBalanceSheet[t + 1, "time"]),
        "",

        "Rückkauf und Prämienfreistellung:",
        "=================================",
        sprintf("Rückkaufsreserve (t=%d):          %8.2f", t, contract$Values$reserves[t + 1, "reduction"]),
        sprintf("Rückkaufswert (t=%d):             %8.2f", t, contract$Values$reserves[t + 1, "Surrender"]),
        sprintf("Abschlusskostenrücktrag (t=%d):   %8.2f", t, contract$Values$reserves[t + 1, "alphaRefund"]),
        "",

        ifelse(has.prf, sprintf("Rückkaufswert (Prf.) (t=%d):      %8.2f (VS: %.2f)",
                t_prf,
                contract.prf$Values$reserves[t + 1, "Surrender"],
                contract.prf$Values$reserves[t + 1, "PremiumFreeSumInsured"]), ""),
        "",

        ifelse(has.prf, sprintf("Prämienfreie VS (t=%d):           %8.2f",
                t, contract$Values$reserves[t + 1, "PremiumFreeSumInsured"]), "")
    );
    cat(paste(output, collapse = '\n'), file = file)

}
