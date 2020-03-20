library(LifeInsuranceContracts)

################################################################### #
#              DEFINITION TARIF                                  ####
################################################################### #
#
# Beispieltarif:
#    endowment with regular premiums
#    death benefit = survival benefit
#    Costs: Alpha: 4% of premium sum up-front (2,5% Zillmer)
#           Beta:  5% of each premium paid
#           Gamma: 0,1% of sum insured per year over the whole contract maturity
#           Unitcosts: 10 EUR + 5% Premium Sum (max. 50 EUR), during premium period
################################################################### #

costs.Bsp = initializeCosts();
costs.Bsp[["alpha", "SumPremiums", "once"]] = 0.04;
costs.Bsp[["Zillmer", "SumPremiums", "once"]] = 0.025; # deutsche Beschränkung der Zillmerung
costs.Bsp[["beta", "GrossPremium", "PremiumPeriod"]] = 0.05;
costs.Bsp[["gamma", "SumInsured", "PolicyPeriod"]] = 0.001;

costs.Bsp.Unterjaehrigkeit = list("1" = 0.0, "2" = 0.01, "4" = 0.015, "12" = 0.02);

# Stückkosten: 10EUR + 5% PS, maximal 50
costs.Bsp.Stueckkosten       = function (params, values) { min(50, 10 + 0.05*values$premiums[["gross"]]) }


surrender.Bsp = function(surrenderReserve, params, values) {
  n = params$ContractData$policyPeriod - params$ContractData$blockStart;
  # Rückkaufsabschlag linear fallend von 10 auf 0%:
  sf = c(rep(0, params$ContractData$blockStart), 1 - 0.1 * (1 - (0:n)/n));
  surrenderReserve * sf
}

#' @export
Tarif.Bsp = InsuranceTarif$new(
  name = "Example Tariff - Standard Endowment",
  type = "endowment",
  tarif = "BSP",
  desc = "Gemischte Versicherung (Standardtarif)",
  #premiumPeriod = 1,
  #alphaRefundLinear = FALSE,

  mortalityTable = mort.AT.census.2011.unisex,
  i = 0.005,
  costs = costs.Bsp,
  unitcosts = costs.Bsp.Stueckkosten,

  premiumFrequencyOrder = -1, # Unterjährige Prämienzahlung wird nicht im BW berücksichtigt, sondern durch Prämienaufschlag
  premiumFrequencyLoading = costs.Bsp.Unterjaehrigkeit,
  premiumRefund = 0,
  tax = 0.04,

  surrenderValueCalculation = surrender.Bsp
);

################################################################### #
# Example contract with dynamiocs                               ####
################################################################### #


contract.Bsp = InsuranceContract$
  new(
    Tarif.Bsp,
    age = 35, policyPeriod = 15, premiumPeriod = 15,
    premiumFrequency = 12,
    sumInsured = 100000,
    contractClosing = as.Date("2020-07-01"),
    id = "Hauptvertrag"
  )$
  addDynamics(t = 5, NewSumInsured = 200000, id = "Dynamik 1", i = 0.05, age = 70)$
  addDynamics(t = 10, NewSumInsured = 250000, id = "Dynamik 2", i = 0.01);
# exportInsuranceContractExample(contract.Bsp, t = 5);
# showVmGlgExamples(contract.Bsp, t = 10)
#
# exportInsuranceContract.xlsx(contract.Bsp, filename = "BEISPIEL_Contract.xlsx")
#
#
# contract.Bsp$Values$cashFlowsBasic
# contract.Bsp$Values$cashFlows
# contract.Bsp$Values$cashFlowsCosts
# contract.Bsp$Values$presentValues
# # contract.U17_3J$Values$presentValuesCosts
# contract.Bsp$Values$premiumSum
# contract.Bsp$Values$premiums
# contract.Bsp$Values$premiumComposition
# contract.Bsp$Values$reserves


################################################################### #
# Testing premium waivers                                        ####
################################################################### #


contract.Bsp.waiver = InsuranceContract$
  new(
    Tarif.Bsp,
    age = 35, policyPeriod = 15, premiumPeriod = 15,
    premiumFrequency = 12,
    sumInsured = 100000,
    contractClosing = as.Date("2020-07-01"),
    id = "Hauptvertrag"
  )$premiumWaiver(t = 5)
exportInsuranceContract.xlsx(contract.Bsp.waiver, filename = "BEISPIEL_Contract_PrfALT.xlsx")

contract.Bsp.waiverNew = InsuranceContract$
  new(
    Tarif.Bsp,
    age = 35, policyPeriod = 15, premiumPeriod = 15,
    premiumFrequency = 12,
    sumInsured = 100000,
    contractClosing = as.Date("2020-07-01"),
    id = "Hauptvertrag"
  )$premiumWaiverNew(t = 5)
exportInsuranceContract.xlsx(contract.Bsp.waiverNew, filename = "BEISPIEL_Contract_PrfNEU.xlsx")

contract.Bsp.waiver$Values$cashFlowsBasic == contract.Bsp.waiverNew$Values$cashFlowsBasic
contract.Bsp.waiver$Values$cashFlows == contract.Bsp.waiverNew$Values$cashFlows
contract.Bsp.waiver$Values$cashFlowsCosts == contract.Bsp.waiverNew$Values$cashFlowsCosts
contract.Bsp.waiver$Values$presentValues == contract.Bsp.waiverNew$Values$presentValues
contract.Bsp.waiver$Values$presentValuesCosts == contract.Bsp.waiverNew$Values$presentValuesCosts

contract.Bsp.waiver$Values$reserves == contract.Bsp.waiverNew$Values$reserves
contract.Bsp.waiver$Values$reservesBalanceSheet == contract.Bsp.waiverNew$Values$reservesBalanceSheet

contract.Bsp.waiver$Values$absCashFlows == contract.Bsp.waiverNew$Values$absCashFlows
contract.Bsp.waiver$Values$absPresentValues == contract.Bsp.waiverNew$Values$absPresentValues
contract.Bsp.waiver$Values$premiumComposition == contract.Bsp.waiverNew$Values$premiumComposition
contract.Bsp.waiver$Values$premiumCompositionSums == contract.Bsp.waiverNew$Values$premiumCompositionSums
contract.Bsp.waiver$Values$premiumCompositionPV == contract.Bsp.waiverNew$Values$premiumCompositionPV
contract.Bsp.waiver$Values$basicData == contract.Bsp.waiverNew$Values$basicData

