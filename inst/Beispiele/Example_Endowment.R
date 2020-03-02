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
#  browser()
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

if (false) {
  contract.Bsp.DynStart = InsuranceContract$new(
    Tarif.Bsp,
    age = 45, policyPeriod = 5, premiumPeriod = 5,
    premiumFrequency = 12,
    sumInsured = 100000,
    contractClosing = as.Date("2030-07-01")
  );
  exportInsuranceContract.xlsx(contract.Bsp.DynStart, t = 5, basename = "Endowment_Dynamic_From0_Baseline");
  # exportInsuranceContractExample(contract.Bsp.DynStart, t = 5, basename = "Endowment_Dynamic_From0_Baseline");
  # showVmGlgExamples(contract.Bsp.Dyn, t = 10)
}


contract.Bsp.DynStart$Values$cashFlowsBasic
contract.Bsp.DynStart$Values$cashFlows
contract.Bsp.DynStart$Values$cashFlowsCosts
contract.Bsp.DynStart$Values$presentValues


contract.Bsp.Dyn = InsuranceContract$new(
  Tarif.Bsp,
  age = 35, policyPeriod = 15, premiumPeriod = 15,
  premiumFrequency = 12,
  sumInsured = 100000,
  contractClosing = as.Date("2020-07-01"),
  blockStart = 10
);
exportInsuranceContract.xlsx(contract.Bsp.Dyn, filename = "Endowment_Dynamic_From5.xlsx");


# exportInsuranceContractExample(contract.Bsp.Dyn, t = 5, basename = "Endowment_Dynamic_From5");
# showVmGlgExamples(contract.Bsp.Dyn, t = 10)


contract.Bsp.Dyn$Values$cashFlowsBasic
contract.Bsp.Dyn$Values$cashFlows
contract.Bsp.Dyn$Values$cashFlowsCosts
contract.Bsp.Dyn$Values$presentValues
contract.Bsp.Dyn$Values$reserves
contract.Bsp.Dyn$Values$premiumComposition


################################################################### #
#              EXAMPLE CONTRACT                                  ####
################################################################### #


contract.Bsp = InsuranceContract$new(
  Tarif.Bsp,
  age = 35, policyPeriod = 15, premiumPeriod = 15,
  premiumFrequency = 12,
  sumInsured = 100000,
  contractClosing = as.Date("2020-07-01")
);
exportInsuranceContractExample(contract.Bsp, t = 5);
showVmGlgExamples(contract.Bsp, t = 10)


contract.Bsp$Values$cashFlowsBasic
contract.Bsp$Values$cashFlows
contract.Bsp$Values$cashFlowsCosts
contract.Bsp$Values$presentValues
# contract.U17_3J$Values$presentValuesCosts
contract.Bsp$Values$premiumSum
contract.Bsp$Values$premiums
contract.Bsp$Values$premiumComposition
contract.Bsp$Values$reserves


################################################################### #
#              DYNAMIC INCREASE                                  ####
################################################################### #


contract.Bsp.Dyn = InsuranceContract$new(
  Tarif.Bsp,
  age = 35, policyPeriod = 15, premiumPeriod = 15,
  premiumFrequency = 12,
  sumInsured = 100000,
  contractClosing = as.Date("2020-07-01"),
  blockStart = 10
);
exportInsuranceContractExample(contract.Bsp.Dyn, t = 5);
showVmGlgExamples(contract.Bsp.Dyn, t = 10)


contract.Bsp.Dyn$Values$cashFlowsBasic
contract.Bsp.Dyn$Values$cashFlows
contract.Bsp.Dyn$Values$cashFlowsCosts
contract.Bsp.Dyn$Values$presentValues
# contract.Bsp.Dyn$Values$presentValuesCosts
contract.Bsp.Dyn$Values$premiumSum
contract.Bsp.Dyn$Values$premiums
contract.Bsp.Dyn$Values$premiumComposition
contract.Bsp.Dyn$Values$reserves
contract.Bsp.Dyn$Values$basicData

