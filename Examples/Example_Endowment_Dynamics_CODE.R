library(magrittr)
library(MortalityTables)
library(LifeInsuranceContracts)
mortalityTables.load("Austria_Census")

# Costs: 4% acquisition, where 2.5% are zillmered, 5\% of each premium as beta costs,
#        1%o acquisition costs of the sum insured over the whole contract period
example.Costs = initializeCosts(
    alpha = 0.04, Zillmer = 0.025,
    beta = 0.05,
    gamma.contract = 0.001, gamma.paidUp = 0.001
)
example.Surrender = function(surrenderReserve, params, values) {
    n = params$ContractData$policyPeriod
    # Surrender Penalty is 10% at the beginning and decreases linearly to 0%
    surrenderReserve * (0.9 + 0.1 * (0:n)/n)
}

Tarif.Endowment = InsuranceTarif$new(
    name = "Example Tariff - Endowment",
    type = "endowment",
    tarif = "EN1",
    desc = "An endowment with regular premiums",

    mortalityTable = mort.AT.census.2011.unisex,
    i = 0.005,
    costs = example.Costs,
    unitcosts = 10,
    tax = 0.04,         # 4% insurance tax
    surrenderValueCalculation = example.Surrender
)

contract.Endowment = InsuranceContract$new(
    Tarif.Endowment,
    age = 50, policyPeriod = 35,
    premiumFrequency = 12,
    sumInsured = 100000,
    contractClosing = as.Date("2020-07-01")
)
contract.exportExample = contract.Endowment$clone()$
    addDynamics(t = 3, SumInsuredDelta = 10000)$
    addDynamics(t = 5, SumInsuredDelta = 15000)$
    addDynamics(t = 10, SumInsuredDelta = 15000)$
    addDynamics(t = 14, SumInsuredDelta = 10000)
exportInsuranceContract.xlsx(contract.exportExample, filename = "Example_Endowment_Dynamics.xlsx")
openxlsx::openXL("Example_Endowment_Dynamics.xlsx")
