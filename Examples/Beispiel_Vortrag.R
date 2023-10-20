# 2.2 Tariff implementation (InsuranceTarif)

library(magrittr)
library(MortalityTables)
library(lubridate)
library(LifeInsureR)
mortalityTables.load("Austria_Census")

mort.AT.census.2011.male

Tarif.L71U = InsuranceTarif$new(
    name = "L71-U",
    type = "wholelife",
    tarif = "DeathPlus - Short Term Life Insurance",
    desc = "Term Life insurance (5 years) with constant sum insured and regular premiums",
    policyPeriod = 5, premiumPeriod = 5,  # premiumPeriod not needed, defaults to maturity

    mortalityTable = mortalityTable.mixed(
        table1 = mort.AT.census.2011.male, weight1 = 0.65,
        table2 = mort.AT.census.2011.female, weight2 = 0.35
    ),
    i = 0.005,
    tax = 0.04,
    costs = initializeCosts(alpha = 0.05, gamma = 0.01, gamma.paidUp = 0.01, unitcosts = 10),
    surrenderValueCalculation = function(surrenderReserve, params, values) {
        surrenderReserve * 0.9
    }
);



# 2.3 Creating a contract

contract.L71U  = InsuranceContract$new(
    Tarif.L71U,
    age = 35,
    contractClosing = as.Date("2020-08-18"),
    sumInsured = 100000);
contract.L71U.1996R  = InsuranceContract$new(
    Tarif.L71U,
    age = 35,
    contractClosing = as.Date("2020-08-18"),
    mortalityTable = AVOe1996R.male,
    sumInsured = 100000);

contract.L71U$Values$premiums
contract.L71U.1996R$Values$premiums

library(openxlsx)
exportInsuranceContract.xlsx(contract.L71U, "L71U.xlsx")
openXL("L71U.xlsx")


contract.L71U$Values$premiums
contract.L71U$Values$reserves


contract.L71U$Values$cashFlows
contract.L71U$Values$cashFlowsCosts[,,,"survival"]
contract.L71U$Parameters$Costs %>% costsDisplayTable()

contract.L71U$Values$presentValues
contract.L71U$Values$presentValuesCosts


VMGL.contract = InsuranceContract$new(
    Tarif.L71U,
    age = 35, policyPeriod = 5, premiumPeriod
    sumInsured = 100000,
    contractClosing = as.Date("2020-07-01")
)

showVmGlgExamples(VMGL.contract, t_prf = 3, t = 3)






library(MortalityTables)
mortalityTables.load("Austria_Census")
mortalityTables.load("Austria_Annuities_AVOe2005R")
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

contract.Endowment.Dynamics = InsuranceContract$new(
    tarif = Tarif.Endowment,
    sumInsured = 10000,
    age = 40,
    policyPeriod = 10,
    contractClosing = as.Date("2020-09-01"),
    id = "Initial contract"
)$
    addDynamics(t = 5, NewSumInsured = 11000, id = "Dynamic at 5")$
    addDynamics(t = 7, NewSumInsured = 12000, id = "Dynamic at 7")$
    addDynamics(t = 8, NewSumInsured = 13500, id = "Dynamic at 8")

contract.Endowment.Dynamics
exportInsuranceContract.xlsx(contract.Endowment.Dynamics, "dyn.xlsx")
openXL("dyn.xlsx")


contract.Endowment.Dynamics = InsuranceContract$new(
    tarif = Tarif.Endowment,
    sumInsured = 10000,
    age = 40,
    policyPeriod = 10,
    contractClosing = as.Date("2020-09-01"),
    id = "Initial contract"
)$addBlock(t = 3, tarif = Tarif.Endowment, age = 43, policyPeriod = 7,
           sumInsured = 1000, premiumPeriod = 1, id = "Einmalzuzahlung at 5")

contract.L71U.prf = contract.L71U$premiumWaiver(t = 3)
contract.L71U.prf$Values$reservesBalanceSheet

contract.L71U.prf$Values$cashFlows

grd = contractGridPremium(
    axes = list(mortalityTable = mort.AT.census["m", ], age = seq(20, 80, 10)),
    tarif = Tarif.L71U,
    sumInsured = 100000,
    contractClosing = as.Date("2020-08-18")
)
