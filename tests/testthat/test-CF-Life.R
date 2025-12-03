test_that("Term Life Cash Flows", {
    library(MortalityTables)
    mortalityTables.load("Austria_Census")

    Tarif.Life = InsuranceTarif$new(
        name = "Example Tariff - Whole/Term Life",
        type = "wholelife",
        tarif = "Life1",
        desc = "A whole or term life insurance with regular premiums",

        mortalityTable = mort.AT.census.2011.unisex,
        i = 0.005
    )

    Contract.Life = InsuranceContract$new(
        tarif = Tarif.Life,
        age = 40, policyPeriod = 20,
        sumInsured = 10000,
        contractClosing = as.Date("2020-09-01"),
        calculate = "cashflows"
    )
    expect_equal(Contract.Life$getPolicyTerm(), 20)
    expect_equal(Contract.Life$Parameters$ContractData$deferralPeriod, 0)
    expect_equal(Contract.Life$getPremiumTerm(), 20)


    expect_true(all(Contract.Life$Values$cashFlows %>% select(-premiums_advance, -death_SumInsured, -death_PremiumFree) == 0))

    expect_equal(Contract.Life$Values$cashFlows$premiums_advance, c(rep(1, 20), 0))
    expect_equal(Contract.Life$Values$cashFlows$death_SumInsured, c(rep(1, 20), 0))
    expect_equal(Contract.Life$Values$cashFlows$death_PremiumFree, c(rep(1, 20), 0))
})
test_that("Whole Life Cash Flows", {
  library(MortalityTables)
  mortalityTables.load("Austria_Census")

  Tarif.Life = InsuranceTarif$new(
    name = "Example Tariff - Whole/Term Life",
    type = "wholelife",
    tarif = "Life1",
    desc = "A whole or term life insurance with regular premiums",
    policyPeriod = Inf,
    premiumPeriod = Inf,
    mortalityTable = mort.AT.census.2011.unisex,
    i = 0.005
  )

  Contract.Life = InsuranceContract$new(
    tarif = Tarif.Life,
    age = 40,
    sumInsured = 10000,
    contractClosing = as.Date("2020-09-01")#,
#    calculate = "cashflows"
  )
  # Premium and policy terms are cut at the max age of the mortality table:
  expect_equal(Contract.Life$getPolicyTerm(), 60)
  expect_equal(Contract.Life$getPremiumTerm(), 60)

  # Death benefit cashflows last until the very last year:
  expect_true(all(head(Contract.Life$Values$cashFlows$death_SumInsured, -1) == 1))
})
