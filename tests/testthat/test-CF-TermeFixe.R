test_that("Termfix Cash Flows", {
	library(dplyr)
	library(MortalityTables)
    mortalityTables.load("Austria_Census")

    Tarif.Termfix = InsuranceTarif$new(
        name = "Example Tariff - Terme Fixe",
        type = "terme-fix",
        tarif = "TF",
        desc = "A termfix insurance with regular premiums",

        mortalityTable = mort.AT.census.2011.unisex,
        i = 0.005
    )
    Contract.Termfix = InsuranceContract$new(
        tarif = Tarif.Termfix,
        age = 40, policyPeriod = 20,
        sumInsured = 10000,
        contractClosing = as.Date("2020-09-01"),
        calculate = "cashflows"
    )
    expect_equal(Contract.Termfix$getPolicyTerm(), 20)
    expect_equal(Contract.Termfix$Parameters$ContractData$deferralPeriod, 0)
    expect_equal(Contract.Termfix$getPremiumTerm(), 20)


    expect_true(all(Contract.Termfix$Values$cashFlows %>% select(-premiums_advance, -guaranteed_advance) == 0))

    expect_equal(Contract.Termfix$Values$cashFlows$premiums_advance, c(rep(1, 20), 0))
    expect_equal(Contract.Termfix$Values$cashFlows$guaranteed_advance, c(rep(0, 20), 1))
})
test_that("Termfix Cash Flows (alternative name)", {
	library(dplyr)
	library(MortalityTables)
	mortalityTables.load("Austria_Census")

	Tarif.Termfix = InsuranceTarif$new(
		name = "Example Tariff - Terme Fixe",
		type = "termfix",
		tarif = "TF",
		desc = "A termfix insurance with regular premiums",

		mortalityTable = mort.AT.census.2011.unisex,
		i = 0.005
	)
	Contract.Termfix = InsuranceContract$new(
		tarif = Tarif.Termfix,
		age = 40, policyPeriod = 20,
		sumInsured = 10000,
		contractClosing = as.Date("2020-09-01"),
		calculate = "cashflows"
	)
	expect_equal(Contract.Termfix$getPolicyTerm(), 20)
	expect_equal(Contract.Termfix$Parameters$ContractData$deferralPeriod, 0)
	expect_equal(Contract.Termfix$getPremiumTerm(), 20)


	expect_true(all(Contract.Termfix$Values$cashFlows %>% select(-premiums_advance, -guaranteed_advance) == 0))

	expect_equal(Contract.Termfix$Values$cashFlows$premiums_advance, c(rep(1, 20), 0))
	expect_equal(Contract.Termfix$Values$cashFlows$guaranteed_advance, c(rep(0, 20), 1))
})

