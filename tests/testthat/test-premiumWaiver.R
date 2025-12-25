test_that("SurrenderValue", {
	library(MortalityTables)
	mortalityTables.load("Austria_Census")

	Tarif.Endowment = InsuranceTarif$new(
		name = "Example Tariff - Endowment",
		type = "endowment",
		tarif = "E1-RP",
		desc = "An endowment with regular premiums (standard tariff)",

		mortalityTable = mort.AT.census.2011.unisex,
		i = 0.03
	)
	# 20-year contract, premium waiver after 10 years
	Contract.Endowment = InsuranceContract$new(
		tarif = Tarif.Endowment,
		age = 40, policyPeriod = 20,
		sumInsured = 10000,
		contractClosing = as.Date("2020-09-01")
	)


	Contract.EndowmentDyn = Contract.Endowment$copy()$addDynamics(t = 3, SumInsuredDelta = 1000)$addDynamics(t = 9, SumInsuredDelta = 1200)
	Contract.EndowmentDynPaidUp = Contract.EndowmentDyn$copy()$premiumWaiver(10)
	expect_equal(
	  Contract.EndowmentDyn$Values$reserves[["10", "PremiumFreeSumInsured"]],
	  Contract.EndowmentDynPaidUp$Values$reserves[["19", "SumInsured"]]
	)
})
