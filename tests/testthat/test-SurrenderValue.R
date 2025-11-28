test_that("SurrenderValue", {
	library(MortalityTables)
	mortalityTables.load("Austria_Census")

	Tarif.Endowment = InsuranceTarif$new(
		name = "Example Tariff - Endowment",
		type = "endowment",
		tarif = "E1-RP",
		desc = "An endowment with regular premiums (standard tariff)",

		surrenderValueCalculation = function(resReduction, ...) { resReduction/2 },
		mortalityTable = mort.AT.census.2011.unisex,
		i = 0.005
	)
	# surrender penalty applied on surrender and premium waivers:
	Contract.Endowment = InsuranceContract$new(
		tarif = Tarif.Endowment,
		age = 40, policyPeriod = 20,
		sumInsured = 10000,
		contractClosing = as.Date("2020-09-01"),
		calculate = "reserves"
	)$premiumWaiver(10)
	# surrender penalty only applied on real surrender, not on premium waivers:
	Contract.Endowment.WaiverNo = InsuranceContract$new(
		tarif = Tarif.Endowment,
		age = 40, policyPeriod = 20,
		sumInsured = 10000,
		contractClosing = as.Date("2020-09-01"),
		surrenderPenaltyOnPremiumWaiver = FALSE,
		calculate = "reserves"
	)$premiumWaiver(10)

	# Case without penalty on waiver has double the premiumfree sum insured:
	expect_equal(Contract.Endowment$Values$reserves[,"PremiumFreeSumInsured"], Contract.Endowment.WaiverNo$Values$reserves[,"PremiumFreeSumInsured"]/2)
	# Surrender value is always the same:
	expect_equal(Contract.Endowment$Values$reserves[,"Surrender"], Contract.Endowment.WaiverNo$Values$reserves[,"Surrender"])

	# After premium waiver, all reserves (except premium free SI, premiums paid and surrender value) differ by a factor 1/2:
	expect_equal(
		Contract.Endowment$Values$reserves[11:21,1:9],
		Contract.Endowment.WaiverNo$Values$reserves[11:21,1:9]/2
	)
})
