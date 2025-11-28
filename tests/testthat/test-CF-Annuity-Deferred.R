test_that("Deferred Annuity Cash Flows", {
    library(MortalityTables)
    mortalityTables.load("Austria_Annuities_AVOe2005R")

    Tarif.DefAnnuity = InsuranceTarif$new(
        name = "Example Tariff - Deferred Annuity",
        type = "annuity",
        tarif = "Life1",
        desc = "A deferred annuity (life-long payments start at age 65) with reg. premiums",

        policyPeriod = function(params, values) { 120 - params$ContractData$age},
        deferralPeriod = function(params, values) { 65 - params$ContractData$age},
        premiumPeriod = function(params, values) { 65 - params$ContractData$age},
        premiumRefund = 1,

        mortalityTable = AVOe2005R.unisex,
        i = 0.005
    )
    Contract.DefAnnuity = InsuranceContract$new(
        tarif = Tarif.DefAnnuity,
        age = 40, YOB = 1980,
        sumInsured = 1200,
        guaranteedPeriod = 15,
        contractClosing = as.Date("2020-09-01"),
        calculate = "cashflows"
    )
    expect_equal(Contract.DefAnnuity$Parameters$ContractData$policyPeriod, 80)
    expect_equal(Contract.DefAnnuity$Parameters$ContractData$deferralPeriod, 25)
    expect_equal(Contract.DefAnnuity$Parameters$ContractData$premiumPeriod, 25)


    expect_true(all(Contract.DefAnnuity$Values$cashFlows %>% dplyr::select(-premiums_advance, -survival_advance, -death_GrossPremium, -death_Refund_past) == 0))

    # 25 years premium cash flow
    expect_equal(Contract.DefAnnuity$Values$cashFlows$premiums_advance, c(rep(1, 25), rep(0, 56)))
    # premium payment start after 25 years
    expect_equal(Contract.DefAnnuity$Values$cashFlows$survival_advance, c(rep(0, 25), rep(1, 55),0))
    # premium payment start after 25 years
    expect_equal(Contract.DefAnnuity$Values$cashFlows$death_GrossPremium, c(1:25, rep(0, 56)))
    # death refund flag
    expect_equal(Contract.DefAnnuity$Values$cashFlows$death_Refund_past, c(rep(1, 25), rep(0, 56)))

    Contract.DefAnnuity.costs = InsuranceContract$new(
    	tarif = Tarif.DefAnnuity,
    	age = 40, YOB = 1980,
    	sumInsured = 1200,
    	contractClosing = as.Date("2020-09-01"),
    	calculate = "cashflows",
    	# 20 years contract, with 10 years deferral (7 premium payments), and 10 year term annuity (5 guaranteed)
    	policyPeriod = 20,
    	deferralPeriod = 10,
    	premiumPeriod = 7,
    	guaranteedPeriod = 5,
    	costs = initializeCosts(alpha = 0.02, alpha.ongoing = 0.01, gamma = 0.02, gamma.premiumfree = 0.025) %>%
    		setCost("gamma", "SumInsured", "PaymentPeriod", 0.005)
    )

    # Gamma costs: 0.02 during premium payments, 0.025 after premium payments, additionally 0.005 during annuity phase
    gamma.expected = c(rep(0.02, 7), rep(0.025, 3), rep(0.03, 10), 0);
    names(gamma.expected) = 0:20
    expect_equal(Contract.DefAnnuity.costs$Values$cashFlowsCosts[,"gamma", "SumInsured", "survival"], gamma.expected)
})
