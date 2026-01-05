test_that("Rounding Helper", {

    rounding = RoundingHelper$new(test = 2, gross.premium = 0, "Sum Insured" = -2)

    rounding$rounding

    expect_equal(
        rounding$round("test", c(2*10^(-8:5), 987654321.987654321)),
        c(0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.02, 0.20, 2.00, 20.00, 200.00, 2000.00, 20000.00, 200000.00, 987654321.99)
    )

    expect_equal(
        rounding$round("gross.premium", c(2*10^(-8:5), 987654321.987654321)),
        c(0, 0, 0, 0, 0, 0, 0, 0, 2, 20, 200, 2000, 20000, 200000, 987654322)
    )

    expect_equal(
        rounding$round("Sum Insured", c(2*10^(-8:5), 987654321.987654321)),
        c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 200, 2000, 20000, 200000, 987654300)
    )

    expect_equal(
        rounding$round("NotExisting", c(2*10^(-8:5), 987654321.987654321)),
        c(2*10^(-8:5), 987654321.987654321)
    )

})

# Test round_half_up:
testthat::test_that("round_half_up rounds .5 ties away from zero (digits = 0)", {
  x <- c(-2.5, -1.5, -0.5, 0.5, 1.5, 2.5)
  testthat::expect_equal(round_half_up(x), c(-3, -2, -1, 1, 2, 3))

  x2 <- c(-10.5, -9.5, 9.5, 10.5)
  testthat::expect_equal(round_half_up(x2), c(-11, -10, 10, 11))
})

testthat::test_that("round_half_up rounds .5 ties away from zero (positive digits)", {
  x <- c(-1.25, -1.15, -1.05, -0.05, 0.05, 1.05, 1.15, 1.25)

  # digits = 1 means ties at 0.05
  testthat::expect_equal(
    round_half_up(x, digits = 1),
    c(-1.3, -1.2, -1.1, -0.1, 0.1, 1.1, 1.2, 1.3)
  )

  # digits = 2 means ties at 0.005
  y <- c(-0.005, 0.005, -1.005, 1.005)
  testthat::expect_equal(round_half_up(y, digits = 2), c(-0.01, 0.01, -1.01, 1.01))
})

testthat::test_that("round_half_up handles negative digits (.5 ties)", {
  x <- c(-150, -50, -15, -5, 5, 15, 50, 150)

  # digits = -1 ties at 5 (e.g., 15 -> 20, -15 -> -20)
  testthat::expect_equal(
    round_half_up(x, digits = -1),
    c(-150, -50, -20, -10, 10, 20, 50, 150)
  )

  # digits = -2 ties at 50 (e.g., 150 -> 200, -150 -> -200)
  testthat::expect_equal(
    round_half_up(x, digits = -2),
    c(-200, -100, 0, 0, 0, 0, 100, 200)
  )
})

testthat::test_that("round_half_up preserves dimensions and names", {
  m <- matrix(c(0.5, 1.5, 2.25, -2.5), nrow = 2,
              dimnames = list(c("r1", "r2"), c("c1", "c2")))
  out <- round_half_up(m)

  testthat::expect_true(is.matrix(out))
  testthat::expect_identical(dim(out), dim(m))
  testthat::expect_identical(dimnames(out), dimnames(m))
  testthat::expect_equal(out, matrix(c(1, 2, 2, -3), nrow = 2,
                                     dimnames = dimnames(m)))
})

testthat::test_that("round_half_up propagates NA/NaN/Inf", {
  x <- c(NA_real_, NaN, Inf, -Inf, 0.5)
  out <- round_half_up(x)

  testthat::expect_true(is.na(out[1]))
  testthat::expect_true(is.nan(out[2]))
  testthat::expect_equal(out[3], Inf)
  testthat::expect_equal(out[4], -Inf)
  testthat::expect_equal(out[5], 1)
})

testthat::test_that("round_half_up matches base::round when not exactly at .5 ties", {
  # Construct values that are safely away from ties at the chosen digits.
  # For digits=2, ties occur at k + 0.005. We avoid that by using increments of 0.01
  # plus an offset not equal to 0.005 (e.g. 0.002).
  set.seed(1)
  base_vals <- sample(-1000:1000, 1000, replace = TRUE)

  x0 <- base_vals + sample(c(-0.2, -0.1, -0.02, 0.02, 0.1, 0.2), 1000, TRUE)
  testthat::expect_equal(round_half_up(x0, 0), round(x0, 0))

  x1 <- base_vals / 10 + sample(c(-0.02, -0.01, 0.01, 0.02), 1000, TRUE)
  testthat::expect_equal(round_half_up(x1, 1), round(x1, 1))

  x2 <- base_vals / 100 + sample(c(-0.002, -0.001, 0.001, 0.002), 1000, TRUE)
  testthat::expect_equal(round_half_up(x2, 2), round(x2, 2))

  # negative digits: avoid ties at 5*10^k by adding +/- (1,2,3,4) etc.
  xneg1 <- base_vals * 10 + sample(c(-4, -3, -2, -1, 1, 2, 3, 4), 1000, TRUE)
  testthat::expect_equal(round_half_up(xneg1, -1), round(xneg1, -1))

  xneg2 <- base_vals * 100 + sample(c(-40, -30, -20, -10, 10, 20, 30, 40), 1000, TRUE)
  testthat::expect_equal(round_half_up(xneg2, -2), round(xneg2, -2))
})

testthat::test_that("round_half_up differs from base::round on .5 ties (sanity)", {
  # These are the canonical banker-rounding examples.
  testthat::expect_equal(round(c(0.5, 1.5, 2.5)), c(0, 2, 2))
  testthat::expect_equal(round_half_up(c(0.5, 1.5, 2.5)), c(1, 2, 3))
})


test_that("Calculation of sumInsured from premium", {
    library(MortalityTables)
    mortalityTables.load("Austria_Census")

    Tarif.EndowmentSI = InsuranceTarif$new(
        name = "Example Tariff - Endowment",
        type = "endowment",
        tarif = "E1-RP",
        desc = "An endowment with regular premiums (standard tariff)",
        age = 40, policyPeriod = 20,

        mortalityTable = mort.AT.census.2011.unisex,
        cost = initializeCosts(alpha = 0.04, gamma.contract = 0.0005, unitcosts = 10),
        i = 0.03,
        sumInsured = 10000,
        contractClosing = as.Date("2020-09-01")
    )
    Tarif.EndowmentSI.rounded = Tarif.EndowmentSI$createModification(
        Rounding = list("Premium gross" = 0, "Premium net" = 2)
    )
    Contract.sumInsured = InsuranceContract$new(
        tarif = Tarif.EndowmentSI
    )
    Contract.sumInsured.rounded = InsuranceContract$new(
        tarif = Tarif.EndowmentSI.rounded
    )

    expect_equal(Contract.sumInsured.rounded$Values$premiums[["gross"]], round(Contract.sumInsured$Values$premiums[["gross"]], 0))
    expect_equal(Contract.sumInsured.rounded$Values$premiums[["net"]], round(Contract.sumInsured$Values$premiums[["net"]], 2))

})
