# LifeInsuranceContracts::vmGlgExample.generateTest(
#     XXXCOMPANYXXX.Tarif2,
#     age=35, policyPeriod = 85, sumInsured=100000,
#     contractClosing = as.Date("2000-07-01"),
#     t = 40)


test_that("Tarif2", {
    contract = InsuranceContract$new(
        XXXCOMPANYXXX.Tarif2,
        age = 35,
        policyPeriod = 85,
        sumInsured = 1e+05,
        contractClosing = as.Date("2000-07-01")
    );
    # showVmGlgExamples(contract, t = 40, prf = 10, t_prf = 12);

    testVmGlgExample(
        contract,
        t = 40, prf = 10, t_prf = 12,
        net = 1210.13,
        Zillmer = 1210.13,
        gross = 1930.16,
        written = 2007.37,
        savings = -137.47,
        risk = 1347.60,
        ZillmerRes = 63074.49,
        ZillmerRes.prf = 10471.41,
        VwKostenRes = -0.00,
        VwKostenRes.prf = 2659.34,
        Bilanzreserve = 63949.81,
        Praemienuebertrag = 965.08,
        Rueckkaufsreserve = 63074.49,
        Rueckkaufswert = 63074.49,
        Abschlusskostenruecktrag = 0.00,
        Rueckkaufswert.prf = 13130.75,
        VS.prf = 80461.07,
        absTolerance = 0.01
    );
})
