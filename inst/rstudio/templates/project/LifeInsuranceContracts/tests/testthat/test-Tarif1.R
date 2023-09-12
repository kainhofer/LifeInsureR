# LifeInsuranceContracts::vmGlgExample.generateTest(
#     XXXCOMPANYXXX.Tarif1,
#     age=35, policyPeriod=30, sumInsured=100000,
#     contractClosing = as.Date("2000-07-01"))


test_that("Tarif1", {
    contract = InsuranceContract$new(
        XXXCOMPANYXXX.Tarif1,
        age = 35,
        policyPeriod = 30,
        sumInsured = 100000,
        contractClosing = as.Date("2000-07-01")
    );
    exportInsuranceContract.xlsx(contract, here("test-Tarif1.xlsx"))
    openxlsx::openXL(here("test-Tarif1.xlsx"))
    # showVmGlgExamples(contract, t = 10, prf = 10, t_prf = 12);

    testVmGlgExample(
        contract,
        t = 10, prf = 10, t_prf = 12,
        net = 2208.00,
        Zillmer = 2308.67,
        gross = 2621.51,
        written = 2726.37,
        savings = 2126.58,
        risk = 182.10,
        ZillmerRes = 23118.57,
        ZillmerRes.prf = 19931.70,
        VwKostenRes = -0.00,
        VwKostenRes.prf = 2245.90,
        Bilanzreserve = 24560.53,
        Praemienuebertrag = 1310.76,
        Rueckkaufsreserve = 23118.57,
        Rueckkaufswert = 21286.21,
        Abschlusskostenruecktrag = 0.00,
        Rueckkaufswert.prf = 22177.59,
        VS.prf = 33014.59,
        absTolerance = 0.01
    );
})
