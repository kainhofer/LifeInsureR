#' @include XXXCOMPANYXXX_General.R


##############################################################################m##
#     Tarif1
##############################################################################m##

# XXXCOMPANYXXX Versicherung, Tarif 1
# Typ: Endowment with level death and survival benefits, level premiums
# Guaranteed interest: i=3%, Mortality rates: XXXCOMPANYXXX.Sterbetafel2001.unisex

XXXCOMPANYXXX.costs = initializeCosts(
    alpha = 0.04, Zillmer = 0.025,
    beta = 0.02,
    gamma = 0.002,
    gamma.paidUp = 0.005
)

#' @export
XXXCOMPANYXXX.Tarif1 = InsuranceTarif$new(
    name = "Tarif1",
    type = "endowment",
    tarif = "Endowment Comfort",
    desc = "Endowment with level death and survival benefits, level premiums, guaranteed interest 3%",

    mortalityTable = XXXCOMPANYXXX.Sterbetafel2001.unisex,
    i = 0.03,
    tax = 0.04,
    costs = XXXCOMPANYXXX.costs,
    premiumFrequencyLoading = freqCharge(3, 2, 1, 0),

    surrenderValueCalculation = XXXCOMPANYXXX.surrender.increasing90,
    profitParticipationScheme = XXXCOMPANYXXX.Gewinnplan1
) %>%
    XXXCOMPANYXXX.register("Tarif1", GV = "1");


