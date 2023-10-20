#' @include XXXCOMPANYXXX_General.R


##############################################################################m##
#     Tarif2
##############################################################################m##

# XXXCOMPANYXXX Versicherung, Tarif2
# Whole life insurance

XXXCOMPANYXXX.costs2 = initializeCosts(
    alpha = 0.035,
    gamma = 0.005,
    gamma.paidUp = 0.005
)

#' @export
XXXCOMPANYXXX.Tarif2 = InsuranceTarif$new(
    name = "Tarif2",
    type = "wholelife",
    tarif = "Death Comfort",
    desc = "Whole life insurance with single premium, guaranteed interest 3%",

    mortalityTable = XXXCOMPANYXXX.Sterbetafel2001.unisex,
    i = 0.03,
    tax = 0.04,
    costs = XXXCOMPANYXXX.costs2,
    premiumFrequencyLoading = freqCharge(3, 2, 1, 0),

    sumRebate = function(params, values) {
        SI = params$ContractData$sumInsured
        if(SI >= 1000000) {
            0.002
        } else if (500000 <= SI) {
            0.001
        } else if (200000 <= SI) {
            0.0005
        } else {
            0
        }
    },
    surrenderValueCalculation = XXXCOMPANYXXX.surrender.increasing90
) %>%
    XXXCOMPANYXXX.register("Tarif2", GV = "2");


