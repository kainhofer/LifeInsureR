#' @include HelperFunctions.R
#' @import lubridate
NULL


#' Initialize a cost matrix with dimensions: [CostType, Basis, Period], with:
#'     CostType: alpha, Zillmer, beta, gamma, gamma_nopremiums
#'     Basis:    SumInsured, SumPremiums, GrossPremium
#'     Period:   once, PremiumPeriod, PremiumFree, PolicyPeriod
#' TODO: gamma an Erlebensleistungen?
#' @export
initializeCosts = function() {
  dimnm=list(
    c("alpha", "Zillmer", "beta", "gamma", "gamma_nopremiums"),
    c("SumInsured", "SumPremiums", "GrossPremium"),
    c("once", "PremiumPeriod", "PremiumFree", "PolicyPeriod")
  );
  array(0,
        dim=sapply(dimnm, length),
        dimnames=dimnm
  )
}


#' @export
InsuranceContract.Values = list(
  basicData = NULL,
  transitionProbabilities = NULL,

  cashFlowsBasic = NULL,
  cashFlows = NULL,
  cashFlowsCosts = NULL,
  unitPremiumSum = 0,

  presentValues = NULL,
  presentValuesCosts = NULL,

  premiumCoefficients = NULL,
  premiums = NULL,
  absCashFlows = NULL,
  absPresentValues = NULL,

  reserves = NULL,
  reservesBalanceSheet = NULL,

  premiumComposition = NULL
);



#' @export
InsuranceContract.ParameterDefaults = list(
  ContractData = list (
    sumInsured = 100000,
    YOB = 1975,
    age = 40,
    policyPeriod = 25,                      # gesamte Vertragslaufzeit
    premiumPeriod = 25,                     # Prämienzahlungsdauer (1 für Einmalprämie)
    deferralPeriod = 0,                     # Aufschubzeit bei Leibrenten
    guaranteedPeriod = 0,                   # Garantiezeit bei Leibrenten
    contractClosing = Sys.Date(),           # Contract closing date (day/month is relevant for balance sheet reserves)

    premiumPayments = "in advance",         # Prämienzahlungsweise (vor-/nachschüssig)
    benefitPayments = "in advance",         # Leistungszahlungsweise (vor-/nachschüssig)

    premiumFrequency = 1,                   # Anzahl der Prämienzahlungen pro Jahr
    benefitFrequency = 1,                   # Anzahl der Leistungszahlungen pro Jahr (bei Renten, bzw. bei ALV Leistung am Ende des k-ten Teil des Jahres)

    widowProportion = 0,                    # Witwenübergang (Anteil an VS des VN)
    deathBenefitProportion = 1,             # For endowments: Proportion of the death benefit relative to the life benefit
    premiumRefund = 0                       # Proportion of premiums refunded on death (including additional risk, e.g. 1.10 = 110% of paid premiums)
  ),
  ContractState = list(
    premiumWaiver = FALSE,                  # Vertrag ist prämienfrei gestellt
    surrenderPenalty = TRUE,                # Set to FALSE after the surrender penalty has been applied once, e.g. on a premium waiver
    alphaRefunded = FALSE                   # Alpha costs not yet refunded (in case of contract changes)
  ),
  ActuarialBases = list(
    mortalityTable = NULL,
    invalidityTable = NULL,
    i = 0.00,                               # guaranteed interest rate
    balanceSheetDate = as.Date("1900-12-31"),  # Balance sheet date (for the calculation of the balance sheet reserves, year is irrelevant)
    balanceSheetMethod = "30/360",
    surrenderValueCalculation = NULL,       # By default no surrender penalties

    premiumFrequencyOrder = 0,              # Order of the approximation for payments within the year (unless an extra frequency loading is used => then leave this at 0)
    benefitFrequencyOrder = 0
  ),
  Costs = initializeCosts(),
  Loadings = list( # Loadings can also be function(sumInsured, premiums)
    ongoingAlphaGrossPremium = 0,           # Acquisition cost that increase the gross premium
    tax = 0.04,                             # insurance tax, factor on each premium paid
    unitcosts = 0,                          # annual unit cost for each policy (Stückkosten), absolute value
    security = 0,                           # Additional security loading on all benefit payments, factor on all benefits
    noMedicalExam = 0,                      # Loading when no medicial exam is done, % of SumInsured
    noMedicalExamRelative = 0,              # Loading when no medicial exam is done, % of gross premium
    sumRebate = 0,                          # gross premium reduction for large premiums, % of SumInsured
    premiumRebate = 0,                      # gross premium reduction for large premiums, % of gross premium # TODO
    partnerRebate = 0,                      # Partnerrabatt auf Prämie mit Zu-/Abschlägen, wenn mehr als 1 Vertrag gleichzeitig abgeschlossen wird, additiv mit advanceBonusInclUnitCost and premiumRebate
    extraChargeGrossPremium = 0,            # extra charges on gross premium (smoker, leisure activities, BMI too high, etc.)
    benefitFrequencyLoading = list("1" = 0.0, "2" = 0.0, "4" = 0.0, "12" = 0.0), # TODO: Properly implement this as a function
    premiumFrequencyLoading = list("1" = 0.0, "2" = 0.0, "4" = 0.0, "12" = 0.0) # TODO: Properly implement this as a function
  ),
  Features = list(                          # Special cases for the calculations
    betaGammaInZillmer = FALSE,             # Whether beta and gamma-costs should be included in the Zillmer premium calculation
    alphaRefundLinear  = TRUE               # Whether the refund of alpha-costs on surrender is linear in t or follows the NPV of an annuity
  ),

  ProfitParticipation = list(
      advanceProfitParticipation = 0,                # Vorweggewinnbeteiligung (%-Satz der Bruttoprämie)
      advanceProfitParticipationInclUnitCost = 0,    # Vorweggewinnbeteiligung (%-Satz der Prämie mit Zu-/Abschlägen, insbesondere nach Stückkosten)

      guaranteedInterest = 0,
      interestBonusRate = 0,
      totalInterest = 0,
      mortalityBonusRate = 0,
      costBonusRate = 0,
      terminalBonusRate = 0,
      terminalBonusQuote = 0,

      profitParticipationScheme = NULL      # Gewinnbeteiligungssystem (object of class Profit Participation)
  )
);


#' Full insurance contract parameter structure.
#'
#' All values are filled with NULL,
#' so the functions \code{\link{InsuranceContract.ParametersFill}} and
#' \code{\link{InsuranceContract.ParametersFallback}} can be used to override
#' existing parameters or to provide default values for unset (NULL) entries.
#'
#'  @export
InsuranceContract.ParameterStructure = rapply(InsuranceContract.ParameterDefaults, function (x) NULL, how="replace")
InsuranceContract.ParameterStructure$Loadings$benefitFrequencyLoading = NULL
InsuranceContract.ParameterStructure$Loadings$premiumFrequencyLoading = NULL


#' InsuranceContract.ParametersFill
#'
#' Initialize the insurance contract parameters from the passed
#' arguments. Arguments not given are left unchanged. If no existing parameter
#' structure is given, an empty (i.e. all NULL entries) structure is used.
#'
#' @param params Initial values of the insurance contract parameters. (default: empty parameter structure)
#' @param costs,... Values for any of the entries in the insurance contract
#'                  parameter structure. These values take precedence over the
#'                  initial parameters provided in \code{params}.
#'
#' @export
InsuranceContract.ParametersFill = function(params=InsuranceContract.ParameterStructure, costs=NULL, ...) {
    # params = InsuranceContract.ParameterStructure;
    params$ContractData = fillFields(params$ContractData, list(...));
    params$ContractState = fillFields(params$ContractState, list(...));
    params$ActuarialBases = fillFields(params$ActuarialBases, list(...));
    params$Loadings = fillFields(params$Loadings, list(...));
    params$Features = fillFields(params$Features, list(...));
    params$ProfitParticipation = fillFields(params$ProfitParticipation, list(...));

    # Costs are a special case, because they are an array rather than a list:
    # TODO: Find a way to partially override
    if (!missing(costs)) params$Costs = costs;
    params
}

#' InsuranceContract.ParametersFallback
#'
#' Provide default values for the insurance contract parameters if any of the
#' parameters is not explicitly set.
#'
#' @param params Current, explicitly set contract parameters. All NULL values
#'               will be filled with the corresponding entry from \code{fallback}.
#' @param fallback Fallback values that will be used when the corresponding
#'                 entry in \code{params} is NULL.
#'
#' @export
InsuranceContract.ParametersFallback = function(params, fallback) {
    # params = InsuranceContract.ParameterStructure;
    params$ContractData = fallbackFields(params$ContractData, fallback$ContractData);
    params$ContractState = fallbackFields(params$ContractState, fallback$ContractState);
    params$ActuarialBases = fallbackFields(params$ActuarialBases, fallback$ActuarialBases);
    params$Loadings = fallbackFields(params$Loadings, fallback$Loadings);
    params$Features = fallbackFields(params$Features, fallback$Features);
    params$ProfitParticipation = fallbackFields(params$ProfitParticipation, fallback$ProfitParticipation);

    # Costs are a special case, because they are an array rather than a list:
    # TODO: Find a way to partially fall back
    if (is.null(params$Costs)) {
        # Fallback can either be a full
        if (!is.null(fallback$costs)) {
            params$Costs = fallback$costs;
        } else {
            params$Costs = fallback$Costs;
        }
    }
    params
}


