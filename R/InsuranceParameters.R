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
    sumInsured = NULL,
    YOB = NULL,
    age = NULL,
    policyPeriod = NULL,               # gesamte Vertragslaufzeit
    premiumPeriod = NULL,                # Prämienzahlungsdauer (1 für Einmalprämie)
    deferralPeriod = NULL,               # Aufschubzeit bei Leibrenten
    guaranteedPeriod = NULL,             # Garantiezeit bei Leibrenten
    contractClosing = NULL,     # Contract closing date (day/month is relevant for balance sheet reserves)

    premiumPayments = NULL, # Prämienzahlungsweise (vor-/nachschüssig)
    benefitPayments = NULL, # Leistungszahlungsweise (vor-/nachschüssig)

    premiumFrequency = NULL,             # Anzahl der Prämienzahlungen pro Jahr
    benefitFrequency = NULL,             # Anzahl der Leistungszahlungen pro Jahr (bei Renten, bzw. bei ALV Leistung am Ende des k-ten Teil des Jahres)

    widowProportion = NULL,              # Witwenübergang (Anteil an VS des VN)
    deathBenefitProportion = NULL,       # For endowments: Proportion of the death benefit relative to the life benefit
    premiumRefund = NULL                 # Proportion of premiums refunded on death (including additional risk, e.g. 1.10 = 110% of paid premiums)
  ),
  ContractState = list(
    premiumWaiver = NULL,        # Vertrag ist prämienfrei gestellt
    surrenderPenalty = NULL,     # Set to FALSE after the surrender penalty has been applied once, e.g. on a premium waiver
    alphaRefunded = NULL         # Alpha costs not yet refunded (in case of contract changes)
  ),
  ActuarialBases = list(
    mortalityTable = NULL,
    invalidityTable = NULL,
    i = NULL,                             # guaranteed interest rate
    balanceSheetDate = NULL,              # Balance sheet date (for the calculation of the balance sheet reserves)
    balanceSheetMethod = NULL,
    surrenderValueCalculation = NULL,     # By default no surrender penalties

    premiumFrequencyOrder = NULL,         # Order of the approximation for payments within the year (unless an extra frequency loading is used => then leave this at 0)
    benefitFrequencyOrder = NULL
  ),
  Costs = NULL,
  Loadings = list( # Loadings can also be function(sumInsured, premiums)    # TODO: Add other possible arguments
    ongoingAlphaGrossPremium = NULL,    # Acquisition cost that increase the gross premium
    tax = NULL,                         # insurance tax, factor on each premium paid
    unitcosts = NULL,                   # annual unit cost for each policy (Stückkosten), absolute value
    security = NULL,                    # Additional security loading on all benefit payments, factor on all benefits
    noMedicalExam = NULL,               # Loading when no medicial exam is done, % of SumInsured
    noMedicalExamRelative = NULL,       # Loading when no medicial exam is done, % of gross premium
    sumRebate = NULL,                   # gross premium reduction for large premiums, % of SumInsured
    premiumRebate = NULL,               # gross premium reduction for large premiums, % of gross premium # TODO
    partnerRebate = NULL,                # Partnerrabatt auf Prämie mit Zu-/Abschlägen, wenn mehr als 1 Vertrag gleichzeitig abgeschlossen wird, additiv mit advanceBonusInclUnitCost and premiumRebate
    extraChargeGrossPremium = NULL,     # extra charges on gross premium (smoker, leisure activities, BMI too high, etc.)
    benefitFrequencyLoading = NULL, # TODO: Properly implement this as a function
    premiumFrequencyLoading = NULL  # TODO: Implement this
  ),
  Features = list(                   # Special cases for the calculations
    betaGammaInZillmer = NULL,      # Whether beta and gamma-costs should be included in the Zillmer premium calculation
    alphaRefundLinear  = NULL        # Whether the refund of alpha-costs on surrender is linear in t or follows the NPV of an annuity
  ),

  ProfitParticipation = list(
      advanceProfitParticipation = NULL,                # Vorweggewinnbeteiligung (%-Satz der Bruttoprämie)
      advanceProfitParticipationInclUnitCost = NULL,    # Vorweggewinnbeteiligung (%-Satz der Prämie mit Zu-/Abschlägen, insbesondere nach Stückkosten)

      guaranteedInterest = NULL,
      interestBonusRate = NULL,
      totalInterest = NULL,
      mortalityBonusRate = NULL,
      costBonusRate = NULL,
      terminalBonusRate = NULL,
      terminalBonusQuote = NULL,

      profitParticipationScheme = NULL                  # Gewinnbeteiligungssystem (object of class Profit Participation)
  )
);

# InsuranceContract.ParametersFill
# Initialize the insurance contract parameters from the passed
# arguments. Arguments not given are left unchanged. If no existing parameter
# structure is given, an empty (i.e. all NULL entries) structure is used.
#
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


