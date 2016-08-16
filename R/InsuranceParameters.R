# Initialize a cost matrix with dimensions: [CostType, Basis, Period], with:
#     CostType: alpha, Zillmer, beta, gamma, gamma_nopremiums
#     Basis:    SumInsured, SumPremiums, GrossPremium
#     Period:   once, PremiumPeriod, PremiumFree, PolicyPeriod
# TODO: gamma an Erlebensleistungen?
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



InsuranceContract.ParameterStructure = list(
  ContractData = list (
    sumInsured = NA,
    YOB = NA,
    age = NA,
    policyPeriod = NA,               # gesamte Vertragslaufzeit
    premiumPeriod = NA,                # Prämienzahlungsdauer (1 für Einmalprämie)
    deferralPeriod = NA,               # Aufschubzeit bei Leibrenten
    guaranteedPeriod = NA,             # Garantiezeit bei Leibrenten
    contractClosing = NA,     # Contract closing date (day/month is relevant for balance sheet reserves)

    premiumPayments = NA, # Prämienzahlungsweise (vor-/nachschüssig)
    benefitPayments = NA, # Leistungszahlungsweise (vor-/nachschüssig)

    premiumFrequency = NA,             # Anzahl der Prämienzahlungen pro Jahr
    benefitFrequency = NA,             # Anzahl der Leistungszahlungen pro Jahr (bei Renten, bzw. bei ALV Leistung am Ende des k-ten Teil des Jahres)

    widowProportion = NA,              # Witwenübergang (Anteil an VS des VN)
    deathBenefitProportion = NA,       # For endowments: Proportion of the death benefit relative to the life benefit
    premiumRefund = NA                 # Proportion of premiums refunded on death (including additional risk, e.g. 1.10 = 110% of paid premiums)
  ),
  ContractState = list(
    premiumWaiver = NA,        # Vertrag ist prämienfrei gestellt
    surrenderPenalty = NA,     # Set to FALSE after the surrender penalty has been applied once, e.g. on a premium waiver
    alphaRefunded = NA         # Alpha costs not yet refunded (in case of contract changes)
  ),
  ActuarialBases = list(
    mortalityTable = NA,
    invalidityTable = NA,
    i = NA,                             # guaranteed interest rate
    v = NA,                             # discount factor
    balanceSheetDate = NA,              # Balance sheet date (for the calculation of the balance sheet reserves)
    balanceSheetMethod = NA,
    surrenderValueCalculation = NA,     # By default no surrender penalties

    premiumFrequencyOrder = NA,         # Order of the approximation for payments within the year (unless an extra frequency loading is used => then leave this at 0)
    benefitFrequencyOrder = NA
  ),
  Costs = list(),
  Loadings = list( # Loadings can also be function(sumInsured, premiums)    # TODO: Add other possible arguments
    ongoingAlphaGrossPremium = NA,    # Acquisition cost that increase the gross premium
    tax = NA,                         # insurance tax, factor on each premium paid
    unitcosts = NA,                   # annual unit cost for each policy (Stückkosten), absolute value
    security = NA,                    # Additional security loading on all benefit payments, factor on all benefits
    noMedicalExam = NA,               # Loading when no medicial exam is done, % of SumInsured
    noMedicalExamRelative = NA,       # Loading when no medicial exam is done, % of gross premium
    sumRebate = NA,                   # gross premium reduction for large premiums, % of SumInsured
    premiumRebate = NA,               # gross premium reduction for large premiums, % of gross premium # TODO
    advanceProfitParticipation = NA,                # Vorweggewinnbeteiligung (%-Satz der Bruttoprämie)
    advanceProfitParticipationInclUnitCost = NA,    # Vorweggewinnbeteiligung (%-Satz der Prämie mit Zu-/Abschlägen, insbesondere nach Stückkosten)
    partnerRebate = NA,                # Partnerrabatt auf Prämie mit Zu-/Abschlägen, wenn mehr als 1 Vertrag gleichzeitig abgeschlossen wird, additiv mit advanceBonusInclUnitCost and premiumRebate
    benefitFrequencyLoading = NA, # TODO: Properly implement this as a function
    premiumFrequencyLoading = NA  # TODO: Implement this
  ),
  Features = list(                   # Special cases for the calculations
    betaGammaInZillmer = NA,      # Whether beta and gamma-costs should be included in the Zillmer premium calculation
    alphaRefundLinear  = NA        # Whether the refund of alpha-costs on surrender is linear in t or follows the NPV of an annuity
  )
);

#' @function InsuranceContract.ParametersFill
#' @description Initialize the insurance contract parameters from the passed
#' arguments. Arguments not given are left unchanged. If no existing parameter structure is given, an empty (i.e. all NA entries) structure is used.
InsuranceContract.ParametersFill = function(params=InsuranceContract.ParameterStructure, ...) {
  # params = InsuranceContract.ParameterStructure;
  params$ContractData = fillFields(params$ContractData, list(...));
  params$ContractState = fillFields(params$ContractState, list(...));
  params$ActuarialBases = fillFields(params$ActuarialBases, list(...));
  params$Loadings = fillFields(params$Loadings, list(...));
  params$Features = fillFields(params$Features, list(...));

  # Costs are a special case, because they are an array rather than a list:
  # params$Costs = match.arg(Costs) # TODO!!!!
  params
}

InsuranceContract.ParametersFallback = function(params, fallback) {
  # params = InsuranceContract.ParameterStructure;
  params$ContractData = fallbackFields(params$ContractData, fallback$ContractData);
  params$ContractState = fallbackFields(params$ContractState, fallback$ContractState);
  params$ActuarialBases = fallbackFields(params$ActuarialBases, fallback$ActuarialBases);
  params$Loadings = fallbackFields(params$Loadings, fallback$Loadings);
  params$Features = fallbackFields(params$Features, fallback$Features);

  # Costs are a special case, because they are an array rather than a list:
  # params$Costs = match.arg(Costs) # TODO!!!! # FIXME
  params
}
