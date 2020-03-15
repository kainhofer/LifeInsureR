#' @include HelperFunctions.R
NULL


#' Initialize a cost matrix with dimensions: [CostType, Basis, Period], with:
#'     CostType: alpha, Zillmer, beta, gamma, gamma_nopremiums, unitcosts
#'     Basis:    SumInsured, SumPremiums, GrossPremium, NetPremium, Constant
#'     Period:   once, PremiumPeriod, PremiumFree, PolicyPeriod
#' TODO: gamma an Erlebensleistungen?
#' @export
initializeCosts = function(costs, alpha, Zillmer, beta, gamma, gamma.paidUp, gamma.premiumfree, unitcosts, unitcosts.PolicyPeriod) {
    if (missing(costs)) {
        dimnm = list(
            type = c("alpha", "Zillmer", "beta", "gamma", "gamma_nopremiums", "unitcosts"),
            basis = c("SumInsured", "SumPremiums", "GrossPremium", "NetPremium", "Constant", "Reserve"),
            frequency = c("once", "PremiumPeriod", "PremiumFree", "PolicyPeriod")
        );
        costs = array(
            0,
            dim = sapply(dimnm, length),
            dimnames = dimnm
        );
    }
    if (!missing(alpha)) {
        costs[["alpha",  "SumPremiums", "once"]] = alpha;
    }
    if (!missing(Zillmer)) {
        costs[["Zillmer","SumPremiums", "once"]] = Zillmer;
    }
    if (!missing(beta))  {
        costs[["beta", "GrossPremium", "PremiumPeriod"]] = beta;
    }
    if (!missing(gamma)) {
        costs[["gamma", "SumInsured", "PremiumPeriod"]] = gamma;
    }
    if (!missing(gamma.premiumfree)) {
        costs[["gamma", "SumInsured", "PremiumFree"]] = gamma.premiumfree;
    }
    if (!missing(gamma.paidUp))  {
        costs[["gamma_nopremiums", "SumInsured", "PolicyPeriod"]] = gamma.paidUp;
    }
    if (!missing(unitcosts)) {
        costs[["unitcosts", "Constant", "PremiumPeriod"]] = unitcosts;
    }
    if (!missing(unitcosts.PolicyPeriod)) {
        costs[["unitcosts", "Constant", "PolicyPeriod"]] = unitcosts.PolicyPeriod;
    }
    costs
}
# costs[["beta", "GrossPremium", "once"]] = 0.12; // Bei EINMALERLAG!
# costs[["gamma", "SumInsured", "PolicyPeriod"]] = 0.0005;
# costs[["alpha", "NetPremium", "once"]]


#' Data structure (filled only with NULL) for insurance contract class member values.
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

    premiumComposition = NULL,

    profitParticipation = list()
);

# InsuranceContract.ParameterDefault #######################################
#' Default parameters for the InsuranceContract class. A new contract will be
#' pre-filled with these values, and values passed in the constructor (or with
#' other setter functions) will override these values.
#' @export
InsuranceContract.ParameterDefaults = list(
    ContractData = list(
        id = "ContractID",
        sumInsured = 100000,
        YOB = NULL,
        age = NULL,
        technicalAge = NULL,
        ageDifferences = NULL,                  # Age differences of all other insured relative to the first one, for joint live insurances
        sex = "unisex",                         # Sex, to allow gender-sepecific pricing (e.g. different mortalities or age modification)
        policyPeriod = 25,                      # gesamte Vertragslaufzeit
        premiumPeriod = NULL,                   # Default: policyPeriod, unless explicitly overridden
        deferralPeriod = 0,                     # Aufschubzeit bei Leibrenten
        guaranteedPeriod = 0,                   # Garantiezeit bei Leibrenten
        contractClosing = NULL,                 # Contract closing date (day/month is relevant for balance sheet reserves)
        blockStart = 0,                         # When the current tariff block starts (main block starts a 0, dynamic increases start later!), only used by the parent block (i.e. t=0 of child is aligned with t=blockStart of parent)

        premiumPayments = "in advance",         # Prämienzahlungsweise (vor-/nachschüssig)
        benefitPayments = "in advance",         # Leistungszahlungsweise (vor-/nachschüssig)

        premiumFrequency = 1,                   # Anzahl der Prämienzahlungen pro Jahr
        benefitFrequency = 1,                   # Anzahl der Leistungszahlungen pro Jahr (bei Renten, bzw. bei ALV Leistung am Ende des k-ten Teil des Jahres)

        widowProportion = 0,                    # Witwenübergang (Anteil an VS des VN)
        deathBenefitProportion = 1,             # For endowments: Proportion of the death benefit relative to the life benefit
        premiumRefund = 0,                      # Proportion of premiums refunded on death (including additional risk, e.g. 1.10 = 110% of paid premiums)
        premiumIncrease = 1,                    # The yearly growth factor of the premium, i.e. 1.05 means +5% increase each year; a Vector describes the premiums for all years
        annuityIncrease = 1,                    # The yearly growth factor of the annuity payments, i.e. 1.05 means +5% incrase each year; a vector describes the annuity unit payments for all years
        deathBenefit = 1                        # The yearly relative death benefit (relative to the initial sum insured); Can be set to a function(len, params, values), e.g. deathBenefit = deathBenefit.linearDecreasing
    ),
    ContractState = list(
        premiumWaiver = FALSE,                  # Vertrag ist prämienfrei gestellt
        surrenderPenalty = TRUE,                # Set to FALSE after the surrender penalty has been applied once, e.g. on a premium waiver
        alphaRefunded = FALSE                   # Alpha costs not yet refunded (in case of contract changes)
    ),
    ActuarialBases = list(
        mortalityTable = NULL,
        invalidityTable = NULL,
        invalidityEndsContract = TRUE,          # Whether a claim for disease ends the contract or not
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
        unitcosts = 0,                          # Annual unit cost for each policy (Stückkosten), absolute value (can be a function)
        security = 0,                           # Additional security loading on all benefit payments, factor on all benefits
        noMedicalExam = 0,                      # Loading when no medicial exam is done, % of SumInsured
        noMedicalExamRelative = 0,              # Loading when no medicial exam is done, % of gross premium
        sumRebate = 0,                          # gross premium reduction for large premiums, % of SumInsured
        extraRebate = 0,                        # gross premium reduction for any reason, % of SumInsured
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

        waitingPeriod = NULL,

        guaranteedInterest = NULL,                # Individual contract-specific overrides (i.e. not keyed by year)
        interestProfitRate = NULL,
        totalInterest = NULL,
        mortalityProfitRate = NULL,
        expenseProfitRate = NULL,
        sumProfitRate = NULL,
        terminalBonusRate = NULL,
        terminalBonusFundRate = NULL,

        profitParticipationScheme = NULL,      # Gewinnbeteiligungssystem (object of class Profit Participation)
        profitComponents = c("interest", "risk", "expense", "sum", "terminal"),
        profitClass = NULL,
        profitRates = NULL                     # General, company-wide profit rates, key columns are year and profitClass
    ),

    Hooks = list(
      # Functions with signature function(x, params, values, ...), default NULL is equivalent to function(x, ...) {x}
      adjustCashFlows = NULL,
      adjustCashFlowsCosts = NULL
    )
);


#' Full insurance contract parameter structure.
#'
#' All values are filled with NULL,
#' so the functions \code{\link{InsuranceContract.ParametersFill}} and
#' \code{\link{InsuranceContract.ParametersFallback}} can be used to override
#' existing parameters or to provide default values for unset (NULL) entries.
#'
#' @export
InsuranceContract.ParameterStructure = rapply(InsuranceContract.ParameterDefaults, function(x) NULL, how = "replace")
InsuranceContract.ParameterStructure$Loadings["benefitFrequencyLoading"] = list(NULL)
InsuranceContract.ParameterStructure$Loadings["premiumFrequencyLoading"] = list(NULL)


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
    params$Hooks = fillFields(params$Hooks, list(...))

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
InsuranceContract.ParametersFallback = function(params, fallback, ppParameters = TRUE) {
    # params = InsuranceContract.ParameterStructure;
    params$ContractData = fallbackFields(params$ContractData, fallback$ContractData);
    params$ContractState = fallbackFields(params$ContractState, fallback$ContractState);
    params$ActuarialBases = fallbackFields(params$ActuarialBases, fallback$ActuarialBases);
    params$Loadings = fallbackFields(params$Loadings, fallback$Loadings);
    params$Features = fallbackFields(params$Features, fallback$Features);
    if (ppParameters) {
        params$ProfitParticipation = fallbackFields(params$ProfitParticipation, fallback$ProfitParticipation);
    }
    params$Hooks = fallbackFields(params$Hooks, fallback$Hooks);

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


