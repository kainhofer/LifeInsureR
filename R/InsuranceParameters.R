#' @include HelperFunctions.R
NULL

#' Initialize a data structure for the definition of [InsuranceTarif] costs
#'
#' Initialize a cost matrix with dimensions: {CostType, Basis, Period}, where:
#' \describe{
#'     \item{CostType:}{alpha, Zillmer, beta, gamma, gamma_nopremiums, unitcosts}
#'     \item{Basis:}{SumInsured, SumPremiums, GrossPremium, NetPremium, Constant}
#'     \item{Period:}{once, PremiumPeriod, PremiumFree, PolicyPeriod}
#' }
#' This cost structure can then be modified for non-standard costs.
#' The main purpose of this structure is to be passed to [InsuranceContract] or
#' [InsuranceTarif] definitions.
# TODO: gamma an Erlebensleistungen?
#'
#' @param costs (optional) existing cost structure to duplicate / use as a starting point
#' @param alpha Alpha costs (charged once, relative to sum of premiums)
#' @param Zillmer Zillmer costs (charged once, relative to sum of premiums)
#' @param beta Collection costs (charged on each gross premium, relative to gross premium)
#' @param gamma Administration costs while premiums are paid (relative to sum insured)
#' @param gamma.paidUp Administration costs for paid-up contracts (relative to sum insured)
#' @param gamma.premiumfree Administration costs for planned premium-free period (reltaive to sum insured)
#' @param gamma.contract Administration costs for the whole contract period (relative to sum insured)
#' @param unitcosts Unit costs (absolute monetary amount, during premium period)
#' @param unitcosts.PolicyPeriod Unit costs (absolute monetary amount, during full contract period)
#'
#' @examples
#' # empty cost structure (only 0 costs)
#' initializeCosts()
#'
#' # the most common cost types can be given in initializeCosts()
#' initializeCosts(alpha = 0.04, Zillmer = 0.025, beta = 0.05, gamma.contract = 0.001)
#'
#' # The same cost structure manually
#' costs.Bsp = initializeCosts();
#' costs.Bsp[["alpha", "SumPremiums", "once"]] = 0.04;
#' costs.Bsp[["Zillmer", "SumPremiums", "once"]] = 0.025;
#' costs.Bsp[["beta", "GrossPremium", "PremiumPeriod"]] = 0.05;
#' costs.Bsp[["gamma", "SumInsured", "PolicyPeriod"]] = 0.001;
#'
#'
#'
#' @export
initializeCosts = function(costs, alpha, Zillmer, beta, gamma, gamma.paidUp, gamma.premiumfree, gamma.contract, unitcosts, unitcosts.PolicyPeriod) {
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
    if (!missing(gamma.contract))  {
        costs[["gamma", "SumInsured", "PolicyPeriod"]] = gamma.contract;
    }
    if (!missing(unitcosts)) {
        costs[["unitcosts", "Constant", "PremiumPeriod"]] = unitcosts;
    }
    if (!missing(unitcosts.PolicyPeriod)) {
        costs[["unitcosts", "Constant", "PolicyPeriod"]] = unitcosts.PolicyPeriod;
    }
    costs
}


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
#'
#' @format The parameter list is a list of lists with the following structure:
#'
#' Sublists:
#' \itemize{
#'     \item \code{$ContractData} ... Contract-specific data (policy period,
#'               closing, age, sum insured, premium payments, etc.)
#'     \item \code{$ContractState} ... Current contract state (paid-up, surrender
#'               penalty already applied, alpha costs already (partially) refunded)
#'     \item \code{$ActuarialBases} ... Actuarial bases for the contract
#'               calculation (mortality/invalidity table, guaranteed interest,
#'               surrender penalty, etc.)
#'     \item \code{$Costs} ... Expenses charged to the contract (see [initializeCosts()])
#'     \item \code{$Loadings} ... Loadings, rebates and other charges of the
#'               tariff / contract (tax, unit costs, surcharge for no medial exam, premium/benefit frequency loading)
#'     \item \code{$Features} ... Peculiarities of the tariff (to enable
#'               non-standard formulas for certain company-specific historical
#'               "glitches" in the tariff definitions.)
#'     \item \code{$ProfitParticipation} ... Profit scheme and profit participation
#'               rates (default values, can be overwritten per profit scenario)
#'     \item \code{$Hooks} ... Hook functions to allow modification of various
#'               calculation aspects (e.g. modify the default cash flows after
#'               their setup)
#' }
#'
#' ## Elements of sublist \code{InsuranceContract.ParameterDefault$ContractData}
#'
#' These values are typically set per contract and not by the tariff. Notable
#' exceptions are the contract duration in some instances and the premiumPeriod=1
#' for single-premium contracts.
#'
#' \describe{
#'     \item{\code{$id}}{ID of the contract (to distinguish individual parts in
#'               contracts with multiple parts, e.g. dynamic increases),
#'               default = "Hauptvertrag"}
#'     \item{\code{$sumInsured}}{Sum insured, default = 100,000}
#'     \item{\code{$YOB}}{Year of birth of the insured, used to determine the
#'               age for the application of the mortality table}
#'     \item{\code{$age}}{Age of the insured}
#'     \item{\code{$technicalAge}}{Technical age of the insured (when the age
#'               for the application of the mortality table does not coincide
#'               with the real age)}
#'     \item{\code{$ageDifferences}}{Vector of age differences to the first
#'               insured for contracts with multiple insured (i.e. joint-lives)}
#'     \item{\code{$sex}}{Sex of the insured, to allow gender-specific prixing
#'               (e.g. different mortalities or age modification), default="unisex",
#'               Type is [SexEnum]}
#'     \item{\code{$policyPeriod}}{Policy Duration (in years)}
#'     \item{\code{$premiumPeriod}}{Premium payment period (in year), for
#'               single-premium contracts, \code{premiumPeriod = 1}. Default is
#'               \code{policyPeriod}, i.e. regular premiums during the whole
#'               contract period}
#'     \item{\code{$deferralPeriod}}{deferral period for annuities, i.e. the
#'               period survival payments start only after this period, typically
#'               the retirement age.  This applies mostly to tariffs of type
#'               annuity, although deferral periods are possible (but not common)
#'               for all other types of insurance, too.}
#'     \item{\code{$guaranteedPeriod}}{guaranteed annuity payment period. The
#'               annuity pays out for this period, even if the insured dies.
#'               This applies only to tariffs of type annuity.}
#'     \item{\code{$contractClosing}}{The date (variable of type [Date]) when
#'               the coverage of the contract starts (not neccessarily equal to
#'               the date when the contract was signed). Typically generated by
#'               a call to [as.Date()]. The year is relevant to derive the age
#'               of the insured, while month and day are relevant for the
#'               interpolation of the balance sheet reserves}
#'     \item{\code{$blockStart}}{For contracts with multiple blocks (e.g.
#'               multiple dynamic increases, where each increase is modelled
#'               like a separate contract), this variable holds the offset of
#'               the current contract block relative to the main contract block.
#'               The main block starts a 0, dynamic increases start later! This
#'               value is only used by the parent block (i.e. $t=0$ of the child
#'               is aligned with $t=blockStart$ of the parent block.}
#'     \item{\code{$premiumPayments}}{Whether premiums are paid in advance
#'               (default) or arrears. Value is of type [PaymentTimeEnum]
#'               with possible values "in advance" and 'in arrears"}
#'     \item{\code{$benefitPayments}}{Whether recurring benefits (e.g. annuities)
#'               are paid in advance (default) or arrears. Value is of type
#'               [PaymentTimeEnum] with possible values "in advance" and
#'               "in arrears"}
#'     \item{\code{$premiumFrequency}}{Number of premium payments per year, default is 1.}
#'     \item{\code{$benefitFrequency}}{Number of benefit payments per year, default is 1.}
#'     \item{\code{$widowProportion}}{For annuities with a widow transition,
#'               this describes the factor of the widow benefits relative to
#'               the original benefit.}
#'     \item{\code{$deathBenefitProportion}}{For endowments with a death and
#'               survival benefit, this describes the proportion of the death
#'               benefit relative to the survival benefit.}
#'     \item{\code{$premiumRefund}}{Proportion of (gross) premiums refunded on
#'               death (including additional risk, e.g. 1.10 = 110% of paid premiums)}
#'     \item{\code{$premiumIncrease}}{The yearly growth factor of the premium,
#'               i.e. 1.05 means +5% increase each year; a vector describes the
#'               premiums for all years}
#'     \item{\code{$annuityIncrease}}{The yearly growth factor of the annuity
#'               payments, i.e. 1.05 means +5% increase each year; a vector
#'               describes the annuity unit payments for all years}
#'     \item{\code{$deathBenefit}}{The yearly relative death benefit (relative
#'               to the initial sum insured); Can be set to a \code{function(len,
#'               params, values)}, e.g. \code{deathBenefit = deathBenefit.linearDecreasing}}
#' }
#'
#' ## Elements of sublist \code{InsuranceContract.ParameterDefault$ContractState}
#'
#' Contract-specific status variables holding the status of the contract.
#'
#' \describe{
#'     \item{\code{$premiumWaiver}}{Whether the contract is paid-up.}
#'     \item{\code{$surrenderPenalty}}{Whether a surrender penalty still applies
#'          (e.g. because it has already been applied during a contract change,
#'          or because due to legal reasons it can no longer be applied)}
#'     \item{\code{$alphaRefunded}}{Whether alpha costs have (at least partially)
#'          been refunded (e.g. when a contract is changed or paid-up). Default
#'          is not yet refunded.}
#' }
#'
#'
#' ## Elements of sublist \code{InsuranceContract.ParameterDefault$ActuarialBases}
#'
#' Tarif-specific actuarial calculation parameters of the contract. Typically,
#' these values are set by the tariff, but can be overridden by contract (e.g.
#' while prototyping a new product or a product change).
#'
#' \describe{
#'     \item{\code{$mortalityTable}}{The [mortalityTable] object describing the
#'               mortality of the insured}
#'     \item{\code{$invalidityTable}}{For contracts with invalidity benefits,
#'               the [mortalityTable] object describing the probabilities of
#'               invalidity}
#'     \item{\code{$invalidityEndsContract}}{For contracts with invalidity
#'               benefits, whether a payment of an invalidity benefit ends the
#'               contract.}
#'     \item{\code{$i}}{Guaranteed yearly interest rate, default is 0.00, i.e. 0%}
#'     \item{\code{$balanceSheetDate}}{The day/month when balance sheet reserves
#'               are calculated. Value of type [Date], typically generated with
#'               [as.Date()]. The year is actually irrelevant, only the day and
#'               month are relevant.}
#'     \item{\code{$balanceSheetMethod}}{How to interpolate the balance sheet
#'               reserves (at the balandeSheetDate) from the yearly contractual
#'               reserves.}
#'     \item{\code{$surrenderValueCalculation}}{A function describing the surrender
#'               value calculation.}
#'     \item{\code{$premiumFrequencyOrder}}{Order of the approximation for
#'               payments within the year (unless an extra frequency loading is
#'               used => then leave this at 0)}
#'     \item{\code{$benefitFrequencyOrder}}{Order of the approximation for
#'               payments within the year (unless an extra frequency loading is
#'               used => then leave this at 0)}
#' }
#'
#' ## Elements of sublist \code{InsuranceContract.ParameterDefault$Costs}
#'
#' Definition of contractual costs charged to the contract. See [initializeCosts()].
#'
#' ## Elements of sublist \code{InsuranceContract.ParameterDefault$Loadings}
#'
#' \describe{
#'     \item{\code{$ongoingAlphaGrossPremium}}{Acquisition cost that increase the gross premium}
#'     \item{\code{$tax}}{insurance tax, factor on each premium paid, default is 4%, i.e. \code{i=0.04}}
#'     \item{\code{$unitcosts}}{Annual unit cost for each policy, absolute value (can be a function)}
#'     \item{\code{$security}}{Additional security loading on all benefit payments, factor on all benefits}
#'     \item{\code{$noMedicalExam}}{Loading when no medicial exam is done, % of SumInsured}
#'     \item{\code{$noMedicalExamRelative}}{Loading when no medicial exam is done, % of gross premium}
#'     \item{\code{$sumRebate}}{gross premium reduction for large premiums, % of SumInsured}
#'     \item{\code{$extraRebate}}{gross premium reduction for any reason, % of SumInsured}
#'     \item{\code{$premiumRebate}}{gross premium reduction for large premiums, % of gross premium}
#'     \item{\code{$partnerRebate}}{Rebate on premium with all surcharges and
#'               rebates when more than one contract is written with identical
#'               parameters. Sums with advanceBonusInclUnitCost and premiumRebate.}
#'     \item{\code{$extraChargeGrossPremium}}{extra charges on gross premium
#'               (smoker, leisure activities, BMI too high, etc.)}
#'     \item{\code{$benefitFrequencyLoading}}{Loading on the benefit for premium
#'               payment frequencies of more than once a year. Format is
#'               \code{list("1" = 0.0, "2" = 0.0, "4" = 0.0, "12" = 0.0)}}
#'     \item{\code{$premiumFrequencyLoading}}{Loading on the premium for premium
#'               payment frequencies of more than once a year. Format is
#'               \code{list("1" = 0.0, "2" = 0.0, "4" = 0.0, "12" = 0.0)}}
#'     \item{\code{$alphaRefundPeriod}}{How long the acquisition costs should be
#'               (partially) refunded in case of surrender or premium waiver.}
#' }
#'
#'
#' ## Elements of sublist \code{InsuranceContract.ParameterDefault$Features}
#'
#' \describe{
#'     \item{\code{$betaGammaInZillmer}}{Whether beta and gamma-costs should be
#'               included in the Zillmer premium calculation}
#'     \item{\code{$alphaRefundLinear}}{Whether the refund of alpha-costs on
#'               surrender is linear in t or follows the NPV of an annuity}
#' }
#'
#' ## Elements of sublist \code{InsuranceContract.ParameterDefault$ProfitParticipation}
#'
#' Parameters describing the profit participation (instance of [ProfitParticipation])
#' Most element descrive some kind of profit rate (which can vary in time),
#' while the bases, on which they are applied is defined in the profit scheme.
#'
#' \describe{
#'     \item{\code{$advanceProfitParticipation}}{Advance profit participation
#'               rate (percentage rebate of the gross premium)}
#'     \item{\code{$advanceProfitParticipationInclUnitCost}}{Advance profit
#'               participation rate (percentage rebate on the gross premium after all surcharges and unit costs.}
#'     \item{\code{$waitingPeriod}}{Waiting period of the profit sharing (e.g.
#'               no profit in the first two years of a contract, or similar)}
#'     \item{\code{$guaranteedInterest}}{Individual contract-specific overrides
#'               of the guaranteed interest rate (i.e. not keyed by year)}
#'     \item{\code{$interestProfitRate}}{Interest profit rate (guaranteed interest
#'               rate + interest profit rate = total credited rate)}
#'     \item{\code{$totalInterest}}{Total credited rate (guarantee + interest profit)}
#'     \item{\code{$mortalityProfitRate}}{Mortality Profit rate}
#'     \item{\code{$expenseProfitRate}}{Expense profit rate}
#'     \item{\code{$sumProfitRate}}{Sum profit rate (for high sumInsured)}
#'     \item{\code{$terminalBonusRate}}{Terminal bonus rate (non-terminal-bonus
#'              fund, but "old" Austrian terminal bonus)}
#'     \item{\code{$terminalBonusFundRate}}{Terminal bonus fund rate}
#'     \item{\code{$profitParticipationScheme}}{Profit participation scheme (object of class [ProfitParticipation])}
#'     \item{\code{$profitComponents}}{Profit components of the profit scheme. List containing one or more of \code{c("interest", "risk", "expense", "sum", "terminal")}}
#'     \item{\code{$profitClass}}{String describing the profit class the tariff
#'               is assigned to. Profit classes are used to bundle similar
#'               contracts (e.g. following similar risks) together. Profit
#'               participation rates are defined at the level of profit classes.}
#'     \item{\code{$profitRates}}{General, company-wide profit rates, key columns are year and profitClass}
#'
#'     \item{\code{$scenarios}}{profit participation scenarios (list of overridden parameters for each scenario)}
#' }
#'
#' ## Elements of sublist \code{InsuranceContract.ParameterDefault$Hooks}
#'
#' \describe{
#'     \item{\code{$adjustCashFlows}}{Function with signature \code{function(x, params, values, ...)} to adjust the benefit/premium cash flows after their setup.}
#'     \item{\code{$adjustCashFlowsCosts}}{Function with signature \code{function(x, params, values, ...)} to adjust the costs cash flows after their setup.}
#' }
#'
#'
#'
#' @examples
#' InsuranceContract.ParameterDefaults
#' @export
InsuranceContract.ParameterDefaults = list(
    ContractData = list(
        id = "Hauptvertrag",
        sumInsured = 100000,
        YOB = NULL,
        age = NULL,
        technicalAge = NULL,
        ageDifferences = NULL,                  # Age differences of all other insured relative to the first one, for joint live insurances
        sex = "unisex",                         # Sex, to allow gender-sepecific pricing (e.g. different mortalities or age modification)
        policyPeriod = 25,                      # total policy duration (including deferral period, guaranteed annuity payments etd.)
        premiumPeriod = NULL,                   # Default: policyPeriod, unless explicitly overridden
        deferralPeriod = 0,                     # deferral period for annuities
        guaranteedPeriod = 0,                   # guaranteed payments for annuities
        contractClosing = NULL,                 # Contract closing date (day/month is relevant for balance sheet reserves)
        blockStart = 0,                         # When the current tariff block starts (main block starts a 0, dynamic increases start later!), only used by the parent block (i.e. t=0 of child is aligned with t=blockStart of parent)

        premiumPayments = "in advance",         # premium payments in advance or arrears
        benefitPayments = "in advance",         # benefit payments in advance or arrears (annuities!)

        premiumFrequency = 1,                   # number of premium payments per year
        benefitFrequency = 1,                   # number of benefit payments per year (for annuities) or death benefit at the end of every 1/k-th year

        widowProportion = 0,                    # widow transition factor (on sum insured)
        deathBenefitProportion = 1,             # For endowments: Proportion of the death benefit relative to the life benefit
        premiumRefund = 0,                      # Proportion of premiums refunded on death (including additional risk, e.g. 1.10 = 110% of paid premiums)
        premiumIncrease = 1,                    # The yearly growth factor of the premium, i.e. 1.05 means +5% increase each year; a Vector describes the premiums for all years
        annuityIncrease = 1,                    # The yearly growth factor of the annuity payments, i.e. 1.05 means +5% incrase each year; a vector describes the annuity unit payments for all years
        deathBenefit = 1                        # The yearly relative death benefit (relative to the initial sum insured); Can be set to a function(len, params, values), e.g. deathBenefit = deathBenefit.linearDecreasing
    ),
    ContractState = list(
        premiumWaiver = FALSE,                  # contract is paid-up
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
        unitcosts = 0,                          # Annual unit cost for each policy, absolute value (can be a function)
        security = 0,                           # Additional security loading on all benefit payments, factor on all benefits
        noMedicalExam = 0,                      # Loading when no medicial exam is done, % of SumInsured
        noMedicalExamRelative = 0,              # Loading when no medicial exam is done, % of gross premium
        sumRebate = 0,                          # gross premium reduction for large premiums, % of SumInsured
        extraRebate = 0,                        # gross premium reduction for any reason, % of SumInsured
        premiumRebate = 0,                      # gross premium reduction for large premiums, % of gross premium # TODO
        partnerRebate = 0,                      # Partner rabate on premium (including loading and other rebates) if more than one similar contract is concluded
        extraChargeGrossPremium = 0,            # extra charges on gross premium (smoker, leisure activities, BMI too high, etc.)
        benefitFrequencyLoading = list("1" = 0.0, "2" = 0.0, "4" = 0.0, "12" = 0.0), # TODO: Properly implement this as a function
        premiumFrequencyLoading = list("1" = 0.0, "2" = 0.0, "4" = 0.0, "12" = 0.0), # TODO: Properly implement this as a function
        alphaRefundPeriod = 5                   # How long acquisition costs should be refunded in case of surrender
    ),
    Features = list(                          # Special cases for the calculations
        betaGammaInZillmer = FALSE,             # Whether beta and gamma-costs should be included in the Zillmer premium calculation
        alphaRefundLinear  = TRUE               # Whether the refund of alpha-costs on surrender is linear in t or follows the NPV of an annuity
    ),

    ProfitParticipation = list(
        advanceProfitParticipation = 0,                # advance profit participation (percentage of gross premium)
        advanceProfitParticipationInclUnitCost = 0,    # advance profit participation (percentage of premium including unit cost and all charges and rebates)

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
        profitComponents = c(),                 # Potential values: "interest", "risk", "expense", "sum", "terminal", "TBF"
        profitClass = NULL,
        profitRates = NULL,                     # General, company-wide profit rates, key columns are year and profitClass

        scenarios = list()                      # profit participation scenarios (list of overridden parameters for each scenario)
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
InsuranceContract.ParametersFill = function(params = InsuranceContract.ParameterStructure, costs = NULL, ...) {
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
#' @param ppParameters Whether profit participation parameters should also be
#'                     filled (default is TRUE)
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


