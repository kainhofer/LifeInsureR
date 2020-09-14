#' @include HelperFunctions.R InsuranceParameters.R ProfitParticipation.R
#'
#' @import MortalityTables
#' @import R6
#' @importFrom lubridate year month years days year<-
#' @importFrom objectProperties setSingleEnum
NULL

#' An enum specifying the main characteristics of the tarif.
#' Possible values are:
#' \describe{
#'   \item{annuity}{Whole life or term annuity (periodic survival benefits)
#'        with flexible payouts (constand, increasing, decreasing, arbitrary,
#'        etc.)}
#'   \item{wholelife}{A whole or term life insurance with only death benefits.
#'        The benefit can be constant, increasing, decreasing, described by
#'        a function, etc.}
#'   \item{endowment}{An  endowment with death and survival benefits,
#'        potentially with different benefits.}
#'   \item{pureendowment}{A pure endowment with only a survival benefit at
#'        the end of the contract. Optionally, in case of death, all or part
#'        of the premiums paid may be refunded.}
#'   \item{terme-fix}{A terme-fix insurance with a fixed payout at the end
#'        of the contract, even if the insured dies before that time.
#'        Premiums are paid until death of the insured.}
#'   \item{dread-disease}{A dread-disease insurance, which pays in case of
#'        a severe illness (typically heart attacks, cancer, strokes, etc.),
#'        but not in case of death.}
#'   \item{endowment + dread-disease}{A combination of an endowment and a
#'        temporary dread-disease insurance. Benefits occur either on death,
#'        severe illness or survival, whichever comes first.}
#' }
#' @export
TariffTypeEnum = objectProperties::setSingleEnum(
  "TariffType",
  levels = c(
    "annuity",
    "wholelife",
    "endowment",
    "pureendowment",
    "terme-fix",
    "dread-disease",
    "endowment + dread-disease"
  ))


############# Class InsuranceTarif ###########################################
#' Base class for traditional Insurance Tarifs (with fixed guarantee, profit
#' sharing and no unit-linked component)
#'
#' @description The class \code{InsuranceTarif} provides the code and general
#' framework to implement contract-independent functionality of a life insurance
#' product.
#'
#' @details This is a base class for holding contract-independent values and
#' providing methods to calculate cash flows, premiums, etc. Objects of this
#' class do NOT contain contract-specific values like age, death probabilities,
#' premiums, reserves, etc. Rather, they are the calculation kernels that will
#' be called by the \code{\link{InsuranceContract}} objects to make the actual,
#' tariff-specific calculations.
#'
#' Most methods of this class are not meant to be called manually, but are supposed
#' to be called by the InsuranceContract object with contract-specific information.
#' The only methods that are typically used for defining an insurance tariff are
#' the constructor \href{#method-new}{\code{InsuranceTarif$new()}} and the cloning method
#' \href{#method-createModification}{\code{InsuranceTarif$createModification()}}.
#' All other methods should never be called manually.
#'
#' However, as overriding private methods is not possible in an R6 class, all the
#' methods need to be public to allow overriding them in derived classes.
#'
# # Parameters for the constructors
#' @param name The unique name / ID of the tariff
#' @param type An enum specifying the main characteristics of the tarif. See [TariffTypeEnum]
#' @param tarif The tariff's public name to be stored in the `tarif` field.
#' @param desc A short human-readable description to be stored in the `desc` field.
# # General parameters for (almost) all function
#' @param params Contract-specific, full set of parameters of the contract
#'      (merged parameters of the defaults, the tariff, the profit participation
#'      scheme and the contract)
#' @param values Contract values calculated so far (in the \code{contract$Values}
#'      list) then this method is called by the contract object
#'
#' @param premiumCalculationTime The time when the premiums should be
#'        (re-)calculated according to the equivalence principle. A time 0
#'        means the initial premium calculation at contract closing, later
#'        premium calculation times can be used to re-calculate the new
#'        premium after a contract change (possibly including an existing reserve)
#'
#' @import MortalityTables
#' @examples
#' # Define an insurance tariff for 10-year endowments, using a guaranteed interest
#' # rate of 1% and the Austrian population mortality table of the census 2011.
#' # Premiums are paid monthly in advance during the whole contract period.
#' MortalityTables::mortalityTables.load("Austria_Census")
#' # Cost structure:
#' #   - 4% up-front acquisition costs (of premium sum)
#' #   - 1% collection cost of each premium paid
#' #   - 1%o yearly administration cost (of the sum insured) as long as premiums are paid
#' #   - 2%o yearly administration cost for paid-up contracts
#' #   - 10 Euro yearly unit costs (as long as premiums are paid)
#' costs.endw = initializeCosts(alpha = 0.04, beta = 0.01, gamma = 0.001,
#'     gamma.paidUp = 0.002, gamma.premiumfree = 0.002, unitcosts = 10)
#'
#' endowment.AT1 = InsuranceTarif$new(
#'     name = "Endow AT 1%", type = "endowment", tarif = "Austrian Endowment",
#'     desc = "An endowment for Austrian insured with 1% interest and no profit participation",
#'     ContractPeriod = 10,
#'     i = 0.01, mortalityTable = mort.AT.census.2011.unisex,
#'     costs = costs.endw, premiumFrequency = 12)
#'
#' # The instantiation of the actual contract will provide the contract specific
#' # information and immediately calculate all further values:
#' ctr.end.AT1 = InsuranceContract$new(tarif = endowment.AT1,
#'     contractClosing = as.Date("2020-07-01"), age = 42)
#'
#' # All values for the contract are already calculated during construction and
#' # stored in the ctr.end.AT1$Values list:
#' ctr.end.AT1$Values$basicData
#' ctr.end.AT1$Values$transitionProbabilities
#' ctr.end.AT1$Values$cashFlowsCosts
#' ctr.end.AT1$Values$presentValues
#' ctr.end.AT1$Values$premiums
#' ctr.end.AT1$Values$reserves
#' ctr.end.AT1$Values$premiumComposition
#' # etc.
#' @export
InsuranceTarif = R6Class(
  "InsuranceTarif",

  ######################### PUBLIC METHODS ##################################
  public  = list(
    #' @field name The tariff's unique name. Will also be used as the key for exported data.
    name  = "Insurance Contract Type",
    #' @field tarif The tariff's public name (typically a product name), not necessarily unique.
    tarif = NULL,
    #' @field desc A short human-readable description of the tariff and its main features.
    desc  = NULL,
    #' @field tariffType An enum specifying the main characteristics of the tarif.
    #' Possible values are:
    #' \describe{
    #'   \item{annuity}{Whole life or term annuity (periodic survival benefits)
    #'        with flexible payouts (constand, increasing, decreasing, arbitrary,
    #'        etc.)}
    #'   \item{wholelife}{A whole or term life insurance with only death benefits.
    #'        The benefit can be constant, increasing, decreasing, described by
    #'        a function, etc.}
    #'   \item{endowment}{An  endowment with death and survival benefits,
    #'        potentially with different benefits.}
    #'   \item{pureendowment}{A pure endowment with only a survival benefit at
    #'        the end of the contract. Optionally, in case of death, all or part
    #'        of the premiums paid may be refunded.}
    #'   \item{terme-fix}{A terme-fix insurance with a fixed payout at the end
    #'        of the contract, even if the insured dies before that time.
    #'        Premiums are paid until death of the insured.}
    #'   \item{dread-disease}{A dread-disease insurance, which pays in case of
    #'        a severe illness (typically heart attacks, cancer, strokes, etc.),
    #'        but not in case of death.}
    #'   \item{endowment + dread-disease}{A combination of an endowment and a
    #'        temporary dread-disease insurance. Benefits occur either on death,
    #'        severe illness or survival, whichever comes first.}
    #' }
    tariffType = ("wholelife"),

    #' @field Parameters A data structure (nested list) containing all relevant
    #' parameters describing a contract, its underlying tariff, the profit
    #' participation scheme etc. See [InsuranceContract.ParameterStructure] for
    #' all fields.
    Parameters = InsuranceContract.ParameterStructure,

    #' @description Initialize a new tariff object
    #' @details The constructor function defines a tariff and generates the
    #' corresponding data structure, which can then be used with the [InsuranceContract]
    #' class to define an actual contract using the tariff.
    #'
    #' The arguments passed to this function will be stored inside the
    #' \code{Parameters} field of the class, inside one of the lists sublists.
    #' The parameters are stacked from different layers (higher levels override
    #' default values from lower layers):
    #'
    #' * InsuranceContract object (parameters passed directly to the individual
    #'     contract)
    #' * ProfitParticipation object (parameters for profit participation, passed
    #'     to the definition of the profit plan, which is used for the tarif
    #'     definition or the contract)
    #' * InsuranceTarif object (parameters passed to the definition of the tariff
    #'     that was used for the contract)
    #' * Defaults taken from [InsuranceContract.ParameterStructure]
    #'
    #' The general implementation of this parameter layering means that (a) a tariff
    #' can already provide default values for contracts (e.g. a default maturity,
    #' default sum insured, etc) and (b) individual contracts can override all
    #' parameters defined with the underlying tariff. In particular the latter
    #' feature has many use-cases in prototyping: E.g. when you have a tariff
    #' with a guaranteed interest rate of 1\% and a certain mortality table,
    #' one can immediately instantiate a contract with an updated interest rate
    #' or mortality table for comparison. There is no need to re-implement a
    #' tariff for such comparisons, as long as only parameters are changed.
    #'
    #' @param ... Parameters for the [InsuranceContract.ParameterStructure],
    #'            defining the characteristics of the tariff.
    #' @import MortalityTables
    #' @examples
    #' MortalityTables::mortalityTables.load("Austria_Annuities_AVOe2005R")
    #' tarif.male = InsuranceTarif$new(name = "Annuity Males", type = "annuity",
    #'     i = 0.01, mortalityTable = AVOe2005R.male)
    initialize = function(name = NULL, type = "wholelife", tarif = "Generic Tarif", desc = "Description of tarif", ...) {
      if (!missing(name))           self$name = name;
      if (!missing(type))           self$tariffType = type;
      if (!missing(tarif))          self$tarif = tarif;
      if (!missing(desc))           self$desc = desc;

      # Set the passed arguments as tariff parameters
      self$Parameters = InsuranceContract.ParametersFill(self$Parameters, ...)

      # Use the profit participation's parameters as fallback for initialized parameters
      ppScheme = self$Parameters$ProfitParticipation$profitParticipationScheme;
      if (!is.null(ppScheme)) {
          self$Parameters$ProfitParticipation = InsuranceContract.ParametersFallback(self$Parameters$ProfitParticipation, ppScheme$Parameters)
      }

      # Fill all remaining uninitialized values with their defaults, except for profit participation params
      self$Parameters = InsuranceContract.ParametersFallback(self$Parameters, InsuranceContract.ParameterDefaults, ppParameters = FALSE);
    },

    #' @description create a copy of a tariff with certain parameters changed
    #' @details This method \code{createModification} returns a copy of the tariff
    #' with all given arguments changed in the tariff's `InsuranceTarif$Parameters`
    #' parameter list.
    #'
    #' As InsuranceTarif is a R6 class with reference logic, simply assigning
    #' the object to a new variable does not create a copy, but references the
    #' original tariff object. To create an actual copy, one needs to call this
    #' method, which first clones the whole object and then adjusts all parameters
    #' to the values passed to this method.
    #'
    #' @param tariffType An enum specifying the main characteristics of the tarif.
    #'       See [TariffTypeEnum]
    #' @param ... Parameters for the [InsuranceContract.ParameterStructure],
    #'            defining the characteristics of the tariff.
    #' @import MortalityTables
    #' @examples
    #' MortalityTables::mortalityTables.load("Austria_Annuities_AVOe2005R")
    #' tarif.male = InsuranceTarif$new(name = "Annuity Males", type = "annuity",
    #'     i = 0.01, mortalityTable = AVOe2005R.male)
    #' tarif.unisex = tarif.male$createModification(name = "Annuity unisex",
    #'     mortalityTable = AVOe2005R.unisex)
    createModification = function(name  = NULL, tarif = NULL, desc  = NULL, tariffType = NULL, ...) {
      cloned = self$clone();
      if (!missing(name))       cloned$name = name;
      if (!missing(tarif))      cloned$tarif = tarif;
      if (!missing(desc))       cloned$desc = desc;
      if (!missing(tariffType)) cloned$tariffType = tariffType;

      cloned$Parameters = InsuranceContract.ParametersFill(cloned$Parameters, ...);
      cloned
    },

    #' @description Retrieve the parameters for this tariff (can be overridden
    #' for each contract)
    #'
    #' @examples
    #' tarif.male = InsuranceTarif$new(name = "Annuity Males", type = "annuity",
    #'     i = 0.01, mortalityTable = AVOe2005R.male)
    #' tarif.male$getParameters()
    getParameters = function() {
      self$Parameters
    },

    #' @description Get some internal parameters cached (length of data.frames,
    #' policy periods cut at max.age, etc.)
    #'
    #' @details This methos is not meant to be called explicitly, but rather used
    #' by the InsuranceContract class. It returns a list of maturities and ages
    #' relevant for the contract-specific calculations
    #'
    #' @param ... currently unused
    getInternalValues = function(params, ...) {
      age = params$ContractData$technicalAge
      maxAge = MortalityTables::getOmega(params$ActuarialBases$mortalityTable)
      policyPeriod = params$ContractData$policyPeriod
      list(
        l = min(maxAge - age, policyPeriod) + 1,
        policyTerm = min(maxAge - age, policyPeriod),
        premiumTerm = min(policyPeriod, params$ContractData$premiumPeriod)
      )
    },


    #' @description Calculate the contract-relevant age(s) given a certain
    #' parameter data structure (contract-specific values)
    #'
    #' @details This method is not meant to be called explicitly, but rather used
    #' by the InsuranceContract class. It returns the relevant ages during the
    #' whole contract period
    getAges = function(params) {
      ages = ages(params$ActuarialBases$mortalityTable, YOB = params$ContractData$YOB);
      age = params$ContractData$technicalAge;
      if (age > 0) {
        ages = ages[-age:-1];
      }
      ages
    },

    #' @description Calculate the transition probabilities from the contract-specific
    #'  parameters passed as \code{params} and the already-calculated contract
    #'  values \code{values}
    #' @details Not to be called directly, but implicitly by the [InsuranceContract] object.
    getTransitionProbabilities = function(params, values) {
      age = params$ContractData$technicalAge;
      ages = self$getAges(params);
      q = MortalityTables::deathProbabilities(params$ActuarialBases$mortalityTable, YOB = params$ContractData$YOB, ageDifferences = params$ContractData$ageDifferences);
      if (age > 0) {
        q    = q[-age:-1];
      }
      if (!is.null(params$ActuarialBases$invalidityTable)) {
        i = MortalityTables::deathProbabilities(params$ActuarialBases$invalidityTable, YOB = params$ContractData$YOB, ageDifferences = params$ContractData$ageDifferences);
        if (age > 0) {
          i    = i[-age:-1];
        }
      } else {
        i = rep(0, length(q));
      }
      i = pad0(i, length(q));
      # invalidity/disease does NOT end the contract if flag is set!
      if (params$ActuarialBases$invalidityEndsContract) {
        p = 1 - q - i
      } else {
        p = 1 - q
      }
      df = data.frame(age = ages, q = q, i = i, p = p, row.names = ages - age)
      df
    },

    #' @description Obtain the cost structure from the cost parameter and the
    #' given paremeter set
    #' @details Not to be called directly, but implicitly by the [InsuranceContract] object.
    #' The cost parameter can be either an array of costs (generated by [initializeCosts()])
    #' or a function with parameters \code{param} and \code{values}(=NULL) returning
    #' an array of the required dimensions. This function makes sures that the
    #' latter function is actually evaluated.
    #'
    #' @param costs The cost parameter passed to the tarif definition or the
    #' contract (either an array of the form returned by [initializeCosts()] or
    #' a function(params, values) returning such an array)
    getCostValues = function(costs, params) {
        valueOrFunction(costs, params = params, values = NULL)
    },

    #' @description Returns the unit premium cash flow for the whole contract period.
    #'   - For constant premiums it will be rep(1, premiumPeriod),
    #'   - for single premiums it will be c(1, 0, 0, ...),
    #'   - for increasing premiums it will be (1+increase)^(0:(premiumPeriod-1))
    #' and 0 after the premium period
    #' @details Not to be called directly, but implicitly by the [InsuranceContract] object.
    #' @param len The desired length of the returned data frame (the number of contract periods desire)
    getPremiumCF = function(len, params, values) {
      premPeriod = min(params$ContractData$premiumPeriod, params$ContractData$policyPeriod, len);
      if (is.null(params$ContractData$premiumIncrease)) {
        pad0(rep(1, premPeriod - 1), len)
      } else {
        inc = valueOrFunction(params$ContractData$premiumIncrease, premiumPeriod = premPeriod, params = params, values = values)
        if (is.vector(inc) && length(inc) > 1) {
          # If premiumIncrease is (or returns) a vector, treat it as
          # relative premium amounts, ie. c(1, 1.1, 1.2) means +10% of
          # the initial premium for the second and third year
          pad0(inc, len)
        } else {
          pad0(inc ^ (0:(premPeriod - 1)), len)
        }
      }
    },

    #' @description Returns the unit annuity cash flow (guaranteed and contingent) for
    #'     the whole annuity payment period (after potential deferral period)
    #'   - For constant annuity it will be rep(1, annuityPeriod),
    #'   - for increasing annuities it will be (1+increase)^(0:(premiumPeriod-1))
    #' and 0 after the premium period
    #' @details Not to be called directly, but implicitly by the [InsuranceContract] object.
    #' @param len The desired length of the returned data frame (the number of contract periods desire)
    getAnnuityCF = function(len, params, values) {
      annuityPeriod = min(params$ContractData$policyPeriod - params$ContractData$deferralPeriod, len);
      if (is.null(params$ContractData$annuityIncrease)) {
        pad0(rep(1, annuityPeriod), len);
      } else {
        inc = valueOrFunction(params$ContractData$annuityIncrease, annuityPeriod = annuityPeriod, params = params, values = values)
        if (is.vector(inc) && length(inc) > 1) {
          # If annuityIncrease is (or returns) a vector, treat it as
          # relative annuity amounts, ie. c(1, 1.1, 1.2) means +10% of
          # the initial annuity for the second and third year
          pad0(inc, len)
        } else {
          # a numeric value means constant yearly increases (multiplicative)
          pad0(inc ^ (0:annuityPeriod), len)
        }
      }
    },

    #' @description Returns the unit death cash flow for the whole protection
    #' period (after potential deferral period!)
    #'   - For constant death benefit it will be rep(1, policyPeriod),
    #'   - for linearly decreasing sum insured it will be (policyPeriod:0)/policyPeriod
    #' @details Not to be called directly, but implicitly by the [InsuranceContract] object.
    #' @param len The desired length of the returned data frame (the number of contract periods desire)
    getDeathCF = function(len, params, values) {
      period = params$ContractData$policyPeriod - params$ContractData$deferralPeriod;
      if (is.null(params$ContractData$deathBenefit)) {
        pad0(rep(1, period), len)
      } else {
        benefit = valueOrFunction(params$ContractData$deathBenefit, len = len, params = params, values = values)
        if (is.vector(benefit) && length(benefit) > 1) {
          # If deathBenefit is (or returns) a vector, treat it as
          # relative annuity amounts, ie. c(1, 1.1, 1.2) means +10% of
          # the initial annuity for the second and third year
          pad0(benefit, len)
        } else {
          # constant death benefit
          pad0(rep(benefit, period), len)
        }
      }
    },

    #' @description Returns the basic (unit) cash flows associated with the type
    #' of insurance given in the InsuranceTarif's `tariffType` field
    #' @details Not to be called directly, but implicitly by the [InsuranceContract] object.
    getBasicCashFlows = function(params, values) {
      deferralPeriod = params$ContractData$deferralPeriod;
      guaranteedPeriod = params$ContractData$guaranteedPeriod;

      zeroes = pad0(0, values$int$l)

      cf = data.frame(
        guaranteed = zeroes,
        survival = zeroes,
        death = zeroes,
        disease = zeroes,
        sumInsured = rep(1, values$int$l)
      );
      if (self$tariffType == "annuity") {
        annuityPeriod = values$int$policyTerm - deferralPeriod;
        annuityCF = self$getAnnuityCF(len = annuityPeriod, params = params, values = values)
        # guaranteed payments exist only with annuities (first n years of the payment)
        cf$guaranteed = pad0(
          c(
            rep(0, deferralPeriod),
            head(annuityCF, n = guaranteedPeriod)
          ), values$int$l);
        cf$survival = pad0(c(
          rep(0, deferralPeriod + guaranteedPeriod),
          if (guaranteedPeriod > 0) tail(annuityCF, n = -guaranteedPeriod) else annuityCF,
          0), values$int$l)

        # start current contract block after deferral period
        cf$sumInsured = c(rep(0, deferralPeriod), annuityCF, 0)


      } else if (self$tariffType == "terme-fix") {
        # Begin of bock does not have any influence
        cf$guaranteed = c(rep(0, values$int$policyTerm), 1)

      } else if (self$tariffType == "dread-disease") {
        # potential Payments start after deferral period
        cf$disease = c(
          rep(0, deferralPeriod),
          rep(1, values$int$l - 1 - deferralPeriod),
          0)
      } else {
        # For endowments, use the death factor here in the basic death CF
        # to fix the relation of death to survival benefit
        deathCF = self$getDeathCF(values$int$l - 1 - deferralPeriod, params = params, values = values)

        if (self$tariffType == "endowment" || self$tariffType == "pureendowment" || self$tariffType == "endowment + dread-disease") {
          cf$survival = c(rep(0, values$int$policyTerm), 1)
        }
        if (self$tariffType == "endowment" || self$tariffType == "wholelife" || self$tariffType == "endowment + dread-disease") {
          cf$death = c(rep(0, deferralPeriod), deathCF, 0)
          cf$sumInsured = c(rep(0, deferralPeriod), deathCF, 1);
        }
        if (self$tariffType == "endowment + dread-disease") {
          cf$disease = c(
            rep(0, deferralPeriod),
            rep(1, values$int$l - 1 - deferralPeriod),
            0);
        }
      }
      cf
    },

    #' @description Returns the cash flows for the contract given the parameters
    #' @details Not to be called directly, but implicitly by the [InsuranceContract] object.
    getCashFlows = function(params, values) {
      age = params$ContractData$technicalAge;

      if (is.null(values$cashFlowsBasic)) {
        values$cashFlowsBasic = self$getBasicCashFlows(params, values);
      }
      cflen = values$int$l
      zeroes = pad0(0, cflen)
      ages = pad0(self$getAges(params), cflen);
      cf = data.frame(
        premiums_advance   = zeroes,
        premiums_arrears   = zeroes,
        guaranteed_advance = zeroes,
        guaranteed_arrears = zeroes,
        survival_advance   = zeroes,
        survival_arrears   = zeroes,
        death_SumInsured   = zeroes,
        disease_SumInsured = zeroes,
        death_GrossPremium = zeroes,
        death_Refund_past  = zeroes,
        death_PremiumFree  = zeroes,
        row.names          = ages - age
      );

      # Premiums:
      if (!params$ContractState$premiumWaiver) {
        premiums = self$getPremiumCF(len = cflen, params = params, values = values)
        if (params$ContractData$premiumPayments == "in advance") {
          cf$premiums_advance = premiums;
        } else {
          cf$premiums_arrears = premiums;
        }
      }

      # Survival Benefits
      if (params$ContractData$benefitPayments == "in advance") {
        cf$guaranteed_advance = pad0(values$cashFlowsBasic$guaranteed, cflen);
        cf$survival_advance = pad0(values$cashFlowsBasic$survival, cflen);
      } else {
        cf$guaranteed_arrears = pad0(values$cashFlowsBasic$guaranteed, cflen);
        cf$survival_arrears = pad0(values$cashFlowsBasic$survival, cflen);
      }

      # Death Benefits
      cf$death_SumInsured = pad0(values$cashFlowsBasic$death, cflen);
      cf$disease_SumInsured = pad0(values$cashFlowsBasic$disease, cflen);
      cf$death_PremiumFree = cf$death_SumInsured;
      # premium refund
      if (params$ContractData$premiumRefund != 0) {
        totalpremiumcf = cf$premiums_advance + pad0(c(0, cf$premiums_arrears), cflen);

        # death benefit for premium refund is the sum of all premiums so far:
        cf$death_GrossPremium = pad0(Reduce("+", totalpremiumcf[0:params$ContractData$policyPeriod], accumulate = TRUE), cflen)
        cf$death_Refund_past = cf$death_GrossPremium
        cf$death_Refund_past[(cf$death_GrossPremium > 0)] = 1;
      }

      applyHook(params$Hooks$adjustCashFlows, cf, params, values)
    },

    #' @description Returns the cost cash flows of the contract given the contract
    #'  and tariff parameters
    #' @details Not to be called directly, but implicitly by the [InsuranceContract] object.
    getCashFlowsCosts = function(params, values) {
      dm = dim(params$Costs);
      dmnames = dimnames(params$Costs);
      cf = array(0, dim = list(values$int$l, dm[1], dm[2]), dimnames = list(0:(values$int$l - 1), dmnames[[1]], dmnames[[2]]));
      cf[1,,] = cf[1,,] + params$Costs[,,"once"]
      for (i in 1:values$int$premiumTerm) {
        cf[i,,] = cf[i,,] + params$Costs[,,"PremiumPeriod"];
      }
      if (values$int$premiumTerm < values$int$policyTerm) {
        for (i in (values$int$premiumTerm + 1):values$int$policyTerm) {
          cf[i,,] = cf[i,,] + params$Costs[,,"PremiumFree"];
        }
      }
      for (i in 1:values$int$policyTerm) {
        cf[i,,] = cf[i,,] + params$Costs[,,"PolicyPeriod"];
      }

      # After premiums are waived, use the gamma_nopremiums instead of gamma:
      if (params$ContractState$premiumWaiver) {
        cf[,"gamma",] = cf[,"gamma_nopremiums",];
      }

      # some values like sumInsured or gross premium might change over time,
      # so multiply them with the unit cash flows stored in values$cashFlows
      cf[,,"SumInsured"] = cf[,,"SumInsured"] * values$cashFlowsBasic$sumInsured

      applyHook(params$Hooks$adjustCashFlowsCosts, cf, params, values)
    },

    #' @description Returns the present values of the cash flows of the contract
    #' (cash flows already calculated and stored in the \code{cashFlows} data.frame)
    #' @details Not to be called directly, but implicitly by the [InsuranceContract] object.
    #' @param cashFlows data.frame of cash flows calculated by a call to \href{#method-getCashFlows}{\code{InsuranceTarif$getCashFlows()}}
    presentValueCashFlows = function(cashFlows, params, values) {

      qq = self$getTransitionProbabilities(params);
      qx = pad0(qq$q, values$int$l);
      ix = pad0(qq$i, values$int$l);
      px = pad0(qq$p, values$int$l);

      i = params$ActuarialBases$i;
      v = 1/(1 + i);
      benefitFreqCorr = correctionPaymentFrequency(i = i,
                                                   m = params$ContractData$benefitFrequency,
                                                   order = params$ActuarialBases$benefitFrequencyOrder);
      premiumFreqCorr = correctionPaymentFrequency(i = i,
                                                   m = params$ContractData$premiumFrequency,
                                                   order = params$ActuarialBases$premiumFrequencyOrder);

      pvRefund = calculatePVDeath(px, qx, values$cashFlows$death_GrossPremium, v = v);
      pvRefundPast = calculatePVDeath(
        px, qx,
        values$cashFlows$death_Refund_past,
        v = v) *
        (values$cashFlows[,"death_GrossPremium"] - values$cashFlows[,"premiums_advance"]);

      pv = cbind(
        premiums = calculatePVSurvival(
          px, qx,
          values$cashFlows$premiums_advance, values$cashFlows$premiums_arrears,
          m = params$ContractData$premiumFrequency, mCorrection = premiumFreqCorr,
          v = v),
        guaranteed = calculatePVGuaranteed(
          values$cashFlows$guaranteed_advance, values$cashFlows$guaranteed_arrears,
          m = params$ContractData$benefitFrequency, mCorrection = benefitFreqCorr,
          v = v),
        survival = calculatePVSurvival(
          px, qx,
          values$cashFlows$survival_advance, values$cashFlows$survival_arrears,
          m = params$ContractData$benefitFrequency, mCorrection = benefitFreqCorr,
          v = v),
        death_SumInsured = calculatePVDeath(
          px, qx,
          values$cashFlows$death_SumInsured,
          v = v),
        disease_SumInsured = calculatePVDisease(
          px, qx, ix,
          values$cashFlows$disease_SumInsured, v = v),
        death_GrossPremium = pvRefund,
        death_Refund_past = pvRefundPast,
        death_Refund_future = pvRefund - pvRefundPast,
        death_PremiumFree = calculatePVDeath(
          px, qx,
          values$cashFlows$death_PremiumFree, v = v)
      );

      rownames(pv) <- pad0(rownames(qq), values$int$l);
      pv
    },

    #' @description Calculates the present values of the cost cash flows of the
    #' contract (cost cash flows alreay calculated by \href{#method-getCashFlowsCosts}{\code{InsuranceTarif$getCashFlowsCosts()}}
    #' and stored in the \code{values} list
    #' @details Not to be called directly, but implicitly by the [InsuranceContract] object.
    presentValueCashFlowsCosts = function(params, values) {
      len = values$int$l;
      q = self$getTransitionProbabilities(params);
      qx = pad0(q$q, len);
      px = pad0(q$p, len);
      v = 1/(1 + params$ActuarialBases$i)
      pvc = calculatePVCosts(px, qx, values$cashFlowsCosts, v = v);
      pvc
    },

    #' @description Convert the cost values array to a tx15 matrix
    #' @details Not to be called directly, but implicitly by the [InsuranceContract] object.
    #' Convert the array containing cost values like cashflows, present
    #' values, etc. (objects of dimension tx5x3) to a matrix with dimensions (tx15)
    #' @param costValues The cost data structure (array of size tx5x3) to be converted to a matrix
    costValuesAsMatrix = function(costValues) {
      dm = dim(costValues);
      nm = dimnames(costValues);
      colnames = t(outer(nm[[2]], nm[[3]], paste, sep = "."));

      res = aperm(costValues, c(1,3,2));
      dim(res) = c(dm[[1]], dm[[2]]*dm[[3]]);
      dimnames(res) = list(nm[[1]], colnames)
      res
    },

    #' @description Calculate the cash flows in monetary terms of the insurance contract
    #' @details Once the premiums of the insurance contracts are calculated, all
    #' cash flows can also be expressed in absolute terms. This function
    #' calculates these time series in monetary terms, once the premiums
    #' are calculated by the previous functions of this class.
    #'
    #' This method is NOT to be called directly, but implicitly by the [InsuranceContract] object.
    getAbsCashFlows = function(params, values) {

        # TODO: Set up a nice list with coefficients for each type of cashflow,
        # rather than multiplying each item manually (this also mitigates the risk
        # of forgetting a dimension, because then the dimensions would not match,
        # while here it's easy to overlook a multiplication)
        # Multiply each CF column by the corresponding basis
        #
        # All propSI cash flows are already set up with the correct multiple
        # of the sumInsured (in cashFlowsBasic) for non-constant sums insured.
        # So here, we don't need to multiply with  values$cashFlowsBasic$sumInsured!
        propGP = c("premiums_advance", "premiums_arrears");
        propSI = c("guaranteed_advance", "guaranteed_arrears",
                   "survival_advance", "survival_arrears", "death_SumInsured",
                   "death_PremiumFree", "disease_SumInsured");
        propPS = c("death_GrossPremium", "death_Refund_past");
      values$cashFlows[,propGP] = values$cashFlows[,propGP] * values$premiums[["gross"]];
      values$cashFlows[,propSI] = values$cashFlows[,propSI] * params$ContractData$sumInsured;
      values$cashFlows[,propPS] = values$cashFlows[,propPS] * values$premiums[["gross"]] * params$ContractData$premiumRefund;

      # Sum all death-related payments to "death"  and remove the death_GrossPremium column
      values$cashFlows[,"death_SumInsured"] = values$cashFlows[,"death_SumInsured"] + values$cashFlows[,"death_GrossPremium"]
      colnames(values$cashFlows)[colnames(values$cashFlows) == "death_SumInsured"] = "death";
      # cashFlows[,"death_GrossPremium"] = NULL;

      # costs relative to sumInsured are already set up as the correct multiple
      # of the original SI, including the dynamic changes over time!
      values$cashFlowsCosts = values$cashFlowsCosts[,,"SumInsured"] * params$ContractData$sumInsured +
        values$cashFlowsCosts[,,"SumPremiums"] * values$unitPremiumSum * values$premiums[["gross"]] +
        values$cashFlowsCosts[,,"GrossPremium"] * values$premiums[["gross"]] +
          values$cashFlowsCosts[,,"NetPremium"] * values$premiums[["net"]] +
          values$cashFlowsCosts[,,"Constant"];

      cbind(values$cashFlows, values$cashFlowsCosts)
    },

    #' @description Calculate the absolute present value time series of the insurance contract
    #' @details Once the premiums of the insurance contracts are calculated, all
    #' present values can also be expressed in absolute terms. This function
    #' calculates these time series in monetary terms, once the premiums and the
    #'  unit-benefit present values are calculated by the previous functions of
    #'  this classe.
    #'
    #' This method is NOT to be called directly, but implicitly by the [InsuranceContract] object.
    getAbsPresentValues = function(params, values) {
      pv = values$presentValues;

      #pv[,"age"] = pv[,"premiums"];
      #colnames(pv)[colnames(pv)=="age"] = "premiums.unit";

      # Multiply each CF column by the corresponding basis
      pv[,"premiums"] = pv[,"premiums"] * values$premiums[["gross"]];
      pv[,c("guaranteed", "survival", "death_SumInsured", "disease_SumInsured", "death_PremiumFree")] =
        pv[,c("guaranteed", "survival", "death_SumInsured", "disease_SumInsured", "death_PremiumFree")] * params$ContractData$sumInsured;
      pv[,c("death_GrossPremium", "death_Refund_past", "death_Refund_future")] = pv[,c("death_GrossPremium", "death_Refund_past", "death_Refund_future")] * values$premiums[["gross"]] * params$ContractData$premiumRefund;
      pv[,c("benefits", "benefitsAndRefund", "alpha", "Zillmer", "beta", "gamma", "gamma_nopremiums", "unitcosts")] =
        pv[,c("benefits", "benefitsAndRefund", "alpha", "Zillmer", "beta", "gamma", "gamma_nopremiums", "unitcosts")] * params$ContractData$sumInsured;

      # Sum all death-related payments to "death"  and remove the death_SumInsured column
      pv[,"death_SumInsured"] = pv[,"death_SumInsured"] + pv[,"death_GrossPremium"]
      colnames(pv)[colnames(pv) == "death_SumInsured"] = "death";

      cbind("premiums.unit" = values$presentValues[,"premiums"], pv)
    },


    #' @description Calculate the absolute present value time series of the
    #' benefits of the insurance contract
    #' @details Once the premiums of the insurance contracts are calculated, all
    #' present values can also be expressed in absolute terms. This function
    #' calculates these time series of the benefits present values in monetary
    #' terms, once the premiums and the unit-benefit present values are calculated
    #'  by the previous functions of this classe.
    #'
    #' This method is NOT to be called directly, but implicitly by the [InsuranceContract] object.
    presentValueBenefits = function(params, values) {
      # TODO: Here we don't use the securityLoading parameter => Shall it be used or are these values to be understood without additional security loading?
      benefits    = values$presentValues[,"survival"] +
                    values$presentValues[,"guaranteed"] +
                    values$presentValues[,"death_SumInsured"] +
                    values$presentValues[,"disease_SumInsured"];
      allBenefits = benefits +
          values$presentValues[,"death_GrossPremium"] * values$premiums[["unit.gross"]] * params$ContractData$premiumRefund;

      benefitsCosts = values$presentValuesCosts[,,"SumInsured"] +
        values$presentValuesCosts[,,"SumPremiums"] * values$unitPremiumSum * values$premiums[["unit.gross"]] +
        values$presentValuesCosts[,,"GrossPremium"] * values$premiums[["unit.gross"]] +
        values$presentValuesCosts[,,"NetPremium"] * values$premiums[["unit.net"]] +
        values$presentValuesCosts[,,"Constant"] / params$ContractData$sumInsured;

      cbind(
        benefits = benefits,
        benefitsAndRefund = allBenefits,
        benefitsCosts)
    },

    #' @description Calculate the linear coefficients of the premium calculation formula for the insurance contract
    #' @details Not to be called directly, but implicitly by the [InsuranceContract] object.
    #' When \code{getPremiumCoefficients} is called, the \code{values$premiums}
    #' array has NOT yet been filled! Instead, all premiums already calculated
    #' (and required for the premium coefficients) are passed in the \code{premiums}
    #' argument.
    #'
    #' @param type The premium that is supposed to be calculated ("gross", "Zillmer", "net")
    #' @param coeffBenefits (empty) data structure of the benefit coefficients.
    #'        The actual values have no meaning, this parameter is only used to
    #'        derive the required dimensions
    #' @param coeffCosts (empty) data structure of the cost coefficients. The
    #'        actual values have no meaning, this parameter is only used to
    #'        derive the required dimensions
    #' @param premiums The premium components that have already been calculated
    #'         (e.g. for net and Zillmer, the gross premium has already been
    #'         calculated to allow modelling the premium refund)
    getPremiumCoefficients = function(type = "gross", coeffBenefits, coeffCosts, premiums, params, values, premiumCalculationTime = values$int$premiumCalculationTime) {
      # Merge a possibly passed loadings override with the defaults of this class:
      securityLoading = valueOrFunction(params$Loadings$security, params = params, values = values);
      t = as.character(premiumCalculationTime)

      coeff = list(
        "SumInsured" = list("benefits" = coeffBenefits*0, "costs" = coeffCosts*0),
        "Premium"    = list("benefits" = coeffBenefits*0, "costs" = coeffCosts*0)
      );

      coeff[["Premium"]][["benefits"]][["premiums"]]            = 1;

      # Costs proportional to NetPremium introduce a non-linearity, as the NP is not available when the gross premium is calculated
      # => the corresponding costs PV is included in the coefficient!
      coeff.benefits = (1 + securityLoading);
      if (type == "gross") {
          # TODO: How to include this into the Zillmer premium calculation?
          coeff.benefits = coeff.benefits * (1 + sum(values$presentValuesCosts[t, c("alpha", "beta", "gamma"), "NetPremium"]) / values$presentValues[[t,"premiums"]])
      }
      coeff[["SumInsured"]][["benefits"]][["guaranteed"]]       = coeff.benefits;
      coeff[["SumInsured"]][["benefits"]][["survival"]]         = coeff.benefits;
      coeff[["SumInsured"]][["benefits"]][["death_SumInsured"]] = coeff.benefits;
      coeff[["SumInsured"]][["benefits"]][["disease_SumInsured"]] = coeff.benefits;

      # Premium refund is handled differently for gross and net premiums, because it is proportional to the gross premium
      if (type == "gross") {
        coeff[["Premium"]][["benefits"]][["death_GrossPremium"]] = -params$ContractData$premiumRefund * coeff.benefits;
      } else if (type == "net" || type == "Zillmer") {
        coeff[["SumInsured"]][["benefits"]][["death_GrossPremium"]] = premiums[["unit.gross"]] * params$ContractData$premiumRefund * (1 + securityLoading);
      }


      # coefficients for the costs

      if (type == "gross") {
        coeff[["SumInsured"]][["costs"]]["alpha", "SumInsured"] = 1;
        coeff[["SumInsured"]][["costs"]]["beta",  "SumInsured"] = 1;
        coeff[["SumInsured"]][["costs"]]["gamma", "SumInsured"] = 1;
        # TODO: How to handle beta costs proportional to Sum Insured
        coeff[["Premium"]][["costs"]]["alpha", "SumPremiums"] = -values$unitPremiumSum;
        coeff[["Premium"]][["costs"]]["beta",  "SumPremiums"] = -values$unitPremiumSum;
        coeff[["Premium"]][["costs"]]["gamma", "SumPremiums"] = -values$unitPremiumSum;

        coeff[["Premium"]][["costs"]]["alpha", "GrossPremium"] = -1;
        coeff[["Premium"]][["costs"]]["beta",  "GrossPremium"] = -1;
        coeff[["Premium"]][["costs"]]["gamma", "GrossPremium"] = -1;

        coeff[["SumInsured"]][["costs"]]["alpha", "Constant"] = 1 / params$ContractData$sumInsured;
        coeff[["SumInsured"]][["costs"]]["beta",  "Constant"] = 1 / params$ContractData$sumInsured;
        coeff[["SumInsured"]][["costs"]]["gamma", "Constant"] = 1 / params$ContractData$sumInsured;

      } else if (type == "Zillmer") {
          # TODO: Include costs with basis NetPremium and fixed costs!
        coeff[["SumInsured"]][["costs"]]["Zillmer","SumInsured"] = 1;
        coeff[["SumInsured"]][["costs"]]["Zillmer","SumPremiums"] = values$unitPremiumSum * premiums[["unit.gross"]];
        coeff[["SumInsured"]][["costs"]]["Zillmer","GrossPremium"] = premiums[["unit.gross"]];
        if (params$Features$betaGammaInZillmer) {
          coeff[["SumInsured"]][["costs"]]["beta",  "SumInsured"] = 1;
          coeff[["SumInsured"]][["costs"]]["gamma", "SumInsured"] = 1;
          coeff[["SumInsured"]][["costs"]]["beta",  "SumPremiums"] = values$unitPremiumSum * premiums[["unit.gross"]];
          coeff[["SumInsured"]][["costs"]]["gamma", "SumPremiums"] = values$unitPremiumSum * premiums[["unit.gross"]];
          coeff[["SumInsured"]][["costs"]]["beta",  "GrossPremium"] = premiums[["unit.gross"]];
          coeff[["SumInsured"]][["costs"]]["gamma", "GrossPremium"] = premiums[["unit.gross"]];
        }
      }

      coeff
    },

    #' @description Calculate the premiums of the InsuranceContract given the
    #' parameters, present values and premium cofficients already calculated and
    #' stored in the \code{params} and \code{values} lists.
    #'
    #' @details Not to be called directly, but implicitly by the [InsuranceContract] object.
    premiumCalculation = function(params, values, premiumCalculationTime = values$int$premiumCalculationTime) {
      loadings = params$Loadings;
      sumInsured = params$ContractData$sumInsured
      values$premiums = c(
        "unit.net" = 0, "unit.Zillmer" = 0, "unit.gross" = 0,
        "net" = 0, "Zillmer" = 0, "gross" = 0,
        "unitcost" = 0, "written_yearly" = 0,
        "written_beforetax" = 0, "tax" = 0, "written" = 0);
      coefficients = list("gross" = c(), "Zillmer" = c(), "net" = c());

      # Get the present values of the premiums, claims and costs at time 'premiumCalculationTime' (where the premium is to be calculated)
      t = as.character(premiumCalculationTime)
      pv = values$presentValues[t,]
      pvCost = values$presentValuesCosts[t,,]

      if (pv[["premiums"]] == 0) {
        return(list("premiums" = values$premiums, "coefficients" = coefficients))
      }


      # net, gross and Zillmer premiums are calculated from the present values using the coefficients on each present value as described in the formulas document
      coeff = self$getPremiumCoefficients("gross", pv * 0, pvCost * 0, premiums = values$premiums, params = params, values = values, premiumCalculationTime = premiumCalculationTime)
      enumerator  = sum(coeff[["SumInsured"]][["benefits"]] * pv) + sum(coeff[["SumInsured"]][["costs"]] * pvCost);
      denominator = sum(coeff[["Premium"   ]][["benefits"]] * pv) + sum(coeff[["Premium"   ]][["costs"]] * pvCost);
      values$premiums[["unit.gross"]] = enumerator/denominator * (1 + loadings$ongoingAlphaGrossPremium);
      values$premiums[["gross"]] = values$premiums[["unit.gross"]] * sumInsured;
      coefficients[["gross"]] = coeff;

      coeff = self$getPremiumCoefficients("net", pv*0, pvCost*0, premiums = values$premiums, params = params, values = values, premiumCalculationTime = premiumCalculationTime)
      enumerator  = sum(coeff[["SumInsured"]][["benefits"]] * pv) + sum(coeff[["SumInsured"]][["costs"]] * pvCost);
      denominator = sum(coeff[["Premium"   ]][["benefits"]] * pv) + sum(coeff[["Premium"   ]][["costs"]] * pvCost);
      values$premiums[["unit.net"]] = enumerator/denominator;
      values$premiums[["net"]] = values$premiums[["unit.net"]] * sumInsured;
      coefficients[["net"]] = coeff;

      coeff = self$getPremiumCoefficients("Zillmer", pv * 0, pvCost * 0, premiums = values$premiums, params = params, values = values, premiumCalculationTime = premiumCalculationTime);
      enumerator  = sum(coeff[["SumInsured"]][["benefits"]] * pv) + sum(coeff[["SumInsured"]][["costs"]] * pvCost);
      denominator = sum(coeff[["Premium"   ]][["benefits"]] * pv) + sum(coeff[["Premium"   ]][["costs"]] * pvCost);
      values$premiums[["unit.Zillmer"]] = enumerator/denominator;
      values$premiums[["Zillmer"]] = values$premiums[["unit.Zillmer"]] * sumInsured;
      coefficients[["Zillmer"]] = coeff;


      # The written premium is the gross premium with additional loadings, rebates, unit costs and taxes
      tax           = valueOrFunction(loadings$tax,          params = params, values = values);
      unitCosts     = valueOrFunction(loadings$unitcosts,    params = params, values = values);
      noMedicalExam = valueOrFunction(loadings$noMedicalExam,params = params, values = values);
      noMedicalExam.relative = valueOrFunction(loadings$noMedicalExamRelative,params = params, values = values);
      extraRebate   = valueOrFunction(loadings$extraRebate,  params = params, values = values);
      sumRebate     = valueOrFunction(loadings$sumRebate,    params = params, values = values);
      premiumRebate = valueOrFunction(loadings$premiumRebate,params = params, values = values);
      extraChargeGrossPremium = valueOrFunction(loadings$extraChargeGrossPremium, params = params, values = values);
      advanceProfitParticipation = 0;
      advanceProfitParticipationUnitCosts = 0;
      ppScheme      = params$ProfitParticipation$profitParticipationScheme;
      if (!is.null(ppScheme)) {
          advanceProfitParticipation = ppScheme$getAdvanceProfitParticipation(params = params, values = values)
          advanceProfitParticipationUnitCosts = ppScheme$getAdvanceProfitParticipationAfterUnitCosts(params = params, values = values)
      }
      if (is.null(advanceProfitParticipation)) advanceProfitParticipation = 0;
      if (is.null(advanceProfitParticipationUnitCosts)) advanceProfitParticipationUnitCosts = 0;

      partnerRebate = valueOrFunction(loadings$partnerRebate, params = params, values = values);

      pv.unitcosts = pvCost["unitcosts","SumInsured"] * sumInsured +
        pvCost["unitcosts","SumPremiums"] * values$unitPremiumSum * values$premiums[["gross"]] +
        pvCost["unitcosts","GrossPremium"] * values$premiums[["gross"]] +
        pvCost["unitcosts","NetPremium"] * values$premiums[["net"]] +
        pvCost["unitcosts","Constant"];
      premium.unitcosts = pv.unitcosts / pv[["premiums"]] + valueOrFunction(loadings$unitcosts, params = params, values = values);
      values$premiums[["unitcost"]] = premium.unitcosts;


      frequencyLoading = valueOrFunction(loadings$premiumFrequencyLoading, params = params, values = values);
      premiumBeforeTax = (values$premiums[["unit.gross"]]*(1 + noMedicalExam.relative + extraChargeGrossPremium) + noMedicalExam - sumRebate - extraRebate) * sumInsured * (1 - advanceProfitParticipation) + premium.unitcosts;
      premiumBeforeTax = premiumBeforeTax * (1 - premiumRebate - advanceProfitParticipationUnitCosts - partnerRebate);
      # TODO / FIXME: Add a check that frequencyLoading has an entry for the premiumFrequency -> Otherwise do not add any loading (currently NULL is returned, basically setting all premiums to NULL)
      premiumBeforeTax.y = premiumBeforeTax * (1 + frequencyLoading[[toString(params$ContractData$premiumFrequency)]]);
      premiumBeforeTax = premiumBeforeTax.y / params$ContractData$premiumFrequency;
      values$premiums[["written_yearly"]] = premiumBeforeTax.y * (1 + tax)
      values$premiums[["written_beforetax"]] = premiumBeforeTax;
      values$premiums[["tax"]] = premiumBeforeTax * tax;
      values$premiums[["written"]] = premiumBeforeTax * (1 + tax);

      list("premiums" = values$premiums, "coefficients" = coefficients)
    },

    #' @description Calculate the reserves of the InsuranceContract given the
    #' parameters, present values and premiums already calculated and stored in
    #' the \code{params} and \code{values} lists.
    #'
    #' @details Not to be called directly, but implicitly by the [InsuranceContract] object.
    reserveCalculation = function(params, values) {
      t = "0"
      securityFactor = (1 + valueOrFunction(params$Loadings$security, params = params, values = values));
      ppScheme      = params$ProfitParticipation$profitParticipationScheme;

      # Net, Zillmer and Gross reserves
      resNet = values$absPresentValues[,"benefitsAndRefund"] * securityFactor - values$premiums[["net"]] * values$absPresentValues[,"premiums.unit"];
      BWZcorr = ifelse(values$absPresentValues[t, "premiums"] == 0, 0,
                       values$absPresentValues[t, "Zillmer"] / values$absPresentValues[t, "premiums"]) * values$absPresentValues[,"premiums"];
      resZ = resNet - BWZcorr;

      resAdeq = values$absPresentValues[,"benefitsAndRefund"] * securityFactor +
        values$absPresentValues[,"alpha"] + values$absPresentValues[,"beta"] + values$absPresentValues[,"gamma"] -
        values$premiums[["gross"]] * values$absPresentValues[,"premiums.unit"];

      #values$premiums[["Zillmer"]] * values$absPresentValues[,"premiums"];
      resGamma = values$absPresentValues[,"gamma"] -
        ifelse(values$absPresentValues[t, "premiums"] == 0, 0,
               values$absPresentValues[t, "gamma"] / values$absPresentValues[t, "premiums"]) * values$absPresentValues[,"premiums"]

      advanceProfitParticipation = 0;
      if (!is.null(ppScheme)) {
          advanceProfitParticipation = ppScheme$getAdvanceProfitParticipation(params = params, values = values)
      }
      resConversion = (resZ + resGamma) * (1 - advanceProfitParticipation);

      # Alpha refund: Distribute alpha-costs to 5 years (or if shorter, the policy period), always starting at time 0:
      # If alphaRefunded==TRUE, don't refund a second time!
      if (params$ContractState$alphaRefunded) {
        alphaRefund = 0
      } else {
        r = min(params$ContractData$policyPeriod, params$Loadings$alphaRefundPeriod);
        ZillmerSoFar = Reduce("+", values$absCashFlows$Zillmer, accumulate = TRUE);
        ZillmerTotal = sum(values$absCashFlows$Zillmer);
        len = length(ZillmerSoFar);
        if (params$Features$alphaRefundLinear) {
          ZillmerVerteilungCoeff = pad0((0:r)/r, len, 1);
        } else {
          q = self$getTransitionProbabilities(params);
          # vector of all ä_{x+t, r-t}
          pvAlphaTmp = calculatePVSurvival(q = pad0(q$q, len), advance = pad0(rep(1,r), len), v = 1/(1 + params$ActuarialBases$i));
          ZillmerVerteilungCoeff = (1 - pvAlphaTmp/pvAlphaTmp[[1]]);
        }
        alphaRefund = ZillmerSoFar - ZillmerVerteilungCoeff * ZillmerTotal;
      }

      # Reduction Reserve: Reserve used for contract modifications:
      resReduction = pmax(0, resZ + resGamma + alphaRefund) # V_{x,n}^{Rkf}

      # Collect all reserves to one large matrix
      res = cbind(
            "SumInsured"  = head0(rep(params$ContractData$sumInsured, values$int$l)),
            "net"         = resNet,
            "Zillmer"     = resZ,
            "adequate"    = resAdeq,
            "gamma"       = resGamma,
            "contractual" = resZ + resGamma,
            "conversion"  = resConversion,
            "alphaRefund" = alphaRefund,
            "reduction"   = resReduction
            #, "Reserve.premiumfree"=res.premiumfree, "Reserve.gamma.premiumfree"=res.gamma.premiumfree);
      );
      rownames(res) <- rownames(values$absPresentValues);
      values$reserves = res;

      # The surrender value functions can have arbitrary form, so we store a function
      # here in the tarif and call that, passing the reduction reserve as
      # starting point, but also all reserves, cash flows, premiums and present values
      if (!params$ContractState$surrenderPenalty) {
          # No surrender penalty any more (has already been applied to the first contract change!)
          surrenderValue = resReduction;
      } else if (!is.null(params$ActuarialBases$surrenderValueCalculation)) {
          surrenderValue = params$ActuarialBases$surrenderValueCalculation(resReduction, params, values);
      } else {
          # by default, refund the full reduction reserve, except the advance profit participation, which is also included in the reserve, but not charged on the premium!
          advanceProfitParticipationUnitCosts = 0;
          ppScheme      = params$ProfitParticipation$profitParticipationScheme;
          if (!is.null(ppScheme)) {
              advanceProfitParticipationUnitCosts = ppScheme$getAdvanceProfitParticipationAfterUnitCosts(params = params, values = values)
          }
          partnerRebate = valueOrFunction(params$Loadings$partnerRebate, params = params, values = values);
          surrenderValue = resReduction * (1 - advanceProfitParticipationUnitCosts - partnerRebate);
      }


      # Calculate new sum insured after premium waiver
      Storno = 0; # TODO: Implement storno costs
      premiumfreePV = (values$absPresentValues[, "benefits"] * securityFactor + values$absPresentValues[, "gamma_nopremiums"]); # PV of future premium free claims + costs
      newSI = ifelse(premiumfreePV == 0, 0,
        (surrenderValue - values$absPresentValues[,"death_Refund_past"] * securityFactor - c(Storno)) /
        premiumfreePV * params$ContractData$sumInsured);

      cbind(res,
            "PremiumsPaid" = Reduce("+", values$absCashFlows$premiums_advance, accumulate = TRUE),
            "Surrender" = surrenderValue,
            "PremiumFreeSumInsured" = newSI
      )
    },

    #' @description Calculate the (linear) interpolation factors for the balance
    #' sheet reserve (Dec. 31) between the yearly contract clowing dates
    #' @details Not to be called directly, but implicitly by the [InsuranceContract] object.
    #' @param years how many years to calculate (for some usances, the factor
    #'      is different in leap years!)
    getBalanceSheetReserveFactor = function(params, years = 1) {
      balanceDate = params$ActuarialBases$balanceSheetDate
      year(balanceDate) = year(params$ContractData$contractClosing);
      if (balanceDate < params$ContractData$contractClosing) {
        balanceDate = balanceDate + years(1);
      }

      contractDates = params$ContractData$contractClosing + years(1:years);
      balanceDates = balanceDate + years(1:years);

      if (params$ActuarialBases$balanceSheetMethod == "30/360") {
        baf = ((month(balanceDates + days(1)) - month(contractDates) - 1) %% 12 + 1) / 12
      } else if (params$ActuarialBases$balanceSheetMethod == "act/act") {
        baf = as.numeric((balanceDates + days(1)) - contractDates, units = "days" ) / as.numeric(balanceDates - (balanceDates - years(1)), units = "days")
      } else if (params$ActuarialBases$balanceSheetMethod == "act/360") {
        baf = pmin(as.numeric((balanceDates + days(1)) - contractDates, units = "days" ) / 360, 1)
      } else if (params$ActuarialBases$balanceSheetMethod == "act/365") {
        baf = pmin(as.numeric((balanceDates + days(1)) - contractDates, units = "days" ) / 365, 1)
      }
      baf
    },

    #' @description Calculate the reserves for the balance sheets at Dec. 31 of each
    #'              year by interpolation from the contract values calculated for
    #'              the yearly reference date of the contract
    #' @details Not to be called directly, but implicitly by the [InsuranceContract] object.
    reserveCalculationBalanceSheet = function(params, values) {
      reserves = values$reserves;
      years = length(reserves[,"Zillmer"]);
      # Balance sheet reserves:
      baf = self$getBalanceSheetReserveFactor(params, years = years);
      resZ_BS = (1 - baf) * reserves[,"Zillmer"] + baf * c(reserves[-1, "Zillmer"], 0);
      resGamma_BS = (1 - baf) * reserves[,"gamma"] + baf * c(reserves[-1, "gamma"], 0);
      res_BS = resZ_BS + resGamma_BS;

      # Premium transfer / unearned premium:
      bm = month(params$ContractData$contractClosing)
      freq = params$ContractData$premiumFrequency
      # TODO: We have no vector of actual written premiums (implicit assumption
      # seems to be that the premium stays constant!). Once we have such a vector,
      # rewrite the following code
      fact = (bm - 1) %% (12/freq) / 12 * freq
      unearnedPremiums = fact * values$cashFlows$premiums_advance * values$premiums[["written_beforetax"]] # TODO
      # If advance profit participation is granted, unearned premiums still apply to the whole gross premium without PP and partner rebate!
      ppScheme      = params$ProfitParticipation$profitParticipationScheme;
      if (!is.null(ppScheme)) {
          partnerRebate = valueOrFunction(params$Loadings$partnerRebate, params = params, values = values);
          advanceProfitParticipation = ppScheme$getAdvanceProfitParticipationAfterUnitCosts(params = params, values = values);
          unearnedPremiums = unearnedPremiums / (1 - partnerRebate - advanceProfitParticipation);
      }

      # Collect all reserves to one large matrix
      res = cbind("time" = baf + (1:years) - 1,
                  "Zillmer"               = pmax(resZ_BS,0),
                  "gamma"                 = pmax(resGamma_BS,0),
                  "Balance Sheet Reserve" = pmax(res_BS,0),
                  "unearned Premiums"     = unearnedPremiums
      );
      rownames(res) <- rownames(reserves);
      res
    },

    #' @description Calculate the profit participation given the contract
    #' parameters and the already calculated reserves of the contract.
    #'
    #' @details Not to be called directly, but implicitly by the [InsuranceContract] object.
    #' @param ... Additional parameters for the profit participation calculation, passed
    #'            through to the profit participation scheme's \href{../../LifeInsuranceContracts/html/ProfitParticipation.html#method-getProfitParticipation}{\code{ProfitParticipation$getProfitParticipation()}}
    calculateProfitParticipation = function(params, ...) {
        ppScheme = params$ProfitParticipation$profitParticipationScheme;
        if (!is.null(ppScheme)) {
            ppScheme$getProfitParticipation(params = params, ...)
        }
    },

    #' @description Calculate the reserves after profit participation for the given profit scenario
    #' @details Not to be called directly, but implicitly by the [InsuranceContract] object.
    #' @param profitScenario The ID of the profit scenario for which to calculate the reserves
    #' @param ... TODO
    reservesAfterProfit = function(profitScenario, params, values, ...) {
        # TODO
    },


    #' @description Return the time series of the basic contract
    #' @details Not to be called directly, but implicitly by the [InsuranceContract] object.
    getBasicDataTimeseries = function(params, values) {
        res = cbind(
            "PremiumPayment" = values$premiumComposition[, "charged"] > 0,
            "SumInsured" = values$reserves[, "SumInsured"],
            "Premiums" = values$absCashFlows$premiums_advance + values$absCashFlows$premiums_arrears,
            "InterestRate" = rep(params$ActuarialBases$i, values$int$l),
            "PolicyDuration" = rep(values$int$policyTerm, values$int$l),
            "PremiumPeriod" = rep(values$int$premiumTerm, values$int$l)
        );
        rownames(res) = 0:(values$int$l-1);
        res
    },

    #' @description Calculate the time series of the premium decomposition of the contract
    #' @details Not to be called directly, but implicitly by the [InsuranceContract] object.
    #'          All premiums, reserves and present values have already been calculated.
    premiumDecomposition = function(params, values) {
      loadings   = params$Loadings;
      sumInsured = params$ContractData$sumInsured;
      premiums   = values$premiums;
      v          = 1/(1 + params$ActuarialBases$i);
      l          = dim(values$reserves)[[1]];
      ppScheme   = params$ProfitParticipation$profitParticipationScheme;
      t          = as.character(0) # Time for original premium calculation => TODO: Use values stored in ContractData?

      # TODO: This assumes all premiums are paid in advance!
      premium.gross    = values$absCashFlows[,"premiums_advance"];

      # First get the charges and rebates that are added to the gross premium to obtain the charged premium:

      # charge for no medical exam:
      extraChargeGrossPremium = valueOrFunction(loadings$extraChargeGrossPremium, params = params, values = values);
      noMedExam        = valueOrFunction(loadings$noMedicalExam,params = params, values = values);
      noMedExam.rel    = valueOrFunction(loadings$noMedicalExamRelative,params = params, values = values);
      withMedExam      = premium.gross * (1 + noMedExam.rel + extraChargeGrossPremium) + noMedExam * sumInsured;
      charge.noMedicalExam = withMedExam - premium.gross;

      # sum rebate:
      sumRebate        = valueOrFunction(loadings$sumRebate,    params = params, values = values);
      extraRebate      = valueOrFunction(loadings$extraRebate,  params = params, values = values);
      afterSumRebate   = withMedExam - (sumRebate + extraRebate) * sumInsured; # calculate the charge as the difference, because we want a vector!
      rebate.sum       = afterSumRebate - withMedExam;

      # advance profit participation has two parts, one before and one after unit costs. Part 1:
      advanceProfitParticipation = 0;
      if (!is.null(ppScheme)) {
          advanceProfitParticipation = ppScheme$getAdvanceProfitParticipation(params = params, values = values)
      }
      afterProfit      = afterSumRebate * (1 - advanceProfitParticipation);
      profits.advance  = afterProfit - afterSumRebate;

      # unit costs
      unitCosts        = premiums[["unitcost"]];
      # unit costs are only charged if a premium is paid, so exclude all times with premium==0!
      afterUnitCosts   = afterProfit + (afterProfit != 0)*unitCosts;
      unitcosts        = afterUnitCosts - afterProfit;

      # advance profit participation, Part 2:
      advanceProfitParticipationUnitCosts = 0;
      if (!is.null(ppScheme)) {
          advanceProfitParticipationUnitCosts = ppScheme$getAdvanceProfitParticipationAfterUnitCosts(params = params, values = values)
      }
      afterProfit      = afterUnitCosts * (1 - advanceProfitParticipationUnitCosts);
      profits.advance  = profits.advance + afterProfit - afterUnitCosts;

      # premium rebate
      premiumRebate    = valueOrFunction(loadings$premiumRebate,params = params, values = values);
      afterPremiumRebate = afterUnitCosts * (1 - premiumRebate);
      rebate.premium   = afterPremiumRebate - afterUnitCosts;

      # partner rebate
      partnerRebate    = valueOrFunction(loadings$partnerRebate,params = params, values = values);
      afterPartnerRebate = afterUnitCosts * (1 - partnerRebate);
      rebate.partner   = afterPartnerRebate - afterUnitCosts;

      # value after all rebates
      afterRebates     = afterProfit + rebate.premium + rebate.partner;

      # premium frequency loading
      frequencyLoading = valueOrFunction(params$Loadings$premiumFrequencyLoading, params = params, values = values);

      afterFrequency   = afterRebates * (1 + frequencyLoading[[toString(params$ContractData$premiumFrequency)]]);
      charge.frequency = afterFrequency - afterRebates;

      # insurance tax
      taxRate          = valueOrFunction(loadings$tax,          params = params, values = values);
      premium.charged  = afterFrequency * (1 + taxRate);
      tax              = premium.charged - afterFrequency;


      # Gross premium = net + zillmeredAlpha + unzillmeredAlpha + beta + gamma premium
      unit.premiumCF   = ifelse(premiums[["gross"]] == 0, premium.gross * 0, premium.gross / premiums[["gross"]]);
      if (values$absPresentValues[t, "premiums.unit"] == 0) {
        premium.gamma    = 0
        premium.beta     = 0
        premium.alpha    = 0
        premium.alpha.Zillmer = 0
      } else {
        premium.gamma    = unit.premiumCF * values$absPresentValues[t, "gamma"] / values$absPresentValues[t, "premiums.unit"];
        premium.beta     = unit.premiumCF * values$absPresentValues[t, "beta"]  / values$absPresentValues[t, "premiums.unit"];
        premium.alpha    = unit.premiumCF * values$absPresentValues[t, "alpha"] / values$absPresentValues[t, "premiums.unit"];
        premium.alpha.Zillmer = unit.premiumCF * values$absPresentValues[t, "Zillmer"] / values$absPresentValues[t, "premiums.unit"];
      }
      premium.Zillmer  = unit.premiumCF * premiums[["Zillmer"]];
      premium.alpha.noZ = premium.alpha - premium.alpha.Zillmer; # ungezillmerter Teil der Abschlusskosten

      premium.net       = unit.premiumCF * premiums[["net"]];

      securityLoading   = valueOrFunction(params$Loadings$security, params = params, values = values);
      premium.risk.actual   = v * (values$absCashFlows[,"death"] - c(values$reserves[,"net"][-1], 0)) * pad0(values$transitionProbabilities$q, l);
      premium.risk.security = v * (values$absCashFlows[,"death"] * securityLoading) * pad0(values$transitionProbabilities$q, l);
      premium.risk          = premium.risk.actual + premium.risk.security;

      premium.risk.disease.actual   = v * (values$absCashFlows[,"disease_SumInsured"] - c(values$reserves[,"net"][-1], 0)) * pad0(values$transitionProbabilities$i, l);
      premium.risk.disease.security = v * (values$absCashFlows[,"disease_SumInsured"] * securityLoading) * pad0(values$transitionProbabilities$i, l);
      premium.risk.disease          = premium.risk.disease.actual + premium.risk.disease.security;
      premium.savings       = getSavingsPremium(
          values$reserves[,"net"], v = v,
          survival_advance = values$absCashFlows[,"survival_advance"] + values$absCashFlows[,"guaranteed_advance"],
          survival_arrears = values$absCashFlows[,"survival_arrears"] + values$absCashFlows[,"guaranteed_arrears"]
      )

      premium.Zillmer.risk.actual   = v * (values$absCashFlows[,"death"] - c(values$reserves[,"contractual"][-1], 0)) * pad0(values$transitionProbabilities$q, l);
      premium.Zillmer.risk.security = v * (values$absCashFlows[,"death"] * securityLoading) * pad0(values$transitionProbabilities$q, l);
      premium.Zillmer.risk          = premium.Zillmer.risk.actual + premium.Zillmer.risk.security;
      premium.Zillmer.risk.disease.actual   = v * (values$absCashFlows[,"disease_SumInsured"] - c(values$reserves[,"contractual"][-1], 0)) * pad0(values$transitionProbabilities$i, l);
      premium.Zillmer.risk.disease.security = v * (values$absCashFlows[,"disease_SumInsured"] * securityLoading) * pad0(values$transitionProbabilities$i, l);
      premium.Zillmer.risk.disease          = premium.Zillmer.risk.disease.actual + premium.Zillmer.risk.disease.security;


      premium.Zillmer.savings  = getSavingsPremium(
          values$reserves[,"contractual"], v = v,
          survival_advance = values$absCashFlows[,"survival_advance"] + values$absCashFlows[,"guaranteed_advance"],
          survival_arrears = values$absCashFlows[,"survival_arrears"] + values$absCashFlows[,"guaranteed_arrears"]
      )
      premium.Zillmer.amortization = getSavingsPremium(
              pmin(0, values$reserves[,"contractual"]), v = v
      )
      premium.Zillmer.actsavings = getSavingsPremium(
              pmax(0, values$reserves[,"contractual"]), v = v,
              survival_advance = values$absCashFlows[,"survival_advance"] + values$absCashFlows[,"guaranteed_advance"],
              survival_arrears = values$absCashFlows[,"survival_arrears"] + values$absCashFlows[,"guaranteed_arrears"]
      )

      res = cbind(
        "charged"         = premium.charged,
        "tax"             = tax,
        "loading.frequency" = charge.frequency,
        "rebate.premium"  = rebate.premium,
        "rebate.partner"  = rebate.partner,
        "unitcosts"       = unitcosts,
        "profit.advance"  = profits.advance,
        "rebate.sum"      = rebate.sum,
        "charge.noMedicalExam" = charge.noMedicalExam,
        "gross"           = premium.gross,

        "gamma"           = premium.gamma,
        "beta"            = premium.beta,
        "alpha"           = premium.alpha,
        "alpha.noZillmer" = premium.alpha.noZ,
        "alpha.Zillmer"   = premium.alpha.Zillmer,
        "Zillmer"         = premium.Zillmer,

        "net"                   = premium.net,

        "risk"                          = premium.risk,
        "premium.risk.actual"           = premium.risk.actual,
        "premium.risk.security"         = premium.risk.security,
        "risk.disease"                  = premium.risk.disease,
        "premium.risk.disease.actual"   = premium.risk.disease.actual,
        "premium.risk.disease.security" = premium.risk.disease.security,
        "savings"                       = premium.savings,

        "Zillmer.risk"                  =  premium.Zillmer.risk,
        "Zillmer.risk.actual"           = premium.Zillmer.risk.actual,
        "Zillmer.risk.security"         = premium.Zillmer.risk.security,
        "Zillmer.risk.disease"          = premium.Zillmer.risk.disease,
        "Zillmer.risk.disease.actual"   = premium.Zillmer.risk.disease.actual,
        "Zillmer.risk.disease.security" = premium.Zillmer.risk.disease.security,

        "Zillmer.savings"               = premium.Zillmer.savings,
        "Zillmer.amortization"          = premium.Zillmer.amortization,
        "Zillmer.savings.real"          = premium.Zillmer.actsavings
      )
      rownames(res) <- rownames(premiums);
      res
    },


    #' @description Generic function to calculate future sums of the values
    #' @param values The time series, for which future sums at all times are desired
    #' @param ... currently unused
    calculateFutureSums = function(values, ...) {
      rcumsum = function(vec) rev(cumsum(rev(vec)))
      apply(values, 2, rcumsum)
    },
    #' @description Calculate all present values for a given time series. The
    #' mortalities are taken from the contract's parameters.
    #' @param values The time series, for which future present values at all
    #'      times are desired
    #' @param ... currently unused
    calculatePresentValues = function(values, params) {
      len = dim(values)[1];
      q = self$getTransitionProbabilities(params);
      pv = function(vec) calculatePVSurvival(px = pad0(q$p, len), advance = vec, v = 1/(1 + params$ActuarialBases$i));
      apply(values, 2, pv)
    },




    #' @field dummy Dummy field to allow commas after the previous method
    dummy = 0
  )
)

