#' @include HelperFunctions.R InsuranceParameters.R
#'
#' @import R6
NULL



#' Base Class for Profit Participation Schemes
#'
#' base class for Profit Participation schemes  (holding contract-independent values and
#' providing methods to calculate the profit participation values from the given
#' reserves).
#'
#' @export
ProfitParticipation = R6Class(
  "ProfitParticipation",
  public  = list(
    name  = "Name des Gewinnplans",
    Parameters = InsuranceContract.ParameterStructure$ProfitParticipation,


    initialize = function(name = NULL, ...) {
      if (!missing(name))           self$name = name;
      self$setParameters(...);
      self$setFallbackParameters();
    },

    setParameters = function(...) {
        self$Parameters = fillFields(self$Parameters, list(...));
    },

    setFallbackParameters = function() {
        self$Parameters = fallbackFields(self$Parameters, list(profitParticipationScheme = self));
        self$Parameters = fallbackFields(self$Parameters, InsuranceContract.ParameterDefaults$ProfitParticipation);
    },


    ############################################################################
    # Advance Profit Participation
    ############################################################################

    getAdvanceProfitParticipation = function(params, values, ...) {
        "@function getAdvanceProfitParticipation"
        "Return either one numerical value (constant for the whole premium payment period)"
        "of a vector of numerical values for the whole contract period "
        valueOrFunction(params$ProfitParticipation$advanceProfitParticipation, params, values, ...)
    },

    getAdvanceProfitParticipationAfterUnitCosts = function(params, values, ...) {
        "@function getAdvanceProfitParticipationAfterUnitCosts"
        "Return either one numerical value (constant for the whole premium payment period)"
        "of a vector of numerical values for the whole contract period "
        valueOrFunction(params$ProfitParticipation$advanceProfitParticipationInclUnitCost, params, values, ...)
    },


    ############################################################################
    # Traditional Profit participation:
    #   - Interest profit
    #   - Risk profit
    #   - Expense profit
    #   - Sum profit
    #   - Terminal bonus
    ############################################################################

    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # Calculation bases for the various types of profit
    # Can / shall be overridden in child classes that use other bases!
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    getInterestProfitBase = function(params, values) {
        # Rolling mean of the value for the current and previous year.
        pmax(0, rollingmean(c(0, values$reserves[,"contractual"])))
    },
    getRiskProfitBase = function(params, values) {
        # The risk premium of t=0 is used to determine the risk profit at time
        # t=1, so shift the whole vector!
        c(0, head(values$premiumComposition[,"Zillmer.risk"], -1))
    },
    getExpenseProfitBase = function(params, values) {
        params$ContractData$sumInsured
    },
    getSumProfitBase = function(params, values) {
        params$ContractData$sumInsured
    },
    getTerminalBonusBase = function(params, values) {
        params$ContractData$sumInsured
    },


    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # Profit rates for the various types of profit
    # Can / shall be overridden in child classes that use other schemes!
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    getInterestProfitRate = function(rates, params, values) {
        c(0, rates$interest)
    },
    getRiskProfitRate = function(rates, params, values) {
        c(0, rates$risk)
    },
    getExpenseProfitRate = function(rates, params, values) {
        c(0, rates$expense)
    },
    getSumProfitRate = function(rates, params, values) {
        c(0, rates$sum)
    },
    getTerminalBonusRate = function(rates, params, values) {
        c(0, rates$terminalBonus)
    },


    getInterestOnProfits = function(rates, params, values) {
        c(0, rates$totalInterest)
    },


    getTerminalBonusReserves = function(rates, terminalBonus, terminalBonusAccount, params, values) {
        n = length(terminalBonusAccount)
        terminalBonusAccount * 1/(1.07) ^ ((n - 1):0)
    },

    calculateSurvivalBenefit = function(profits, rates, params, values) {
        profits[,"totalProfit"] + profits[,"terminalBonusReserve"]
    },
    calculateDeathBenefitAccrued = function(profits, rates, params, values) {
        profits[,"totalProfit"]*(1 + c(0,rates$guaranteedInterest))
    },
    calculateDeathBenefitTerminal = function(profits, rates, params, values) {
        n = params$ContractData$policyPeriod;
        profits[, "terminalBonusReserve"] * (0:n)/n * ((0:n) >= max(10, n - 5))
    },

    calculateSurrenderBenefitAccrued = function(profits, rates, params, values) {
        profits[,"totalProfit"]*(1 + c(0,rates$guaranteedInterest) / 2)
    },
    calculateSurrenderBenefitTerminal = function(profits, rates, params, values) {
        n = params$ContractData$policyPeriod;
        profits[, "terminalBonusReserve"] * (0:n)/n * ((0:n) >= max(10, n - 5))
    },

    calculatePremiumWaiverBenefitAccrued = function(profits, rates, params, values) {
        profits[,"totalProfit"]
    },
    calculatePremiumWaiverBenefitTerminal = function(profits, rates, params, values) {
        n = params$ContractData$policyPeriod;
        profits[, "terminalBonusReserve"] * (0:n)/n * ((0:n) >= max(10, n - 5))
    },



    getProfitParticipation = function(rates, params, values) {
        intBase      = self$getInterestProfitBase(params = params, values = values);
        riskBase     = self$getRiskProfitBase(params = params, values = values);
        expenseBase  = self$getExpenseProfitBase(params = params, values = values);
        sumBase      = self$getSumProfitBase(params = params, values = values);
        terminalBase = self$getTerminalBonusBase(params = params, values = values);

        intRate      = self$getInterestProfitRate(rates, params = params, values = values);
        riskRate     = self$getRiskProfitRate(rates, params = params, values = values);
        expenseRate  = self$getExpenseProfitRate(rates, params = params, values = values);
        sumRate      = self$getSumProfitRate(rates, params = params, values = values);
        terminalRate = self$getTerminalBonusRate(rates, params = params, values = values);

        intProfit     = intBase  * intRate;
        riskProfit    = riskBase * riskRate;
        expenseProfit = expenseBase * expenseRate;
        sumProfit     = sumBase  * sumRate;

        interestOnProfitRate = self$getInterestOnProfits(rates, params = params, values = values);


        terminalBonus = terminalBase * terminalRate; # TODO: Add the AF(v) factor!
        terminalBonusAccount = cumsum(terminalBonus)
        terminalBonusReserves = self$getTerminalBonusReserves(rates, terminalBonus, terminalBonusAccount, params = params, values = values)


        res = cbind(
            # Profit Calculation Bases
            interestBase = intBase,
            riskBase = riskBase,
            expenseBase = expenseBase,
            sumBase = sumBase,
            terminalBase = terminalBase,

            # Profit Rates
            guaranteedInterest = c(0, rates$guaranteedInterest),
            totalInterest = c(0, rates$totalInterest),
            interestProfitRate = intRate,
            riskProfitRate = riskRate,
            expenseProfitRate = expenseRate,
            sumProfitRate = sumRate,
            terminalBonusRate = terminalRate,
            interestOnProfitRate = interestOnProfitRate,

            # Profit components
            interestProfit = intProfit,
            riskProfit = riskProfit,
            expenseProfit = expenseProfit,
            sumProfit = sumProfit,
            componentsProfit = intProfit + riskProfit + expenseProfit + sumProfit,
            interestOnProfit = 0,
            totalProfitAssignment = 0,

            totalProfit = 0,

            # Terminal Bonus values
            terminalBonus = terminalBonus,
            terminalBonusAcount = terminalBonusAccount,
            terminalBonusReserve = terminalBonusReserves

        );
        prev = 0;
        for (i in 1:nrow(res)) {
#            res[i,"previousProfit"]   = prev;
            res[i,"interestOnProfit"] = res[i,"interestOnProfitRate"] * prev;
            res[i,"totalProfitAssignment"] = res[i, "componentsProfit"] + res[i,"interestOnProfit"];
            res[i,"totalProfit"] = prev + res[i,"totalProfitAssignment"];
            prev = res[i,"totalProfit"];

        }

        survival       = self$calculateSurvivalBenefit(res, rates = rates, params = params, values = values);

        deathAccrued   = self$calculateDeathBenefitAccrued(res, rates = rates, params = params, values = values);
        deathTerminalBonus = self$calculateDeathBenefitTerminal(res, rates = rates, params = params, values = values);

        surrenderAccrued  = self$calculateSurrenderBenefitAccrued(res, rates = rates, params = params, values = values);
        surrenderTerminalBonus = self$calculateSurrenderBenefitTerminal(res, rates = rates, params = params, values = values);

        premiumWaiverAccrued  = self$calculatePremiumWaiverBenefitAccrued(res, rates = rates, params = params, values = values);
        premiumWaiverTerminalBonus = self$calculatePremiumWaiverBenefitTerminal(res, rates = rates, params = params, values = values);

        res = cbind(
            res,

            survival = survival,

            deathAccrued = deathAccrued,
            deathTerminalBonus = deathTerminalBonus,
            death = deathAccrued + deathTerminalBonus,

            surrenderAccrued  = surrenderAccrued,
            surrenderTerminalBonus = surrenderTerminalBonus,
            surrender      = surrenderAccrued + surrenderTerminalBonus,

            premiumWaiverAccrued  = premiumWaiverAccrued,
            premiumWaiverTerminalBonus = premiumWaiverTerminalBonus,
            premiumWaiver  = premiumWaiverAccrued + premiumWaiverTerminalBonus

        );

        res
    },



   # Dummy to allow commas
    dummy = 0
  )
)
