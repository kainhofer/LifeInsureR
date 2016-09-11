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
        str(1/(1.07) ^ ((n-1):0))
        terminalBonusAccount * 1/(1.07) ^ ((n-1):0)
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
# browser();
        terminalBonusReserves = self$getTerminalBonusReserves(rates, terminalBonus, terminalBonusAccount, params = params, values = values)


        res = cbind(
            # Profit Calculation Bases
            interestBase = intBase,
            riskBase = riskBase,
            expenseBase = expenseBase,
            sumBase = sumBase,
            terminalBase = terminalBase,

            # Profit Rates
            guaranteedInterest = rates$guaranteedInterest,
            totalInterest = rates$totalInterest,
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
            terminalBonusReserve = terminalBonusReserves,

            # Profit included in various benefits
            survival = 0,

            deathAccrued = 0,
            deathTerminalBonus = 0,
            death = 0,

            surrenderAccrued = 0,
            surrenderTerminalBonus = 0,
            surrender = 0,

            premiumWaiverAccrued = 0,
            premiumWaiverTerminalBonux = 0,
            premiumWaiver = 0
        );
        prev = 0;
        for (i in 1:nrow(res)) {
#            res[i,"previousProfit"]   = prev;
            res[i,"interestOnProfit"] = res[i,"interestOnProfitRate"] * prev;
            res[i,"totalProfitAssignment"] = res[i, "componentsProfit"] + res[i,"interestOnProfit"];
            res[i,"totalProfit"] = prev + res[i,"totalProfitAssignment"];
            prev = res[i,"totalProfit"];

#            # The totalProfit is initialized with all profits of the current year!
#            res[i,"totalProfit"]      = prev + res[i,"totalProfit"] + res[i,"interestOnProfit"];
#            prev = res[i, "totalProfit"];
#            res[i,"terminalBonus"]    = self$calculateTerminalBonus(t = i, bonusPrevious = res[i-1,"terminalBonus"], profits = res[i,])
        }

        res
    },



   # Dummy to allow commas
    dummy = 0
  )
)

# Generali.ProfitParticipation.Erleben.v1 = R6Class ("ProfitParticipation",
#   inherit = ProfitParticipation,
#   public = list(
#     name = "Gewinnplan für Erlebensversicherungen und aufgeschobene Rentenversicherungen per 31.12.2006, Version 1"
#
#   )
# );
#
