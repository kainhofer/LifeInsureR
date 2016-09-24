#' @include HelperFunctions.R InsuranceParameters.R
#'
#' @import R6
NULL


#' @export
filterProfitRates = function(rates, classes) {
    filter(.data = rates, profitClass %in% classes)
}


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

    setupRates = function(params, values, ...) {
        # 1) Profit scheme or contract provides general company-wide profit rates for some years:
        #       profitRates
        # 2) Contract can override individual rates (either for calendar years or contract years):
        #       guaranteedInterest, interestProfitRate, totalInterest, mortalityProfitRate,
        #       expenseProfitRate, sumProfitRate, terminalBonusRate, terminalBonusQuote
        # 3) Explicit function arguments (either for calendar years or contract years).
        # 4) Any missing values will be taken from the last given year
#
# params = contract.U3.1_2015$Parameters;
# values = contract.U3.1_2015$Values;
# browser()
        startYear = year(params$ContractData$contractClosing);
        policyPeriod = params$ContractData$policyPeriod;
policyPeriod = 10;
        years = startYear:(startYear + policyPeriod);

        columns = c(
            "year",
            "guaranteedInterest", "interestProfitRate", "totalInterest",
            "mortalityProfitRate", "expenseProfitRate", "expenseProfitRate_premiumfree",
            "sumProfitRate",
            "terminalBonusRate", "terminalBonusQuote")
        rates = data.frame(matrix(ncol = length(columns), nrow = length(years), dimnames = list(years = years, rates = columns)))

        # profitRates are general company-wide tables with all default rates
        # => use them as default, unless overridden
        defrates = params$ProfitParticipation$profitRates
        if (!is.null(defrates)) {
#             profclass = params$ProfitParticipation$profitClass
# # profclass="B10"
#             defrates = filter(defrates, profitClass == profclass)
#             rownames(defrates) = defrates$year
#
#             # Use last year before the startYear as a fallback if startYear is not given
#             if (!startYear %in% defrates$year) {
#                 # TODO
#                 # max(defrates$year[(defrates$year <= startYear)])
#             }
#
#             defrates$year
#             # if (max(d))

        }
        allrates = defrates

        # 2) Add the explicit overrides per profit rate (from the contract)
        for (col in columns) {
            if (!is.null(params$ProfitParticipation[[col]])) {
                rt = valueOrFunction(params$ProfitParticipation[[col]], params = params, values = values);
                if (is.null(names(rt))) {
                    # numeric value or vector => assume values from the contract start year
                    rates[as.character(years), col] = padLast(rt, length(years));
                } else {
                    # values with years assigned => use them only for the given year
                    rates[names(rt), col] = rt;
                    rates[names(rt), "year"] = names(rt);
                }
            }
        }

        # 3) Use explicit function param overrides per profit rate (from this function call)
        for (col in columns) {
            str(col)
            rt = match.arg(arg = col);
str(rt)
            if (!is.null(rt)) {
                rt = valueOrFunction(rt, params = params, values = values);
                if (is.null(names(rt))) {
                    # numeric value or vector => assume values from the contract start year
                    rates[as.character(years), col] = padLast(rt, length(years));
                } else {
                    # values with years assigned => use them only for the given year
                    rates[names(rt), col] = rt;
                    rates[names(rt), "year"] = names(rt);
                }
                str(rates)
            }
            #             str(
        }
        #
# str(defrates$profitClass)
#         # TODO: extract default
#
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



    getProfitParticipation = function(params, values, ...) {
        rates        = self$setupRates(params = params, values = values, ...)

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
