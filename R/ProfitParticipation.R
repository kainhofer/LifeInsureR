#' @include HelperFunctions.R InsuranceParameters.R ProfitParticipation_Functions.R
#'
#' @import R6
#' @import dplyr
NULL


#' @export
filterProfitRates = function(rates, classes) {
    dplyr::filter(.data = rates, profitClass %in% classes)
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

    Functions = list(

        #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        # Calculation bases for the various types of profit
        # Can / shall be overridden in child classes that use other bases!
        #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        getInterestProfitBase = profPart.base.meanContractualReserve,
        getRiskProfitBase     = profPart.base.ZillmerRiskPremium,
        getExpenseProfitBase  = profPart.base.sumInsured,
        getSumProfitBase      = profPart.base.sumInsured,
        getTerminalBonusBase  = profPart.base.sumInsured,

        #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        # Profit rates for the various types of profit
        # Can / shall be overridden in child classes that use other schemes!
        #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        getInterestProfitRate = function(rates, params, values) {
            rates$interestProfitRate
        },
        getRiskProfitRate = function(rates, params, values) {
            rates$mortalityProfitRate
        },
        getExpenseProfitRate = function(rates, params, values) {
            rates$expenseProfitRate
        },
        getSumProfitRate = function(rates, params, values) {
            rates$sumProfitRate
        },
        getTerminalBonusRate = function(rates, params, values) {
            rates$terminalBonusRate
        },

        getInterestOnProfits = profPart.rate.totalInterest,


        getTerminalBonusReserves = function(rates, terminalBonus, terminalBonusAccount, params, values) {
            n = length(terminalBonusAccount)
            terminalBonusAccount * 1/(1.07) ^ ((n - 1):0)
        },

        calculateInterestProfit = profPart.calculate.RateOnBase,
        calculateRiskProfit = profPart.calculate.RateOnBase,
        calculateExpenseProfit = profPart.calculate.RateOnBase,
        calculateSumProfit = profPart.calculate.RateOnBase,

        calculateTerminalBonus = profPart.calculate.RateOnBase,

        #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        # Calculations of the assigned profit amounts, based on the bases and
        # rates defined with the functions above.
        # Can / shall be overridden in child classes that use other bases!
        #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

        calculateSurvivalBenefit = function(profits, rates, params, values) {
            profits[,"totalProfit"] + profits[,"terminalBonusReserve"]
        },
        calculateDeathBenefitAccrued = function(profits, rates, params, values) {
            profits[,"totalProfit"]*(1 + rates$guaranteedInterest)
        },
        calculateDeathBenefitTerminal = function(profits, rates, params, values) {
            n = params$ContractData$policyPeriod;
            profits[, "terminalBonusReserve"] * (0:n)/n * ((0:n) >= max(10, n - 5))
        },
        calculateSurrenderBenefitAccrued = function(profits, rates, params, values) {
            profits[,"totalProfit"]*(1 + rates$guaranteedInterest / 2)
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

        dummy = 0
    ),


    initialize = function(name = NULL, ...) {
      if (!missing(name))           self$name = name;
      self$setParameters(...);
      self$setFunctions(...);
      self$setFallbackParameters();
    },

    setParameters = function(...) {
        self$Parameters = fillFields(self$Parameters, list(...));
    },

    setFunctions = function(...) {
        self$Functions = fillFields(self$Functions, list(...));
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


    setupRates = function(params, values, ...) {
        # 1) Profit scheme or contract provides general company-wide profit rates for some years:
        #       profitRates
        # 2) Contract can override individual rates (either for calendar years or contract years):
        #       guaranteedInterest, interestProfitRate, totalInterest, mortalityProfitRate,
        #       expenseProfitRate, sumProfitRate, terminalBonusRate, terminalBonusQuote
        # 3) Explicit function arguments (either for calendar years or contract years).
        # 4) Any missing values will be taken from the last given year
        startYear = year(params$ContractData$contractClosing);
        policyPeriod = params$ContractData$policyPeriod;
        years = startYear:(startYear + policyPeriod);

        columns = c(
            "year",
            "guaranteedInterest", "interestProfitRate", "totalInterest", "interestProfitRate2", "totalInterest2",
            "mortalityProfitRate", "expenseProfitRate", "expenseProfitRate_premiumfree",
            "sumProfitRate",
            "terminalBonusRate", "terminalBonusQuote")
        rates = data.frame(matrix(ncol = length(columns), nrow = length(years), dimnames = list(years = years, rates = columns)))
        rates$year = years;

        # 1) profitRates are general company-wide tables with all default rates
        # => use them as default, unless overridden
        defrates = params$ProfitParticipation$profitRates
        if (!is.null(defrates)) {
            profclass = params$ProfitParticipation$profitClass
            defrates = dplyr::filter(defrates, profitClass == profclass)
            rownames(defrates) = defrates$year

            defcols = intersect(columns, colnames(defrates))

            rates[rownames(defrates), defcols] = defrates[, defcols]
        }

        # 2) Add the explicit overrides per profit rate (from the contract)
        for (col in columns) {
            if (!is.null(params$ProfitParticipation[[col]])) {
                rt = valueOrFunction(params$ProfitParticipation[[col]], params = params, values = values);
                if (is.null(names(rt))) {
                    # numeric value or vector => assume values from the contract start year
                    # and fill ALL policy years with the given rates (last entry repeated)
                    rates[as.character(years), col] = padLast(rt, length(years));
                } else {
                    # values with years assigned => use them only for the given year(s), don't override any other year
                    rates[names(rt), col] = rt;
                    rates[names(rt), "year"] = names(rt);
                }
            }
        }

        # 3) Use explicit function param overrides per profit rate (from this function call)
        argcols = match.call(expand.dots = FALSE)$`...`;
        matching.cols = intersect(columns, names(argcols))
        for (col in matching.cols) {
            rt = eval(argcols[[col]]);
            if (!is.null(rt)) {
                rt = valueOrFunction(rt, params = params, values = values);
                if (is.null(names(rt))) {
                    # numeric value or vector => assume values from the contract start year
                    # and fill ALL policy years with the given rates (last entry repeated)
                    rates[as.character(years), col] = padLast(rt, length(years));
                } else {
                    # values with years assigned => use them only for the given year(s), don't override any other year
                    rates[names(rt), col] = rt;
                    rates[names(rt), "year"] = names(rt);
                }
            }
        }
        rownames(rates) = rates[, "year"]

        # 4) Fill all NAs with the last known value
        # First, make sure that all entries are in the correct order (sorted by year)
        rates
        newrates = rates %>% dplyr::arrange(year) %>% dplyr::mutate_all(fillNAgaps)
        rownames(newrates) = newrates$year

        # 5) Replace all NA values with 0, so we don't run into any problems later on
        newrates[is.na(newrates)] <- 0

        # TODO: Fix guaranteedInterest + interestProfitRate = totalInterest, where one of them might be missing!

        # Return only the policy years...
        self$adjustRates(newrates[as.character(years),], params = params, values = values)
    },

    adjustRates = function(rates, params, values) {
        rates[1,] = 0;
        rates
    },





    getProfitParticipation = function(params, values, ...) {
        waiting      = valueOrFunction(params$ProfitParticipation$waitingPeriod, params = params, values = values);
        if (is.numeric(waiting) && waiting > 0) {
            waitingFactor = c(rep(0, waiting + 1), rep(1, params$ContractData$policyPeriod - waiting));
        } else {
            waitingFactor = 1;
        }

        rates        = self$setupRates(params = params, values = values, ...)

        intBase      = self$Functions$getInterestProfitBase(params = params, values = values);
        riskBase     = self$Functions$getRiskProfitBase(params = params, values = values);
        expenseBase  = self$Functions$getExpenseProfitBase(params = params, values = values);
        sumBase      = self$Functions$getSumProfitBase(params = params, values = values);

        intRate      = self$Functions$getInterestProfitRate(rates, params = params, values = values);
        riskRate     = self$Functions$getRiskProfitRate(rates, params = params, values = values);
        expenseRate  = self$Functions$getExpenseProfitRate(rates, params = params, values = values);
        sumRate      = self$Functions$getSumProfitRate(rates, params = params, values = values);

        intProfit     = self$Functions$calculateInterestProfit(base = intBase, rate = intRate, waiting = waitingFactor, params = params, values = values);
        riskProfit    = self$Functions$calculateInterestProfit(base = riskBase, rate = riskRate, waiting = waitingFactor, params = params, values = values);
        expenseProfit = self$Functions$calculateInterestProfit(base = expenseBase, rate = expenseRate, waiting = waitingFactor, params = params, values = values);
        sumProfit     = self$Functions$calculateInterestProfit(base = sumBase, rate = sumRate, waiting = waitingFactor, params = params, values = values);

        interestOnProfitRate = self$Functions$getInterestOnProfits(rates, params = params, values = values);



        res = cbind(
            # Profit Calculation Bases
            interestBase = c(intBase),
            riskBase = c(riskBase),
            expenseBase = c(expenseBase),
            sumBase = c(sumBase),

            # Profit Rates
            guaranteedInterest = c(rates$guaranteedInterest),
            totalInterest = c(rates$totalInterest),
            interestProfitRate = c(intRate),
            riskProfitRate = c(riskRate),
            expenseProfitRate = c(expenseRate),
            sumProfitRate = c(sumRate),
            interestOnProfitRate = c(interestOnProfitRate),

            # Profit components
            interestProfit = c(intProfit),
            riskProfit = c(riskProfit),
            expenseProfit = c(expenseProfit),
            sumProfit = c(sumProfit),
            componentsProfit = c(intProfit + riskProfit + expenseProfit + sumProfit),
            interestOnProfit = c(0),
            totalProfitAssignment = c(0),

            totalProfit = c(0)
        );
        prev = 0;
        for (i in 1:nrow(res)) {
            res[i,"interestOnProfit"] = res[i,"interestOnProfitRate"] * prev;
            res[i,"totalProfitAssignment"] = res[i, "componentsProfit"] + res[i,"interestOnProfit"];
            res[i,"totalProfit"] = prev + res[i,"totalProfitAssignment"];
            prev = res[i,"totalProfit"];
        }


        #### Terminal Bonus calculations (might depend on the individual profit assignments calculated above!
        #### => TODO: Pass the current profit calculation inside the values!)
        terminalBase = self$Functions$getTerminalBonusBase(params = params, values = values);
        terminalRate = self$Functions$getTerminalBonusRate(rates, params = params, values = values);
        terminalBonus = terminalBase * terminalRate; # TODO: Add the AF(v) factor!
        terminalBonusAccount = cumsum(terminalBonus)
        terminalBonusReserves = self$Functions$getTerminalBonusReserves(rates, terminalBonus, terminalBonusAccount, params = params, values = values)
        res = cbind(
            res,
            # Terminal Bonus values
            terminalBase = c(terminalBase),
            terminalBonusRate = c(terminalRate),
            terminalBonus = c(terminalBonus),
            terminalBonusAccount = c(terminalBonusAccount),
            terminalBonusReserve = c(terminalBonusReserves)
        )


        survival       = self$Functions$calculateSurvivalBenefit(res, rates = rates, params = params, values = values);

        deathAccrued   = self$Functions$calculateDeathBenefitAccrued(res, rates = rates, params = params, values = values);
        deathTerminalBonus = self$Functions$calculateDeathBenefitTerminal(res, rates = rates, params = params, values = values);

        surrenderAccrued  = self$Functions$calculateSurrenderBenefitAccrued(res, rates = rates, params = params, values = values);
        surrenderTerminalBonus = self$Functions$calculateSurrenderBenefitTerminal(res, rates = rates, params = params, values = values);

        premiumWaiverAccrued  = self$Functions$calculatePremiumWaiverBenefitAccrued(res, rates = rates, params = params, values = values);
        premiumWaiverTerminalBonus = self$Functions$calculatePremiumWaiverBenefitTerminal(res, rates = rates, params = params, values = values);

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
