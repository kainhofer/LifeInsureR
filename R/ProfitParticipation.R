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

    getInterestProfitBase = function (params, values) {
        pmax(0, rollingmean(c(0, values$reserves[,"contractual"])))
    },
    getRiskProfitBase = function (params, values) {
        values$premiumComposition[,"Zillmer.risk"]
    },
    getCostProfitBase = function (params, values) {
        params$ContractData$sumInsured
    },
    getSumProfitBase = function (params, values) {
        params$ContractData$sumInsured
    },

    getInterestProfitRate = function (rates, params, values) {
        rates$interest
    },
    getRiskProfitRate = function (rates, params, values) {
        rates$risk
    },
    getCostProfitRate = function (rates, params, values) {
        rates$cost
    },
    getSumProfitRate = function (rates, params, values) {
        rates$sum
    },


    getInterestRateProfits = function (rates, params, values) {
        rates$totalInterest
    },


    calculateTerminalBonus = function(t=1, bonusPrevious, profits) {
        # TODO
    },



    getProfitParticipation = function(rates, params, values) {
        intBase  = self$getInterestProfitBase(params=params, values=values);
        riskBase = self$getRiskProfitBase(params=params, values=values);
        costBase = self$getCostProfitBase(params=params, values=values);
        sumBase  = self$getSumProfitBase(params=params, values=values);

        intRate  = self$getInterestProfitRate(rates, params=params, values=values);
        riskRate = self$getRiskProfitRate(rates, params=params, values=values);
        costRate = self$getCostProfitRate(rates, params=params, values=values);
        sumRate  = self$getSumProfitRate(rates, params=params, values=values);

        intProfit  = intBase  * intRate;
        riskProfit = riskBase * riskRate;
        costProfit = costBase * costRate;
        sumProfit  = sumBase  * sumRate;

        interestOnProfitRate = self$getInterestRateProfits(rates, params=params, values=values);

        res = cbind(
            "previousProfit"   = c(0),
            "interestProfit"   = intProfit,
            "riskProfit"       = riskProfit,
            "costProfit"       = costProfit,
            "sumProfit"        = sumProfit,
            "interestOnProfit" = interestOnProfitRate,
            "totalProfit"      = intProfit + riskProfit + costProfit + sumProfit, # Temp value! Will be overwritten
            "terminalBonus"    = c(0),

            # All benefits are calculated for the already earned profits
            # survival benefits represent the value at time n when the contract ends
            "deathBenefit"     = c(0), # TODO
            "survivalBenefit"  = c(0), # TODO
            "surrenderValue"   = c(0)  # TODO
        );
        prev=0;
        for (i in 1:nrow(res)) {
            res[i,"previousProfit"]   = prev;
            res[i,"interestOnProfit"] = res[i,"interestOnProfit"] * prev;
            # The totalProfit is initialized with all profits of the current year!
            res[i,"totalProfit"]      = prev + res[i,"totalProfit"] + res[i,"interestOnProfit"];
            res[i,"terminalBonus"]    = self$calculateTerminalBonus(t=i, bonusPrevious=res[i-1,"terminalBonus"], profits=res[i,])
        }

        res
    },



   # Dummy to allow commas
    dummy = 0
  )
);

# Generali.ProfitParticipation.Erleben.v1 = R6Class ("ProfitParticipation",
#   inherit = ProfitParticipation,
#   public = list(
#     name="Gewinnplan für Erlebensversicherungen und aufgeschobene Rentenversicherungen per 31.12.2006, Version 1"
#
#   )
# );
#
