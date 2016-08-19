library(R6)

# base class for Profit Participation schemes  (holding contract-independent values and
# providing methods to calculate the profit participation values from the given
# reserves).
ProfitParticipation = R6Class(
  "ProfitParticipation",
  public  = list(
    name  = "Name des Gewinnplans",
    Parameters = ProfitParticipation.ParameterStructure,


    initialize = function(name = NULL, ...) {
      if (!missing(name))           self$name = name;
      self$setParameters(...);
      self$setFallbackParameters();
    },

    setParameters = function(...) {
        self$Parameters = fillFields(self$Parameters, list(...));
    },

    setFallbackParameters = function() {
        self$Parameters = fallbackFields(self$Parameters, list(
            advanceProfitParticipation = NULL,                # Vorweggewinnbeteiligung (%-Satz der Bruttoprämie)
            advanceProfitParticipationInclUnitCost = NULL,    # Vorweggewinnbeteiligung (%-Satz der Prämie mit Zu-/Abschlägen, insbesondere nach Stückkosten)

            guaranteedInterest = NULL,
            interestBonusRate = NULL,
            totalInterest = NULL,
            mortalityBonusRate = NULL,
            costBonusRate = NULL,
            terminalBonusRate = NULL,

            terminalBonusQuote = NULL,

            profitParticipationScheme = self
        ));
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




#
#     getProfitParticipation = function(...,
#       guaranteedInterest = 0,
#       interestBonusRate = totalInterest - guaranteedInterest,
#       totalInterest = guaranteedInterest + interestProfitRate,
#       mortalityBonusRate = c(0),
#       costBonusRate = c(0),
#       terminalBonusRate = c(0),
#       reserves = c(),
#       premiums = c(),
#       sumInsured = 0
#     ) {
#       if (missing(interestProfitRate) && missing(totalInterest)) {
#         # If neither total interest nor interest profit rate is given, set interest profit to 0.
#         # In all other cases, one can be calculated from the other
#         interestProfitRate = c(0);
#         totalInterest = c(guaranteedInterest);
#       }
#
#       # TODO
#
#       profit=data.frame()
#     },
#
#     InterestOnProfit = function(t, reserve, ..., )



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
