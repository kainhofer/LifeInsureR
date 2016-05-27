library(R6)

# base class for Profit Participation schemes  (holding contract-independent values and
# providing methods to calculate the profit participation values from the given
# reserves).
ProfitParticipation = R6Class(
  "ProfitParticipation",
  public  = list(
    name  = "Name des Gewinnplans",


    initialize = function(name = NULL) {
      if (!missing(name))           self$name = name;
    },

    getProfitParticipation = function(...,
      guaranteedInterest = 0,
      interestBonusRate = totalInterest - guaranteedInterest,
      totalInterest = guaranteedInterest + interestProfitRate,
      mortalityBonusRate = c(0),
      costBonusRate = c(0),
      terminalBonusRate = c(0),
      reserves = c(),
      premiums = c(),
      sumInsured = 0
    ) {
      if (missing(interestProfitRate) && missing(totalInterest)) {
        # If neither total interest nor interest profit rate is given, set interest profit to 0.
        # In all other cases, one can be calculated from the other
        interestProfitRate = c(0);
        totalInterest = c(guaranteedInterest);
      }

      # TODO

    },



    # Dummy to allow commas
    dummy = 0
  )
);
