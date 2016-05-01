library(R6)
library(openxlsx);
# require(xlsx)

InsuranceContract = R6Class(
  "InsuranceContract",
  public = list(
    tarif = NA,

    #### Contract settings
    params = list(
      sumInsured = 1,
      YOB = NA,
      age = NA,
      policyPeriod = Inf,
      premiumPeriod = 1,
      deferral = 0,
      guaranteed = 0,

      premiumPayments = PaymentTimeEnum("in advance"),
      benefitPayments = PaymentTimeEnum("in advance"),

      premiumFrequency = 1,
      benefitFrequency = 1, # Only for annuities!

      loadings = list()     # Allow overriding the tariff-defined loadings (see the InsuranceTariff class for all possible names)
    ),

    #### Caching values for this contract, initialized/calculated when the object is created
    values = list(
      transitionProbabilities = NA,

      cashFlowsBasic = NA,
      cashFlows = NA,
      cashFlowsCosts = NA,
      premiumSum = 0,

      presentValues = NA,
      presentValuesCosts = NA,

      premiumCoefficients = NA,
      premiums = NA,
      absCashFlows = NA,
      absPresentValues = NA,

      reserves = NA,

      premiumComposition = NA
    ),

    #### Keeping the history of all contract changes during its lifetime
    history = list(

    ),


    #### The code:

    initialize = function(tarif, age, policyPeriod,
                          premiumPeriod = policyPeriod, sumInsured = 1,
                          ...,
                          loadings = list(),
                          guaranteed = 0,
                          premiumPayments = "in advance", benefitPayments = "in advance",
                          premiumFrequency = 1, benefitFrequency = 1,
                          deferral = 0, YOB = 1975) {
      self$tarif = tarif;
      self$params$age = age;
      self$params$policyPeriod = policyPeriod;
      if (missing(premiumPeriod) && !is.null(self$tarif$defaultPremiumPeriod)) {
        self$params$premiumPeriod = self$tarif$defaultPremiumPeriod;
      } else {
        self$params$premiumPeriod = premiumPeriod;
      }
      self$params$sumInsured = sumInsured;
      if (!missing(deferral))         self$params$deferral = deferral;
      if (!missing(YOB))              self$params$YOB = YOB;
      if (!missing(premiumPayments))  self$params$premiumPayments = premiumPayments;
      if (!missing(benefitPayments))  self$params$benefitPayments = benefitPayments;
      if (!missing(premiumFrequency)) self$params$premiumFrequency = premiumFrequency;
      if (!missing(benefitFrequency)) self$params$benefitFrequency = benefitFrequency;
      if (!missing(guaranteed))       self$params$guaranteed = guaranteed;
      if (!missing(loadings))         self$params$loadings = loadings;

      self$recalculate();
    },

    addHistorySnapshot = function(time=0, comment="Initial contract values", type="Contract", params=self$params, values = self$values) {
      self$history = c(self$history,
                       list("time"=time, "comment"=comment, "type"=type, "params"=params, "values"=values));
    },

    recalculate = function() {
      self$determineTransitionProbabilities();
      self$determineCashFlows();
      self$calculatePresentValues();
      self$calculatePremiums();
      self$updatePresentValues(); # Update the cash flows and present values with the values of the premium

      self$calculateAbsCashFlows();
      self$calculateAbsPresentValues();
      self$calculateReserves();
      self$premiumAnalysis();
    },


    determineTransitionProbabilities = function() {
      self$values$transitionProbabilities = do.call(self$tarif$getTransitionProbabilities, self$params);
      self$values$transitionProbabilities
    },

    determineCashFlows = function() {
      self$values$cashFlowsBasic = do.call(self$tarif$getBasicCashFlows, self$params);
      self$values$cashFlows = do.call(self$tarif$getCashFlows, c(self$params, "basicCashFlows" = self$values$cashFlowsBasic));
      self$values$premiumSum = sum(self$values$cashFlows$premiums_advance + self$values$cashFlows$premiums_arrears);
      self$values$cashFlowsCosts = do.call(self$tarif$getCashFlowsCosts, self$params);
      list("benefits"= self$values$cashFlows, "costs"=self$values$cashFlowCosts, "premiumSum" = self$values$premiumSum)
    },

    calculatePresentValues = function() {
      self$values$presentValues = do.call(self$tarif$presentValueCashFlows,
                                   c(list("cashflows"=self$values$cashFlows), self$params));
      self$values$presentValuesCosts = do.call(self$tarif$presentValueCashFlowsCosts,
                                        c(list("cashflows"=self$values$cashFlowsCosts), self$params));
      list("benefits" = self$values$presentValues, "costs" = self$values$presentValuesCosts)
    },

    calculatePremiums = function() {
      # the premiumCalculation function returns the premiums AND the cofficients,
      # so we have to extract the coefficients and store them in a separate variable
      res = do.call(self$tarif$premiumCalculation,
                    c(list(pvBenefits=self$values$presentValues,
                      pvCosts=self$values$presentValuesCosts,
                      premiumSum = self$values$premiumSum),
                      self$params));
      self$values$premiumCoefficients = res[["coefficients"]];
      self$values$premiums = res[["premiums"]]
      self$values$premiums
    },

    updatePresentValues = function() {
      pvAllBenefits = do.call(self$tarif$presentValueBenefits,
                              c(list(presentValues = self$values$presentValues,
                                     presentValuesCosts = self$values$presentValuesCosts,
                                     premiums = self$values$premiums,
                                     premiumSum = self$values$premiumSum),
                                self$params));
      self$values$presentValues = cbind(self$values$presentValues, pvAllBenefits)
      self$values$presentValue
    },

    calculateAbsCashFlows = function() {
      absCashFlows = do.call(self$tarif$getAbsCashFlows,
                             c(list(premiums = self$values$premiums,
                                    premiumSum = self$values$premiumSum,
                                    cashflows = self$values$cashFlows,
                                    cashflowsCosts = self$values$cashFlowsCosts),
                               self$params));
      self$values$absCashFlows = absCashFlows
      self$values$absCashFlows
    },

    calculateAbsPresentValues = function() {
      absPresentValues = do.call(self$tarif$getAbsPresentValues,
                                 c(list(premiums = self$values$premiums,
                                        premiumSum = self$values$premiumSum,
                                        presentValues = self$values$presentValues,
                                        presentValuesCosts = self$values$presentValuesCosts),
                                   self$params));
      self$values$absPresentValues = absPresentValues
      self$values$absPresentValues
    },

    calculateReserves = function() {
      self$values$reserves = do.call(self$tarif$reserveCalculation,
                              c(list(premiums=self$values$premiums,
                                presentValues=self$values$absPresentValues,
                                cashflows = self$values$absCashFlows,
                                premiumSum = self$values$premiumSum),
                                self$params));
      self$values$reserves
    },

    premiumAnalysis = function() {
      self$values$premiumComposition = do.call(self$tarif$premiumDecomposition,
                                        c(list(premiums=self$values$premiums,
                                               reserves=self$values$reserves,
                                               cashflows=self$values$absCashFlows,
                                               presentValues=self$values$absPresentValues,
                                               q=self$values$transitionProbabilities),
                                          self$params));
      self$values$premiumComposition
    },

    dummy=NULL
  )
);

