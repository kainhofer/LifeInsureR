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

    premiumComposition = NA,


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
      self$transitionProbabilities = do.call(self$tarif$getTransitionProbabilities, self$params);
      self$transitionProbabilities
    },

    determineCashFlows = function() {
      self$cashFlowsBasic = do.call(self$tarif$getBasicCashFlows, self$params);
      self$cashFlows = do.call(self$tarif$getCashFlows, c(self$params, "basicCashFlows" = self$cashFlowsBasic));
      self$premiumSum = sum(self$cashFlows$premiums_advance + self$cashFlows$premiums_arrears);
      self$cashFlowsCosts = do.call(self$tarif$getCashFlowsCosts, self$params);
      list("benefits"= self$cashFlows, "costs"=self$cashFlowCosts, "premiumSum" = self$premiumSum)
    },

    calculatePresentValues = function() {
      self$presentValues = do.call(self$tarif$presentValueCashFlows,
                                   c(list("cashflows"=self$cashFlows), self$params));
      self$presentValuesCosts = do.call(self$tarif$presentValueCashFlowsCosts,
                                        c(list("cashflows"=self$cashFlowsCosts), self$params));
      list("benefits" = self$presentValues, "costs" = self$presentValuesCosts)
    },

    calculatePremiums = function() {
      # the premiumCalculation function returns the premiums AND the cofficients,
      # so we have to extract the coefficients and store them in a separate variable
      res = do.call(self$tarif$premiumCalculation,
                    c(list(pvBenefits=self$presentValues,
                      pvCosts=self$presentValuesCosts,
                      premiumSum = self$premiumSum),
                      self$params));
      self$premiumCoefficients = res[["coefficients"]];
      self$premiums = res[["premiums"]]
      self$premiums
    },

    updatePresentValues = function() {
      pvAllBenefits = do.call(self$tarif$presentValueBenefits,
                              c(list(presentValues = self$presentValues,
                                     presentValuesCosts = self$presentValuesCosts,
                                     premiums = self$premiums,
                                     premiumSum = self$premiumSum),
                                self$params));
      self$presentValues = cbind(self$presentValues, pvAllBenefits)
      self$presentValue
    },

    calculateAbsCashFlows = function() {
      absCashFlows = do.call(self$tarif$getAbsCashFlows,
                             c(list(premiums = self$premiums,
                                    premiumSum = self$premiumSum,
                                    cashflows = self$cashFlows,
                                    cashflowsCosts = self$cashFlowsCosts),
                               self$params));
      self$absCashFlows = absCashFlows
      self$absCashFlows
    },

    calculateAbsPresentValues = function() {
      absPresentValues = do.call(self$tarif$getAbsPresentValues,
                                 c(list(premiums = self$premiums,
                                        premiumSum = self$premiumSum,
                                        presentValues = self$presentValues,
                                        presentValuesCosts = self$presentValuesCosts),
                                   self$params));
      self$absPresentValues = absPresentValues
      self$absPresentValues
    },

    calculateReserves = function() {
      self$reserves = do.call(self$tarif$reserveCalculation,
                              c(list(premiums=self$premiums,
                                presentValues=self$absPresentValues,
                                cashflows = self$absCashFlows,
                                premiumSum = self$premiumSum),
                                self$params));
      self$reserves
    },

    premiumAnalysis = function() {
      self$premiumComposition = do.call(self$tarif$premiumDecomposition,
                                        c(list(premiums=self$premiums,
                                               reserves=self$reserves,
                                               cashflows=self$absCashFlows,
                                               presentValues=self$absPresentValues,
                                               q=self$transitionProbabilities),
                                          self$params));
      self$premiumComposition
    },

    dummy=NULL
  )
);

