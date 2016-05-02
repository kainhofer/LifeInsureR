library(R6)
library(openxlsx);

InsuranceContract = R6Class("InsuranceContract",

  public = list(
    tarif = NA,

    #### Contract settings
    params = list(
      sumInsured = 1,
      premiumWaiver = 0,
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

      loadings = list(),     # Allow overriding the tariff-defined loadings (see the InsuranceTariff class for all possible names)
      surrenderPenalty = TRUE,  # Set to FALSE after the surrender penalty has been applied once, e.g. on a premium waiver
      alphaRefunded = FALSE     # Alpha costs not yet refunded (in case of contract changes)
    ),

    #### Caching values for this contract, initialized/calculated when the object is created
    values = list(
      basicData = NA,
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
    history = list(),


    #### The code:

    initialize = function(tarif, age, sumInsured = 1,
                          policyPeriod, premiumPeriod = policyPeriod, guaranteed = 0,
                          ...,
                          loadings = list(),
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

      self$calculateContract();
    },

    addHistorySnapshot = function(time=0, comment="Initial contract values", type="Contract", params=self$params, values = self$values) {
      self$history = rbind(self$history,
                       list(time=list("time"=time, "comment"=comment, "type"=type, "params"=params, "values"=values)));
    },

    calculateContract = function() {
      self$values$transitionProbabilities = self$determineTransitionProbabilities();

      self$values$cashFlowsBasic = self$determineCashFlowsBasic();
      self$values$cashFlows = self$determineCashFlows();
      self$values$premiumSum = self$determinePremiumSum();
      self$values$cashFlowsCosts = self$determineCashFlowsCosts();

      self$values$presentValues = self$calculatePresentValues();
      self$values$presentValuesCosts = self$calculatePresentValuesCosts();

      # the premiumCalculation function returns the premiums AND the cofficients,
      # so we have to extract the coefficients and store them in a separate variable
      res = self$calculatePremiums();
      self$values$premiumCoefficients = res[["coefficients"]];
      self$values$premiums = res[["premiums"]]

      # Update the cash flows and present values with the values of the premium
      pvAllBenefits = self$calculatePresentValuesBenefits()
      self$values$presentValues = cbind(self$values$presentValues, pvAllBenefits)

      self$values$absCashFlows = self$calculateAbsCashFlows();
      self$values$absPresentValues = self$calculateAbsPresentValues();
      self$values$reserves = self$calculateReserves();
      self$values$basicData = self$getBasicDataTimeseries()
      self$values$premiumComposition = self$premiumAnalysis();

      self$addHistorySnapshot(0, "Initial contract values", type="Contract", params=self$params, values = self$values);
    },

    determineTransitionProbabilities = function(contractModification=NULL) {
      do.call(self$tarif$getTransitionProbabilities, c(self$params, self$values, list(contractModification=contractModification)));
    },
    determineCashFlowsBasic = function(contractModification=NULL) {
      do.call(self$tarif$getBasicCashFlows, self$params);
    },
    determineCashFlows = function(contractModification=NULL) {
      do.call(self$tarif$getCashFlows, c(self$params, self$values, list(contractModification=contractModification)));
    },
    determinePremiumSum = function(contractModification=NULL) {
      sum(self$values$cashFlows$premiums_advance + self$values$cashFlows$premiums_arrears);
    },
    determineCashFlowsCosts = function(contractModification=NULL) {
      do.call(self$tarif$getCashFlowsCosts, c(self$params, self$values, list(contractModification=contractModification)));
    },
    calculatePresentValues = function(contractModification=NULL) {
      do.call(self$tarif$presentValueCashFlows, c(self$params, self$values, list(contractModification=contractModification)));
    },
    calculatePresentValuesCosts = function(contractModification=NULL) {
      do.call(self$tarif$presentValueCashFlowsCosts, c(self$params, self$values, list(contractModification=contractModification)));
    },
    calculatePremiums = function(contractModification=NULL) {
      do.call(self$tarif$premiumCalculation, c(self$params, self$values, list(contractModification=contractModification)));
    },
    calculatePresentValuesBenefits = function(contractModification=NULL) {
      do.call(self$tarif$presentValueBenefits, c(self$params, self$values, list(contractModification=contractModification)));
    },
    calculateAbsCashFlows = function(contractModification=NULL) {
      do.call(self$tarif$getAbsCashFlows, c(self$params, self$values, list(contractModification=contractModification)));
    },
    calculateAbsPresentValues = function(contractModification=NULL) {
      do.call(self$tarif$getAbsPresentValues, c(self$params, self$values, list(contractModification=contractModification)));
    },
    calculateReserves = function(contractModification=NULL) {
      do.call(self$tarif$reserveCalculation, c(self$params, self$values, list(contractModification=contractModification)));
    },
    premiumAnalysis = function(contractModification=NULL) {
      do.call(self$tarif$premiumDecomposition, c(self$params, self$values, list(contractModification=contractModification)));
    },
    getBasicDataTimeseries = function(contractModification=NULL) {
      do.call(self$tarif$getBasicDataTimeseries, c(self$params, self$values, list(contractModification=contractModification)));
    },

    # Premium Waiver: Stop all premium payments at time t
    # the SumInsured is determined from the available
    premiumWaiver = function (t) {
      newSumInsured = self$values$reserves[[toString(t), "PremiumFreeSumInsured"]];
      self$params$premiumWaiver = TRUE;
      self$params$surrenderPenalty = FALSE; # Surrencer penalty has already been applied, don't apply a second time
      self$params$alphaRefunded = TRUE;     # Alpha cost (if applicable) have already been refunded partially, don't refund again

      self$params$sumInsured = newSumInsured;

      self$values$cashFlowsBasic = mergeValues(starting=self$values$cashFlowsBasic, ending=self$determineCashFlowsBasic(t), t=t);
      self$values$cashFlows = mergeValues(starting=self$values$cashFlows, ending=self$determineCashFlows(t), t=t);
      # Premium sum is not affected by premium waivers, i.e. everything depending on the premium sum uses the original premium sum!
      # self$values$premiumSum = self$determinePremiumSum();
      self$values$cashFlowsCosts = mergeValues3D(starting=self$values$cashFlowsCosts, ending=self$determineCashFlowsCosts(t), t=t);

      pv = self$calculatePresentValues(t);
      pvc = self$calculatePresentValuesCosts(t);
      self$values$presentValuesCosts = mergeValues3D(starting=self$values$presentValuesCosts, ending=pvc, t=t);

      # TODO:
      # the premiumCalculation function returns the premiums AND the cofficients,
      # so we have to extract the coefficients and store them in a separate variable
      # res = self$calculatePremiums(t);
      # self$values$premiumCoefficients = mergeValues(starting=self$values$premiumCoefficients, ending=res[["coefficients"]], t=t);
      # self$values$premiums = mergeValues(starting= = res[["premiums"]]

      # Update the cash flows and present values with the values of the premium
      pvAllBenefits = self$calculatePresentValuesBenefits()
      self$values$presentValues = mergeValues(starting=self$values$presentValues, ending=cbind(pv, pvAllBenefits), t=t);

      self$values$absCashFlows = mergeValues(starting=self$values$absCashFlows, ending=self$calculateAbsCashFlows(t), t=t);
      self$values$absPresentValues = mergeValues(starting=self$values$absPresentValues, ending=self$calculateAbsPresentValues(t), t=t);
      self$values$reserves = mergeValues(starting=self$values$reserves, ending=self$calculateReserves(t), t=t);
      self$values$basicData = mergeValues(starting=self$values$basicData, ending=self$getBasicDataTimeseries(t), t=t);
      self$values$premiumComposition = mergeValues(starting=self$values$premiumComposition, ending=self$premiumAnalysis(t), t=t);

      self$addHistorySnapshot(time=t, comment=sprintf("Premium waiver at time %d", t), type="PremiumWaiver", params=self$params, values=self$values);
    },

    dummy=NULL
  )
);
# InsuranceContract$debug("premiumWaiver")
