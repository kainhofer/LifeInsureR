library(R6)
library(openxlsx);
library(ValuationTables);

#' @include HelperFunctions.R
#' @include InsuranceTarif.R

InsuranceContract = R6Class("InsuranceContract",

  public = list(
    tarif = NA,
    ContractParameters = InsuranceContract.ParameterStructure, # Only values explicitly given for this contract, not including fallback values from the tariff
    Parameters = InsuranceContract.ParameterStructure,         # The whole parameter set, including values given by the tariff

    #### Caching values for this contract, initialized/calculated when the object is created
    Values = list(
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
      reservesBalanceSheet = NA,

      premiumComposition = NA
    ),

    #### Keeping the history of all contract changes during its lifetime
    history = list(),


    #### The code:

    initialize = function(tarif, age, policyPeriod, sumInsured = 1, ... ) {
                          # policyPeriod, premiumPeriod = policyPeriod, guaranteed = 0,
                          # ...,
                          # contractClosing = Sys.Date(),
                          # loadings = list(),
                          # premiumPayments = "in advance", benefitPayments = "in advance",
                          # premiumFrequency = 1, benefitFrequency = 1,
                          # deferral = 0, YOB = 1975) {
      self$tarif = tarif;

      # TODO: implement required parameters like contractClosing, etc. Fill those with defaults if not given
      self$ContractParameters = InsuranceContract.ParametersFill(age=age, policyPeriod=policyPeriod, sumInsured=sumInsured, ..., premiumWaiver = 0, surrenderPenalty = 0, alphaRefunded = 0);

      # Set default values for required contract-specific data
      if (is.na(self$ContractParameters$ContractData$contractClosing))
        self$ContractParameters$ContractData$contractClosing = sys.Date();

      self$Parameters = InsuranceContract.ParametersFallback(self$ContractParameters, self$tarif$getParameters())
str(self$Parameters)

      # self$calculateContract();
    },

    addHistorySnapshot = function(time = 0, comment = "Initial contract values", type = "Contract", params = self$Parameters, values = self$Values) {
      self$history = rbind(self$history,
                       list(time=list("time" = time, "comment" = comment, "type" = type, "params" = params, "values" = values)));
    },

    calculateContract = function() {
      self$Values$transitionProbabilities = self$determineTransitionProbabilities();

      self$Values$cashFlowsBasic = self$determineCashFlowsBasic();
      self$Values$cashFlows = self$determineCashFlows();
      self$Values$premiumSum = self$determinePremiumSum();
      self$Values$cashFlowsCosts = self$determineCashFlowsCosts();

      self$Values$presentValues = self$calculatePresentValues();
      self$Values$presentValuesCosts = self$calculatePresentValuesCosts();

      # the premiumCalculation function returns the premiums AND the cofficients,
      # so we have to extract the coefficients and store them in a separate variable
      res = self$calculatePremiums();
      self$Values$premiumCoefficients = res[["coefficients"]];
      self$Values$premiums = res[["premiums"]]

      # Update the cash flows and present values with the values of the premium
      pvAllBenefits = self$calculatePresentValuesBenefits()
      self$Values$presentValues = cbind(self$Values$presentValues, pvAllBenefits)

      self$Values$absCashFlows = self$calculateAbsCashFlows();
      self$Values$absPresentValues = self$calculateAbsPresentValues();
      self$Values$reserves = self$calculateReserves();
      self$Values$reservesBalanceSheet = self$calculateReservesBalanceSheet();
      self$Values$basicData = self$getBasicDataTimeseries()
      self$Values$premiumComposition = self$premiumAnalysis();
      self$Values$premiumCompositionSums = self$premiumCompositionSums();
      self$Values$premiumCompositionPV = self$premiumCompositionPV();

      self$addHistorySnapshot(0, "Initial contract values", type = "Contract", params = self$Params, values = self$Values);
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
    calculateReservesBalanceSheet = function(contractModification=NULL) {
      do.call(self$tarif$reserveCalculationBalanceSheet, c(self$params, self$values, list(contractModification=contractModification)));
    },
    premiumAnalysis = function(contractModification=NULL) {
      do.call(self$tarif$premiumDecomposition, c(self$params, self$values, list(contractModification=contractModification)));
    },
    premiumCompositionSums = function(contractModification=NULL) {
      do.call(self$tarif$calculateFutureSums, c(list(self$values$premiumComposition), self$params, self$values, list(contractModification=contractModification)));
    },
    premiumCompositionPV = function(contractModification=NULL) {
      do.call(self$tarif$calculatePresentValues, c(list(self$values$premiumComposition), self$params, self$values, list(contractModification=contractModification)));
    },
    getBasicDataTimeseries = function(contractModification=NULL) {
      do.call(self$tarif$getBasicDataTimeseries, c(self$params, self$values, list(contractModification=contractModification)));
    },

    # Premium Waiver: Stop all premium payments at time t
    # the SumInsured is determined from the available
    premiumWaiver = function (t) {
      newSumInsured = self$values$reserves[[toString(t), "PremiumFreeSumInsured"]];
      self$params$premiumWaiver = TRUE;
      self$params$surrenderPenalty = FALSE; # Surrender penalty has already been applied, don't apply a second time
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

      self$addHistorySnapshot(time = t, comment = sprintf("Premium waiver at time %d", t),
                              type = "PremiumWaiver", params = self$Params, values = self$Values);
    },

    dummy=NULL
  )
);
# InsuranceContract$debug("premiumWaiver")
