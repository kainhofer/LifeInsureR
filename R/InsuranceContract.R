
InsuranceContract = R6Class(
  "InsuranceContract",
  public = list(
    tarif = NA,

    #### Contract settings
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

    loadings = list(),    # Allow overriding the tariff-defined loadings (see the InsuranceTariff class for all possible names)

    #### Caching values for this contract, initialized/calculated when the object is created
    transitionProbabilities = NA,

    cashFlowsBasic = NA,
    cashFlows = NA,
    cashFlowsCosts = NA,
    premiumSum = 0,

    presentValues = NA,
    presentValuesCosts = NA,

    premiums = NA,
    reserves = NA,


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
      self$age = age;
      self$policyPeriod = policyPeriod;
      self$premiumPeriod = premiumPeriod;
      self$sumInsured = sumInsured;
      if (!missing(deferral))     self$deferral = deferral;
      if (!missing(YOB))          self$YOB = YOB;
      if (!missing(premiumPayments)) self$premiumPayments = premiumPayments;
      if (!missing(benefitPayments)) self$benefitPayments = benefitPayments;
      if (!missing(premiumFrequency)) self$premiumFrequency = premiumFrequency;
      if (!missing(benefitFrequency)) self$benefitFrequency = benefitFrequency;
      if (!missing(guaranteed))   self$guaranteed = guaranteed;
      if (!missing(loadings))     self$loadings = loadings;

      self$recalculate();
    },

    recalculate = function() {
      self$determineTransitionProbabilities();
      self$determineCashFlows();
      self$calculatePresentValues();
      self$calculatePremiums();
      self$calculatePresentValuesAllBenefits();
      self$calculateReserves();

    },

    determineTransitionProbabilities = function() {
      self$transitionProbabilities = self$tarif$getTransitionProbabilities(YOB = self$YOB, age = self$age);
      self$transitionProbabilities
    },

    determineCashFlows = function() {
      self$cashFlowsBasic = self$tarif$getBasicCashFlows(YOB = self$YOB, age = self$age, guaranteed = self$guaranteed, deferral = self$deferral, policyPeriod = self$policyPeriod, premiumPeriod = self$premiumPeriod);
      self$cashFlows = self$tarif$getCashFlows(age = self$age, premiumPayments = self$premiumPayments, benefitPayments = self$benefitPayments, policyPeriod = self$policyPeriod, guaranteed = self$guaranteed, deferral = self$deferral, premiumPaymentPeriod = self$premiumPeriod, basicCashFlows = self$cashFlowsBasic);
      self$premiumSum = sum(self$cashFlows$premiums_advance + self$cashFlows$premiums_arrears);
      self$cashFlowsCosts = self$tarif$getCashFlowsCosts(YOB = self$YOB, age = self$age, deferral = self$deferral, guaranteed = self$guaranteed, premiumPaymentPeriod = self$premiumPeriod, policyPeriod = self$policyPeriod);
      list("benefits"= self$cashFlows, "costs"=self$cashFlowCosts, "premiumSum" = self$premiumSum)
    },

    calculatePresentValues = function() {
      self$presentValues = self$tarif$presentValueCashFlows(self$cashFlows, age = self$age, YOB = self$YOB, premiumFrequency = self$premiumFrequency, benefitFrequency = self$benefitFrequency, loadings = self$loadings);
      self$presentValuesCosts = self$tarif$presentValueCashFlowsCosts(self$cashFlowsCosts, age = self$age, YOB = self$YOB);
      list("benefits" = self$presentValues, "costs" = self$presentValuesCosts)
    },

    # Add total benefits present value to the PV array. This can only be done after premium calculation, because e.g. premium refund depends on gross premium!
    calculatePresentValuesAllBenefits = function() {
      pvAllBenefits = self$tarif$presentValueBenefits(presentValues = self$presentValues, premiums = self$premiums, sumInsured = self$sumInsured);
      self$presentValues = cbind(self$presentValues, pvAllBenefits)
      self$presentValues
    },

    calculatePremiums = function() {
      self$premiums = self$tarif$premiumCalculation(self$presentValues, self$presentValuesCosts, premiumSum = self$premiumSum, sumInsured = self$sumInsured, loadings = self$loadings);
      self$premiums
    },

    calculateReserves = function() {
      self$reserves = self$tarif$reserveCalculation(premiums=self$premiums, pvBenefits=self$presentValues, pvCosts=self$presentValuesCosts, sumInsured=self$sumInsured, loadings = self$loadings);
    },

    dummy=NA
  )
);
