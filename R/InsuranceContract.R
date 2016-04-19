library(R6)
library(openxlsx);
# require(xlsx)

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
      self$age = age;
      self$policyPeriod = policyPeriod;
      if (missing(premiumPeriod) && !is.na(self$tarif$defaultPremiumPeriod)) {
        self$premiumPeriod = self$tarif$defaultPremiumPeriod;
      } else {
        self$premiumPeriod = premiumPeriod;
      }
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
      self$premiumAnalysis();

    },

    exportExcel.new = function(filename) {
      crow=1;
      nrrows = dim(self$cashFlows)[[1]]; # Some vectors are longer (e.g. qx), so determine the max nr or rows

      wb = openxlsx::createWorkbook();
      addWorksheet(wb, "Tarifinformationen");
      addWorksheet(wb, "Reserven");
      addWorksheet(wb, "Barwerte");
      addWorksheet(wb, "Cash-Flows");

      # Print out general Contract and Tariff information, including results
      crow = 1;
      writeData(wb, "Tarifinformationen", matrix(c(
          "Tarif:", self$tarif$tarif,
          "Tarifname:", self$tarif$name,
          "Description:", self$tarif$desc
        ), 3, 2, byrow = TRUE), startCol=1, startRow=1, colNames=FALSE, rowNames=FALSE,
        borders = "all");
      mergeCells(wb, "Tarifinformationen", cols=2:10, rows=1);
      mergeCells(wb, "Tarifinformationen", cols=2:10, rows=2);
      mergeCells(wb, "Tarifinformationen", cols=2:10, rows=3);

      crow = crow+4;

      # Basic parameters
      values=c(
          "Sum insured"=self$sumInsured,
          "Mortality table"=self$tarif$mortalityTable@name,
          "YOB"=self$YOB,
          "Age"=self$age,
          "Policy duration"=self$policyPeriod,
          "Premium period"=self$premiumPeriod,
          "Deferral"=self$deferral,
          "Guaranteed payments"=self$guaranteed,
          i=self$tarif$i);

      writeData(wb, "Tarifinformationen", "Basisdaten des Vertrags und Tarifs", startCol=1, startRow=crow);
      mergeCells(wb, "Tarifinformationen", cols=1:length(values), rows=crow:crow);
      writeDataTable(wb, "Tarifinformationen", as.data.frame(t(values)),
                     startCol=1, startRow=crow+1, colNames=TRUE, rowNames=FALSE,
                     tableStyle="TableStyleMedium3", withFilter = FALSE);
      crow = crow + 4;

      # Premiums
      writeData(wb, "Tarifinformationen", "Prämien", startCol=1, startRow=crow);
      mergeCells(wb, "Tarifinformationen", cols=1:length(self$premiums), rows=crow:crow);
      writeDataTable(wb, "Tarifinformationen", as.data.frame(t(self$premiums)),
                     startCol=1, startRow=crow+1, colNames=TRUE, rowNames=FALSE,
                     tableStyle="TableStyleMedium3", withFilter = FALSE);
      crow = crow + 4;


      ################################################
      # Print out Reserves and premium decomposition
      ################################################

      # Age, death and survival probabilities
      ccol = 1;
      writeDataTable(wb, "Reserven", self$transitionProbabilities[1:nrrows,],
                     startRow=3, startCol = ccol, colNames = TRUE, rowNames = TRUE,
                     tableStyle = "TableStyleMedium3", withFilter = FALSE);
      ccol = ccol + dim(self$transitionProbabilities)[[2]] + 2;

      writeDataTable(wb, "Reserven", as.data.frame(self$reserves), startRow=3,
                     startCol=ccol, colNames=TRUE, rowNames=FALSE, tableStyle="TableStyleMedium3",
                     tableName="Reserves", withFilter = FALSE)
      ccol = ccol + dim(self$reserves)[[2]] + 1;

      writeDataTable(wb, "Reserven", as.data.frame(self$premiumComposition), startRow=3,
                     startCol=ccol, colNames=TRUE, rowNames=FALSE, tableStyle="TableStyleMedium3",
                     tableName="Premium_Decomposition", withFilter = FALSE)
      ccol = ccol + dim(self$premiumComposition)[[2]] + 1;



      ################################################
      # Print out present values
      ################################################

      # Age, death and survival probabilities
      ccol = 1;
      writeDataTable(wb, "Barwerte", self$transitionProbabilities[1:nrrows,],
                     startRow=3, startCol = ccol, colNames = TRUE, rowNames = TRUE,
                     tableStyle = "TableStyleMedium3", withFilter = FALSE);
      ccol = ccol + dim(self$transitionProbabilities)[[2]] + 2;

      writeDataTable(wb, "Barwerte", as.data.frame(self$presentValues), startRow=3,
                     startCol=ccol, colNames=TRUE, rowNames=FALSE, tableStyle="TableStyleMedium3",
                     tableName="PresentValues_Benefits", withFilter = FALSE)
      ccol = ccol + dim(self$presentValues)[[2]] + 1;

      costPV = as.data.frame(self$tarif$costValuesAsMatrix(self$presentValuesCosts));
      writeDataTable(wb, "Barwerte", as.data.frame(costPV), startRow=3, startCol=ccol,
                     colNames=TRUE, rowNames=FALSE, tableStyle="TableStyleMedium3",
                     tableName="PresentValues_Costs", withFilter = FALSE)
      ccol = ccol + dim(costPV)[[2]] + 1;



      ################################################
      # Print out cash flows
      ################################################

      # Age, death and survival probabilities
      ccol = 1;
      writeDataTable(wb, "Cash-Flows", self$transitionProbabilities[1:nrrows,],
                     startRow=3, startCol = ccol, colNames = TRUE, rowNames = TRUE,
                     tableStyle = "TableStyleMedium3", withFilter = FALSE);
      ccol = ccol + dim(self$transitionProbabilities)[[2]] + 2;

      # Benefit Cash Flows
      writeDataTable(wb, "Cash-Flows", self$cashFlows, startRow=3, startCol=ccol,
                     colNames=TRUE, rowNames=FALSE, tableStyle="TableStyleMedium3",
                     tableName="CashFlows_CBenefots", withFilter = TRUE)
      ccol = ccol + dim(self$cashFlows)[[2]] + 1;

      # Costs Cash Flows
      # Age, death and survival probabilities
      costCF = as.data.frame(self$tarif$costValuesAsMatrix(self$cashFlowsCosts));
      writeDataTable(wb, "Cash-Flows", costCF, startRow=3, startCol=ccol, colNames=TRUE,
                     rowNames=FALSE, tableStyle="TableStyleMedium3",
                     tableName="CashFlows_Costs", withFilter = TRUE)
      ccol = ccol + dim(costCF)[[2]] + 1;



      openxlsx::saveWorkbook(wb, filename, overwrite = TRUE)


      # #### Contract
      # premiumPayments = PaymentTimeEnum("in advance"),
      # benefitPayments = PaymentTimeEnum("in advance"),
      # premiumFrequency = 1,
      # benefitFrequency = 1, # Only for annuities!
      # loadings = list(),    # Allow overriding the tariff-defined loadings (see the InsuranceTariff class for all possible names)
      # premiumSum = 0,
      #
      #
      # #### TARIF:
      # tariffType = TariffTypeEnum("wholelife"), # possible values: annuity, wholelife, endowment, pureendowment, terme-fix
      # premiumFrequencyOrder = 0,
      # benefitFrequencyOrder = 0,
      # widowFactor = 0,
      # premiumRefund = 0,
      # premiumRefundLoading = 0,  # Mindesttodesfallrisiko soll damit erreicht werden, z.B. 105% der einbezahlten Prämien
      # costs = list(),
      # benefitFrequencyLoading = list("1" = 0.0, "2" = 0.0, "4" = 0.0, "12" = 0.0), # TODO: Properly implement this
      # premiumFrequencyLoading = list("1" = 0.0, "2" = 0.0, "4" = 0.0, "12" = 0.0), # TODO: Implement this
      # loadings = list(    # Loadings can also be function(sumInsured, premiums)    # TODO: Add other possible arguments
      #   "ongoingAlphaGrossPremium" = 0,    # Acquisition cost that increase the gross premium
      #   "tax" = 0.04,                      # insurance tax, factor on each premium paid
      #   "unitcosts" = 0,                   # annual unit cost for each policy (Stückkosten), absolute value
      #   "security" = 0,                    # Additional security loading on all benefit payments, factor on all benefits
      #   "noMedicalExam" = 0,               # Loading when no medicial exam is done, % of SumInsured
      #   "noMedicalExamRelative" = 0,       # Loading when no medicial exam is done, % of gross premium
      #   "sumRebate" = 0,                   # gross premium reduction for large premiums, % of SumInsured
      #   "premiumRebate" = 0,               # gross premium reduction for large premiums, % of gross premium # TODO
      #   "advanceProfitParticipation" = 0,                # Vorweggewinnbeteiligung (%-Satz der Bruttoprämie)
      #   "advanceProfitParticipationInclUnitCost" = 0,    # Vorweggewinnbeteiligung (%-Satz der Prämie mit Zu-/Abschlägen, insbesondere nach Stückkosten)
      #   "partnerRebate" = 0                # Partnerrabatt auf Prämie mit Zu-/Abschlägen, wenn mehr als 1 Vertrag gleichzeitig abgeschlossen wird, additiv mit advanceBonusInclUnitCost and premiumRebate
      # ),

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
      pvAllBenefits = self$tarif$presentValueBenefits(presentValues = self$presentValues, premiums = self$premiums, sumInsured = self$sumInsured );
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

    premiumAnalysis = function() {
      self$premiumComposition = self$tarif$premiumDecomposition(premiums=self$premiums, reserves=self$reserves, pvBenefits=self$presentValues, pvCosts=self$presentValuesCosts, sumInsured=self$sumInsured);
      # self$premiums = cbind(self$premiums, premiumComposition)
      # self$premiums
    },

    dummy=NA
  )
);
