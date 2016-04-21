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
    premiumCoefficients = NA,
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

    exportExcel = function(filename) {
      ###
      nrrows = dim(self$cashFlows)[[1]]; # Some vectors are longer (e.g. qx), so determine the max nr or rows

      ################################################
      # Style information
      ################################################
      headerStyle = createStyle(border="TopLeftRight", borderColour="#DA9694", borderStyle="medium", bgFill="#C0504D", fontColour="#FFFFFF", halign="center", valign="center", textDecoration="bold");
      tableHeaderStyle = createStyle(halign="center", valign="center", textDecoration="bold");
      hide0Style = createStyle(numFmt="General; General; \"\"");
      cost0Style = createStyle(numFmt="0.000%; 0.000%; \"\"");
      wrapStyle = createStyle(wrapText=TRUE);
      centerStyle = createStyle(halign="center", valign="center");

      ################################################
      # Helper Functions
      ################################################
      writeAgeQTable = function (sheet, crow=1, ccol=1) {
        writeData(wb, sheet, "Sterblichkeiten", startCol = ccol+2, startRow = crow);
        addStyle(wb, sheet, style=headerStyle, rows=crow, cols = ccol+2, stack=TRUE);
        mergeCells(wb, sheet, rows=crow, cols=(ccol+2):(ccol+3))
        writeDataTable(wb, sheet, self$transitionProbabilities[1:nrrows,],
                     startRow=crow+1, startCol = ccol, colNames = TRUE, rowNames = TRUE,
                     tableStyle = "TableStyleMedium3", withFilter = FALSE, headerStyle = tableHeaderStyle);
        freezePane(wb, sheet, firstActiveRow=crow+2, firstActiveCol = ccol+2)
        addStyle(wb, sheet, style=centerStyle, rows=(crow+2):(crow+1+nrrows), cols=ccol:(ccol+1), gridExpand = TRUE, stack=TRUE);
        dim(self$transitionProbabilities)[[2]] + 2;
      };
      writeValuesTable = function (sheet, values, caption=NULL, crow=1, ccol=1, rowNames=FALSE, tableStyle="TableStyleMedium3", tableName=NULL, withFilter=FALSE, headerStyle=tableHeaderStyle, valueStyle=NULL) {
        nrrow = dim(values)[[1]];
        nrcol = dim(values)[[2]];
        addcol = if (rowNames) 1 else 0;
        ecol = ccol + addcol + nrcol - 1;
        if (!missing(caption)) {
          writeData(wb, sheet, caption, startCol = ccol+addcol, startRow = crow);
          addStyle(wb, sheet, style=headerStyle, rows=crow, cols = ccol+addcol, stack=TRUE);
          mergeCells(wb, sheet, rows=crow, cols=(ccol+addcol):ecol);
        }
        writeDataTable(wb, sheet, values, startRow=crow+1, startCol=ccol, colNames=TRUE,
                       rowNames=rowNames, tableStyle=tableStyle,
                       tableName=tableName, withFilter = withFilter, headerStyle = headerStyle)
        if (!missing(valueStyle)) {
          addStyle(wb, sheet, style=valueStyle, rows=(crow+2):(crow+nrrow+1), cols=(ccol+addcol):ecol, gridExpand = TRUE, stack = TRUE);
        }
        # width of table is the return value
        nrcol + addcol
      };

      writePremiumCoefficients = function(sheet, values, type="benefits", crow=crow, ccol=ccol) {
        writeData(wb, sheet, matrix(c(
                    "Nettoprämie", "", "Zillmerprämie", "", "Bruttoprämie", "",
                    "rel. zu VS", "rel. zu Prämie", "rel. zu VS", "rel. zu Prämie", "rel. zu VS", "rel. zu Prämie"), 6, 2
                  ), startCol = ccol, startRow = crow, colNames = FALSE, borders = "rows", borderColour = "gray5", borderStyle = "thin");
        mergeCells(wb, sheet, cols = ccol, rows = crow:(crow+1));
        mergeCells(wb, sheet, cols = ccol, rows = (crow+2):(crow+3));
        mergeCells(wb, sheet, cols = ccol, rows = (crow+4):(crow+5));
        addStyle(wb, sheet, style=createStyle(valign = "center", borderColour = "gray5", border = "LeftBottomTop", borderStyle = "thin"), rows = crow:(crow+5), cols = ccol);
        mod = function(a) { as.data.frame(t(a)) };
        if (type=="costs") {
          mod = function(vals) {
            newvals=vals;
            dim(newvals) = c(1, dim(vals));
            dimnames(newvals) = c(list("Coeff"), dimnames(vals));
            as.data.frame(self$tarif$costValuesAsMatrix(newvals))
          };
        }

        writeData(wb, sheet, mod(values[["net"]][["SumInsured"]][[type]]),     startCol = ccol+2, startRow = crow, colNames=FALSE, borders="surrounding", borderColour="gray5", borderStyle="thin");
        writeData(wb, sheet, mod(values[["net"]][["Premium"]][[type]]),        startCol = ccol+2, startRow = crow+1, colNames=FALSE, borders="surrounding", borderColour="gray5", borderStyle="thin");
        writeData(wb, sheet, mod(values[["Zillmer"]][["SumInsured"]][[type]]), startCol = ccol+2, startRow = crow+2, colNames=FALSE, borders="surrounding", borderColour="gray5", borderStyle="thin");
        writeData(wb, sheet, mod(values[["Zillmer"]][["Premium"]][[type]]),    startCol = ccol+2, startRow = crow+3, colNames=FALSE, borders="surrounding", borderColour="gray5", borderStyle="thin");
        writeData(wb, sheet, mod(values[["gross"]][["SumInsured"]][[type]]),   startCol = ccol+2, startRow = crow+4, colNames=FALSE, borders="surrounding", borderColour="gray5", borderStyle="thin");
        writeData(wb, sheet, mod(values[["gross"]][["Premium"]][[type]]),      startCol = ccol+2, startRow = crow+5, colNames=FALSE, borders="surrounding", borderColour="gray5", borderStyle="thin");
      }
      # costPV = as.data.frame(self$tarif$costValuesAsMatrix(self$presentValuesCosts));
      # ccol = 1;
      # crow = 4;
      # # We add six lines before the present values to show the coefficients for the premium calculation
      # ccol = ccol + writeAgeQTable("Barwerte", crow=crow+6, ccol=1);
      #
      # ccol = ccol + writeValuesTable("Barwerte", as.data.frame(self$presentValues),
      #                                crow=crow+6, ccol=ccol, tableName="PresentValues_Benefits",
      #                                caption = "Leistungsbarwerte", valueStyle=hide0Style) + 1;
      #
      # writePremiumCoefficients("Barwerte", self$premiumCoefficients, type="costs", crow=crow, ccol=ccol-1);
      # ccol = ccol + writeValuesTable("Barwerte", as.data.frame(costPV),


      ################################################
      # General Workbook setup
      ################################################
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
        ), 3, 2, byrow = TRUE), startCol=1, startRow=1, colNames=FALSE, rowNames=FALSE);
      mergeCells(wb, "Tarifinformationen", cols=2:10, rows=1);
      mergeCells(wb, "Tarifinformationen", cols=2:10, rows=2);
      mergeCells(wb, "Tarifinformationen", cols=2:10, rows=3);
      addStyle(wb, "Tarifinformationen", style=wrapStyle, rows=3, cols=2:10, stack=TRUE);
      addStyle(wb, "Tarifinformationen", style=createStyle(valign="top"), rows=1:3, cols=1:10, gridExpand=TRUE, stack=TRUE);

      crow = crow+4;

      ################################################
      # Basic parameters
      ################################################
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
                     tableStyle="TableStyleMedium3", withFilter = FALSE, headerStyle = tableHeaderStyle);
      crow = crow + 4;

      # Premiums
      writeData(wb, "Tarifinformationen", "Prämien", startCol=1, startRow=crow);
      mergeCells(wb, "Tarifinformationen", cols=1:length(self$premiums), rows=crow:crow);
      writeDataTable(wb, "Tarifinformationen", as.data.frame(t(self$premiums)),
                     startCol=1, startRow=crow+1, colNames=TRUE, rowNames=FALSE,
                     tableStyle="TableStyleMedium3", withFilter = FALSE, headerStyle = tableHeaderStyle);
      crow = crow + 4;


      ################################################
      # Print out Reserves and premium decomposition
      ################################################

      # Age, death and survival probabilities
      ccol = 1;
      crow = 4;
      ccol = ccol + writeAgeQTable("Reserven", crow=crow, ccol=1);
      ccol = ccol + writeValuesTable("Reserven", as.data.frame(self$reserves),
                                     crow=crow, ccol=ccol, tableName="Reserves",
                                     caption="Reserven", valueStyle=hide0Style) + 1;
      ccol = ccol + writeValuesTable("Reserven", as.data.frame(self$premiumComposition),
                                     crow=crow, ccol=ccol, tableName="Premium_Decomposition",
                                     caption = "Prämienzerlegung", valueStyle=hide0Style) + 1;


      ################################################
      # Print out present values
      ################################################

      # Age, death and survival probabilities
      costPV = as.data.frame(self$tarif$costValuesAsMatrix(self$presentValuesCosts));
      ccol = 1;
      crow = 4;
      # We add six lines before the present values to show the coefficients for the premium calculation
      ccol = ccol + writeAgeQTable("Barwerte", crow=crow+6, ccol=1);

      writePremiumCoefficients("Barwerte", self$premiumCoefficients, type="benefits", crow=crow, ccol=ccol-1);
      ccol = ccol + writeValuesTable("Barwerte", as.data.frame(self$presentValues),
                                     crow=crow+6, ccol=ccol, tableName="PresentValues_Benefits",
                                     caption = "Leistungsbarwerte", valueStyle=hide0Style) + 1;

      writePremiumCoefficients("Barwerte", self$premiumCoefficients, type="costs", crow=crow, ccol=ccol-2);
      ccol = ccol + writeValuesTable("Barwerte", as.data.frame(costPV),
                                     crow=crow+6, ccol=ccol, tableName="PresentValues_Costs",
                                     caption = "Kostenbarwerte", valueStyle=cost0Style) + 1;


      ################################################
      # Print out cash flows
      ################################################

      # Age, death and survival probabilities
      costCF = as.data.frame(self$tarif$costValuesAsMatrix(self$cashFlowsCosts));
      ccol = 1;
      crow = 4;
      ccol = ccol + writeAgeQTable("Cash-Flows", crow=crow, ccol=1);
      ccol = ccol + writeValuesTable("Cash-Flows", self$cashFlows,
                                     crow=crow, ccol=ccol, tableName="CashFlows_Benefits",
                                     caption="Leistungscashflows", withFilter=TRUE, valueStyle=hide0Style) + 1;
      ccol = ccol + writeValuesTable("Cash-Flows", costCF,
                                     crow=crow, ccol=ccol, tableName="CashFlows_Costs",
                                     caption="Kostencashflows", withFilter=TRUE, valueStyle=cost0Style) + 1;


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
      pvAllBenefits = self$tarif$presentValueBenefits(presentValues = self$presentValues, presentValuesCosts = self$presentValuesCosts, premiums = self$premiums, sumInsured = self$sumInsured, premiumSum = self$premiumSum );
      self$presentValues = cbind(self$presentValues, pvAllBenefits)
      self$presentValues
    },

    calculatePremiums = function() {
      # the premiumCalculation function returns the premiums AND the cofficients, so we have to extract the coefficients and store them in a separate variable
      res = self$tarif$premiumCalculation(self$presentValues, self$presentValuesCosts, premiumSum = self$premiumSum, sumInsured = self$sumInsured, loadings = self$loadings);
      self$premiumCoefficients = res$coefficients;
      self$premiums = res$premiums
      self$premiums
    },

    calculateReserves = function() {
      self$reserves = self$tarif$reserveCalculation(premiums=self$premiums, pvBenefits=self$presentValues, pvCosts=self$presentValuesCosts, sumInsured=self$sumInsured, premiumSum = self$premiumSum, loadings = self$loadings);
    },

    premiumAnalysis = function() {
      self$premiumComposition = self$tarif$premiumDecomposition(premiums=self$premiums, reserves=self$reserves, pvBenefits=self$presentValues, pvCosts=self$presentValuesCosts, sumInsured=self$sumInsured);
      # self$premiums = cbind(self$premiums, premiumComposition)
      # self$premiums
    },

    dummy=NA
  )
);
