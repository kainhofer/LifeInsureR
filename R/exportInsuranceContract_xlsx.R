library(openxlsx)

################################################
# Helper Functions
################################################


writeAgeQTable = function (wb, sheet, probs, crow=1, ccol=1, styles=list()) {
  writeData(wb, sheet, "Sterblichkeiten", startCol = ccol+2, startRow = crow);
  addStyle(wb, sheet, style=styles$header, rows=crow, cols = ccol+2, stack=TRUE);
  mergeCells(wb, sheet, rows=crow, cols=(ccol+2):(ccol+3))
  writeDataTable(wb, sheet, probs,
                 startRow=crow+1, startCol = ccol, colNames = TRUE, rowNames = TRUE,
                 tableStyle = "TableStyleMedium3", withFilter = FALSE, headerStyle = styles$tableHeader);
  freezePane(wb, sheet, firstActiveRow=crow+2, firstActiveCol = ccol+2)
  addStyle(wb, sheet, style=styles$center, rows=(crow+2):(crow+1+dim(probs)[[1]]), cols=ccol:(ccol+1), gridExpand = TRUE, stack=TRUE);
  addStyle(wb, sheet, style=styles$qx, rows=(crow+2):(crow+1+dim(probs)[[1]]), cols=(ccol+2):(ccol+3), gridExpand = TRUE, stack=TRUE);
  dim(probs)[[2]] + 2;
};

writeValuesTable = function (wb, sheet, values, caption=NULL, crow=1, ccol=1, rowNames=FALSE, tableStyle="TableStyleMedium3", tableName=NULL, withFilter=FALSE, styles=list(), valueStyle=NULL) {
  nrrow = dim(values)[[1]];
  nrcol = dim(values)[[2]];
  addcol = if (rowNames) 1 else 0;
  ecol = ccol + addcol + nrcol - 1;
  if (!missing(caption)) {
    writeData(wb, sheet, caption, startCol = ccol+addcol, startRow = crow);
    addStyle(wb, sheet, style=styles$header, rows=crow, cols = ccol+addcol, stack=TRUE);
    mergeCells(wb, sheet, rows=crow, cols=(ccol+addcol):ecol);
  }

  writeDataTable(wb, sheet, values, startRow=crow+1, startCol=ccol, colNames=TRUE,
                 rowNames=rowNames, tableStyle=tableStyle,
                 tableName=tableName, withFilter = withFilter, headerStyle = styles$tableHeader)
  if (!missing(valueStyle)) {
    addStyle(wb, sheet, style=valueStyle, rows=(crow+2):(crow+nrrow+1), cols=(ccol+addcol):ecol, gridExpand = TRUE, stack = TRUE);
  }
  # width of table is the return value
  nrcol + addcol
};

writePremiumCoefficients = function(wb, sheet, values, tarif=NULL, type="benefits", crow=crow, ccol=ccol) {
  writeData(wb, sheet, matrix(c(
    "Nettoprämie", "", "Zillmerprämie", "", "Bruttoprämie", "",
    "rel. zu VS", "rel. zu Prämie", "rel. zu VS", "rel. zu Prämie", "rel. zu VS", "rel. zu Prämie"), 6, 2
  ), startCol = ccol, startRow = crow, colNames = FALSE, borders = "rows", borderColour = "gray5", borderStyle = "thin");
  mergeCells(wb, sheet, cols = ccol, rows = crow:(crow+1));
  mergeCells(wb, sheet, cols = ccol, rows = (crow+2):(crow+3));
  mergeCells(wb, sheet, cols = ccol, rows = (crow+4):(crow+5));
  addStyle(wb, sheet, style=createStyle(halign = "left", valign = "center",
                                        borderColour = "gray5", border = "LeftBottomTop",
                                        borderStyle = "thin"),
           rows = crow:(crow+5), cols = ccol);
  addStyle(wb, sheet, style=createStyle(halign = "right", valign = "center",
                                        borderColour = "gray5", border = "RightBottomTop",
                                        borderStyle = "thin"),
           rows = crow:(crow+5), cols = ccol+1);

  # The first column of the benefits coefficients is for "age", which we want to remove
  mod = function(a) { as.data.frame(t(a)) };
  if (type=="costs") {
    mod = function(vals) {
      vals = setInsuranceValuesLabels(vals);
      newvals=vals;
      dimn = dimnames(newvals);
      dim(newvals) = c(1, dim(vals));
      dimnames(newvals) = c(list("Coeff"), dimn);
      as.data.frame(tarif$costValuesAsMatrix(newvals))
    };
  }
  coeff = rbind(mod(values[["net"]][["SumInsured"]][[type]]),
                mod(values[["net"]][["Premium"]][[type]]),
                mod(values[["Zillmer"]][["SumInsured"]][[type]]),
                mod(values[["Zillmer"]][["Premium"]][[type]]),
                mod(values[["gross"]][["SumInsured"]][[type]]),
                mod(values[["gross"]][["Premium"]][[type]]));

  writeData(wb, sheet, coeff, startCol = ccol+2, startRow = crow, colNames=FALSE, borders="rows", borderColour="gray5", borderStyle="thin");
  dim(coeff)[[2]]
}

labelsReplace = function(labels) {
  labels[labels=="alpha"] = "α";
  labels[labels=="Zillmer"] = "Zill.";
  labels[labels=="beta"] = "β";
  labels[labels=="gamma"] = "γ";
  labels[labels=="gamma_nopremiums"] = "γ_prf";
  labels[labels=="SumInsured"] = "VS";
  labels[labels=="SumPremiums"] = "PS";
  labels[labels=="GrossPremium"] = "BP";

  labels[labels=="premiums"] = "Präm.";
  labels[labels=="guaranteed"] = "Gar.";
  labels[labels=="survival"] = "Erl.";
  labels[labels=="death_SumInsured"] = "Abl. VS";
  labels[labels=="death_GrossPremium"] = "Abl. BP";
  labels[labels=="death"] = "Abl.";
  labels[labels=="death_PremiumFree"] = "Abl. prf";
  labels[labels=="benefits"] = "Abl.Lst.";
  labels[labels=="benefitsAndRefund"] = "Abl. + RG";

  labels[labels=="once"] = "einm."
  labels[labels=="PremiumPeriod"] = "PD"
  labels[labels=="PremiumFree"] = "Pr.Fr."
  labels[labels=="PolicyPeriod"] = "LZ"
  labels[labels=="Balance Sheet Reserve"] = "Bilanzreserve"


  labels
}

setInsuranceValuesLabels = function(vals) {
  dimnames(vals) = lapply(dimnames(vals), labelsReplace);
  vals
}


################################################################################
#
# The actual export function
#
#    exportInsuranceContract.xlsx(contract, filename)
#
################################################################################


exportInsuranceContract.xlsx = function(contract, filename) {
  # TODO: argument checking for contract and filename

  ###
  nrrows = dim(contract$values$cashFlows)[[1]]; # Some vectors are longer (e.g. qx), so determine the max nr or rows
  qp = contract$values$transitionProbabilities[1:nrrows,]; # extract the probabilities once, will be needed in every sheet

  ################################################
  # Style information
  ################################################
  styles = list(
    header = createStyle(border="TopBottomLeftRight", borderColour="#DA9694", borderStyle="medium",
                         fgFill="#C0504D", fontColour="#FFFFFF",
                         halign="center", valign="center", textDecoration="bold"),
    tableHeader = createStyle(#border="TopLeftRight", borderColour="#DA9694", borderStyle="medium",
                              #bgFill="#C0504D", fontColour="#FFFFFF",
                              halign="center", valign="center", textDecoration="bold"),
    hide0 = createStyle(numFmt="General; General; \"\""),
    currency0 = createStyle(numFmt="[$€-C07] #,##0.00;[red]-[$€-C07] #,##0.00;\"\""),
    cost0 = createStyle(numFmt="0.000%; 0.000%; \"\""),
    pv0 = createStyle(numFmt="0.00000;-0.00000;\"\""),
    qx = createStyle(numFmt="0.000000"),
    wrap = createStyle(wrapText=TRUE),
    center = createStyle(halign="center", valign="center")
  );

  ################################################
  # General Workbook setup
  ################################################
  wb = openxlsx::createWorkbook();
  addWorksheet(wb, "Tarifinformationen");
  addWorksheet(wb, "Basisdaten");
  addWorksheet(wb, "Reserven");
  addWorksheet(wb, "abs.Barwerte");
  addWorksheet(wb, "abs.Cash-Flows");
  addWorksheet(wb, "Barwerte");
  addWorksheet(wb, "Cash-Flows");

  # Print out general Contract and Tariff information, including results
  sheet = "Tarifinformationen"
  crow = 1;
  writeData(wb, sheet, matrix(c(
    "Tarif:", contract$tarif$tarif,
    "Tarifname:", contract$tarif$name,
    "Description:", contract$tarif$desc
  ), 3, 2, byrow = TRUE), startCol=1, startRow=1, colNames=FALSE, rowNames=FALSE);
  mergeCells(wb, sheet, cols=2:10, rows=1);
  mergeCells(wb, sheet, cols=2:10, rows=2);
  mergeCells(wb, sheet, cols=2:10, rows=3);
  addStyle(wb, sheet, style=styles$wrap, rows=3, cols=2:10, stack=TRUE);
  addStyle(wb, sheet, style=createStyle(valign="top"), rows=1:3, cols=1:10, gridExpand=TRUE, stack=TRUE);
  setColWidths(wb, sheet, cols = 1:50, widths = "auto", ignoreMergedCells = TRUE);

  crow = crow+4;

  ################################################
  # Basic parameters
  ################################################
  values=c(
    "Sum insured"=contract$params$sumInsured,
    "Mortality table"=contract$tarif$mortalityTable@name,
    "YOB"=contract$params$YOB,
    "Age"=contract$params$age,
    "Policy duration"=contract$params$policyPeriod,
    "Premium period"=contract$params$premiumPeriod,
    "Deferral"=contract$params$deferral,
    "Guaranteed payments"=contract$params$guaranteed,
    i=contract$tarif$i);

  writeData(wb, sheet, "Basisdaten des Vertrags und Tarifs", startCol=1, startRow=crow);
  mergeCells(wb, sheet, cols=1:length(values), rows=crow:crow);
  writeDataTable(wb, sheet, as.data.frame(t(values)),
                 startCol=1, startRow=crow+1, colNames=TRUE, rowNames=FALSE,
                 tableStyle="TableStyleMedium3", withFilter = FALSE, headerStyle = styles$tableHeader);

  crow = crow + 4;

  # Premiums
  writeData(wb, sheet, "Prämien", startCol=1, startRow=crow);
  mergeCells(wb, sheet, cols=1:length(contract$values$premiums), rows=crow:crow);
  writeDataTable(wb, sheet, setInsuranceValuesLabels(as.data.frame(t(contract$values$premiums))),
                 startCol=1, startRow=crow+1, colNames=TRUE, rowNames=FALSE,
                 tableStyle="TableStyleMedium3", withFilter = FALSE, headerStyle = styles$tableHeader);
  crow = crow + 4;

  # Cost structure:
  costtable = as.data.frame.table(setInsuranceValuesLabels(contract$tarif$costs) )
  colnames(costtable) = c("Kostenart", "Basis", "Periode", "Kostensatz");
  costtable = costtable[costtable[,"Kostensatz"]!=0.0000,]
  writeValuesTable(wb, sheet, costtable, crow=crow, ccol=1, tableName="Kosten", styles=styles, caption="Kosten");
  # writeDataTable(wb, sheet, costtable, startCol=1, startRow=crow+1, colNames=TRUE, rowNames=FALSE,
                 # tableStyle = "TableStyleMedium3", headerStyle = styles$tableHeader);
  addStyle(wb, sheet, style=styles$cost0, rows=(crow+2):(crow+dim(costtable)[[1]]+1), cols=4, stack=TRUE);
  setColWidths(wb, sheet, cols = 1:50, widths = "auto", ignoreMergedCells = TRUE)
  crow = crow + dim(costtable)[[1]] + 3;

  # Contract history
  # time=t, comment=sprintf("Premium waiver at time %d", t), type="PremiumWaiver"
  histtime = unlist(lapply(contract$history, function(xl) xl$time));
  histcomment = unlist(lapply(contract$history, function(xl) xl$comment));
  histtype = unlist(lapply(contract$history, function(xl) xl$type));
  writeValuesTable(wb, sheet, data.frame(time=histtime, Comment=histcomment, Type=histtype),
                   crow=crow, ccol=1, tableName="Vertragshistorie", styles=styles,
                   caption="Vertragshistorie");
  crow = crow + dim(histtime)[[1]] + 3;



  ################################################
  # Print out Basic contract data as time series
  ################################################

  # Age, death and survival probabilities
  ccol = 1;
  crow = 4;
  sheet = "Basisdaten";
  tbl = qp[,"age", drop=FALSE];
  writeDataTable(wb, sheet, tbl,
                 startRow=crow+1, startCol = ccol, colNames = TRUE, rowNames = TRUE,
                 tableStyle = "TableStyleMedium3", withFilter = FALSE, headerStyle = styles$tableHeader);
  freezePane(wb, sheet, firstActiveRow=crow+2, firstActiveCol = ccol+2)
  addStyle(wb, sheet, style=styles$center, rows=(crow+2):(crow+1+dim(tbl)[[1]]), cols=ccol:(ccol+1), gridExpand = TRUE, stack=TRUE);
  ccol = ccol + dim(tbl)[[2]] + 2;

  ccol = ccol + writeValuesTable(wb, sheet, as.data.frame(setInsuranceValuesLabels(contract$values$basicData)),
                                 crow=crow, ccol=ccol, tableName="Grunddaten", styles=styles,
                                 caption="Vertragsgrunddaten im Zeitverlauf", valueStyle=styles$currency0) + 1;
  setColWidths(wb, sheet, cols = 1:50, widths = "auto", ignoreMergedCells = TRUE)

  # TODO: Change PremiumPayment column to 0=prf. / 1=prpfl. / -1=außerplanm.prf.
  # TODO: Change InterestRate column to percent format



  ################################################
  # Print out Reserves and premium decomposition
  ################################################

  # Age, death and survival probabilities
  ccol = 1;
  crow = 4;
  sheet = "Reserven";

  ccol = ccol + writeAgeQTable(wb, sheet, probs=qp, crow=crow, ccol=1, styles=styles);
  ccol = ccol + writeValuesTable(wb, sheet, as.data.frame(setInsuranceValuesLabels(contract$values$reserves)),
                                 crow=crow, ccol=ccol, tableName="Reserves", styles=styles,
                                 caption="Reserven", valueStyle=styles$currency0) + 1;

  oldccol = ccol
  ccol = ccol + writeValuesTable(wb, sheet, as.data.frame(setInsuranceValuesLabels(contract$values$reservesBalanceSheet)),
                                 crow=crow, ccol=ccol, tableName="Bilanzreserve", styles=styles,
                                 caption="Bilanzreserve", valueStyle=styles$currency0) + 1;
  addStyle(wb, sheet, style = createStyle(numFmt="0.0##"), cols = oldccol, rows = (crow+2):(crow+1+dim(contract$values$reservesBalanceSheet)[[1]]), gridExpand = TRUE, stack = TRUE);
str("Style applied to cold and rows:")
str(oldccol)
str((crow+2):(crow+1+dim(contract$values$reservesBalanceSheet)[[1]]))

  ccol = ccol + writeValuesTable(wb, sheet, as.data.frame(setInsuranceValuesLabels(contract$values$premiumComposition)),
                                 crow=crow, ccol=ccol, tableName="Premium_Decomposition", styles=styles,
                                 caption = "Prämienzerlegung", valueStyle=styles$currency0) + 1;
  setColWidths(wb, sheet, cols = 1:50, widths = "auto", ignoreMergedCells = TRUE)


  ################################################
  # Print out absolute values of present values
  ################################################

  # Age, death and survival probabilities
  ccol = 1;
  crow = 4;
  sheet = "abs.Barwerte";
  ccol = ccol + writeAgeQTable(wb, sheet, probs=qp, crow=crow, ccol=1, styles=styles);

  ccol = ccol + writeValuesTable(wb, sheet, as.data.frame(setInsuranceValuesLabels(contract$values$absPresentValues)),
                                 crow=crow, ccol=ccol, tableName="PresentValues_absolute", styles=styles,
                                 caption = "abs. Leistungs- und Kostenbarwerte", valueStyle=styles$currency0) + 1;
  setColWidths(wb, sheet, cols = 1:50, widths = "auto", ignoreMergedCells = TRUE)


  ################################################
  # Print out absolute values for cash flows
  ################################################

  # Age, death and survival probabilities
  ccol = 1;
  crow = 4;
  sheet = "abs.Cash-Flows";
  ccol = ccol + writeAgeQTable(wb, sheet, probs=qp, crow=crow, ccol=1, styles=styles);
  ccol = ccol + writeValuesTable(wb, sheet, as.data.frame(setInsuranceValuesLabels(contract$values$absCashFlows)),
                                 crow=crow, ccol=ccol, tableName="CashFlows_absolute", styles=styles,
                                 caption="abs. Leistungs- und Kostencashflows", withFilter=TRUE, valueStyle=styles$currency0) + 1;
  setColWidths(wb, sheet, cols = 1:50, widths = "auto", ignoreMergedCells = TRUE)


  ################################################
  # Print out present values
  ################################################

  # Age, death and survival probabilities
  costPV = as.data.frame(contract$tarif$costValuesAsMatrix(setInsuranceValuesLabels(contract$values$presentValuesCosts)));
  ccol = 1;
  crow = 4;
  sheet = "Barwerte";
  # We add six lines before the present values to show the coefficients for the premium calculation
  ccol = ccol + writeAgeQTable(wb, sheet, probs=qp, crow=crow+6, ccol=1, styles=styles);

  # Store the start/end columns of the coefficients, since we need them later in the formula for the premiums!
  w1 = writePremiumCoefficients(wb, sheet, contract$values$premiumCoefficients, type="benefits", crow=crow, ccol=ccol-2, tarif=contract$tarif);
  area.premiumcoeff = paste0(int2col(ccol), "%d:", int2col(ccol+w1-1), "%d");
  area.premiumvals  = paste0("$", int2col(ccol), "$", crow+6+2, ":$", int2col(ccol+w1-1), "$", crow+6+2);
  ccol = ccol + writeValuesTable(wb, sheet, as.data.frame(setInsuranceValuesLabels(contract$values$presentValues)),
                                 crow=crow+6, ccol=ccol, tableName="PresentValues_Benefits", styles=styles,
                                 caption = "Leistungsbarwerte", valueStyle=styles$pv0) + 1;

  w2 = writePremiumCoefficients(wb, sheet, contract$values$premiumCoefficients, type="costs", crow=crow, ccol=ccol-2, tarif=contract$tarif);
  area.costcoeff = paste0(int2col(ccol), "%d:", int2col(ccol+w2-1), "%d");
  area.costvals  = paste0("$", int2col(ccol), "$", crow+6+2, ":$", int2col(ccol+w2-1), "$", crow+6+2);
  ccol = ccol + writeValuesTable(wb, sheet, as.data.frame(costPV),
                                 crow=crow+6, ccol=ccol, tableName="PresentValues_Costs", styles=styles,
                                 caption = "Kostenbarwerte", valueStyle=styles$cost0) + 1;

  # Now print out the formulas for premium calculation into the columns 2 and 3:
  writeData(wb, sheet, as.data.frame(c("Nettoprämie", contract$values$premiums[["net"]],"Zillmerprämie", contract$values$premiums[["Zillmer"]], "Bruttoprämie", contract$values$premiums[["gross"]])), startCol = 1, startRow=crow, colNames = FALSE, borders = "rows");
  for (i in 0:5) {
    writeFormula(wb, sheet, paste0("SUMPRODUCT(", sprintf(area.premiumcoeff, crow+i, crow+i), ", ", area.premiumvals, ") + SUMPRODUCT(", sprintf(area.costcoeff, crow+i, crow+i), ", ", area.costvals, ")"), startCol = 3, startRow = crow+i);
    addStyle(wb, sheet, style=styles$pv0, rows = crow+i, cols = 3, stack = TRUE);
  }
  for (i in c(0,2,4)) {
    writeFormula(wb, sheet, paste0(int2col(3), crow+i, "/", int2col(3), crow+i+1), startCol=2, startRow = crow+i);
    addStyle(wb, sheet, style=styles$pv0, rows = crow+i, cols = 2, stack = TRUE);
  }
  for (i in c(1,3,5)) {
    writeFormula(wb, sheet, paste0(int2col(2), crow+i-1, "*", contract$params$sumInsured), startCol=2, startRow = crow+i);
    addStyle(wb, sheet, style=styles$currency0, rows = crow+i, cols = 1:2, stack = TRUE, gridExpand = TRUE);
  }
  setColWidths(wb, sheet, cols = 1:50, widths = "auto", ignoreMergedCells = TRUE)


  ################################################
  # Print out cash flows
  ################################################

  # Age, death and survival probabilities
  costCF = as.data.frame(contract$tarif$costValuesAsMatrix(setInsuranceValuesLabels(contract$values$cashFlowsCosts)));
  ccol = 1;
  crow = 4;
  sheet = "Cash-Flows";
  ccol = ccol + writeAgeQTable(wb, sheet, probs=qp, crow=crow, ccol=1, styles=styles);
  ccol = ccol + writeValuesTable(wb, sheet, setInsuranceValuesLabels(contract$values$cashFlows),
                                 crow=crow, ccol=ccol, tableName="CashFlows_Benefits", styles=styles,
                                 caption="Leistungscashflows", withFilter=TRUE, valueStyle=styles$hide0) + 1;
  ccol = ccol + writeValuesTable(wb, sheet, costCF,
                                 crow=crow, ccol=ccol, tableName="CashFlows_Costs", styles=styles,
                                 caption="Kostencashflows", withFilter=TRUE, valueStyle=styles$cost0) + 1;
  setColWidths(wb, sheet, cols = 1:50, widths = "auto", ignoreMergedCells = TRUE)



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

}
