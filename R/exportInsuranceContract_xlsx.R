#' @include HelperFunctions.R InsuranceContract.R InsuranceParameters.R InsuranceTarif.R ProfitParticipation.R
#'
#' @import openxlsx
#' @import MortalityTables
#' @import R6
NULL



################################################
# Helper Functions
################################################


writeAgeQTable = function(wb, sheet, probs, crow = 1, ccol = 1, styles = list()) {
  writeData(wb, sheet, "Sterblichkeiten", startCol = ccol + 2, startRow = crow);
  addStyle(wb, sheet, style = styles$header, rows = crow, cols = ccol + 2, stack = TRUE);
  mergeCells(wb, sheet, rows = crow, cols = (ccol + 2):(ccol + 4))
  writeDataTable(wb, sheet, setInsuranceValuesLabels(probs),
                 startRow = crow + 1, startCol = ccol, colNames = TRUE, rowNames = TRUE,
                 tableStyle = "tableStyleMedium3", withFilter = FALSE, headerStyle = styles$tableHeader);
  freezePane(wb, sheet, firstActiveRow = crow + 2, firstActiveCol = ccol + 2)
  addStyle(wb, sheet, style = styles$center, rows = (crow + 2):(crow + 1 + dim(probs)[[1]]), cols = ccol:(ccol + 1), gridExpand = TRUE, stack = TRUE);
  addStyle(wb, sheet, style = styles$qx, rows = (crow + 2):(crow + 1 + dim(probs)[[1]]), cols = (ccol + 2):(ccol + 3), gridExpand = TRUE, stack = TRUE);
  dim(probs)[[2]] + 2;
};

writeTableCaption = function(wb, sheet, caption, rows, cols, style = NULL) {
    r = min(rows);
    c = min(cols);
    writeData(wb, sheet, caption, startCol = c, startRow = r);
    if (!is.null(style)) {
        addStyle(wb, sheet, style = style, rows = r, cols = c, stack = TRUE);
    }
    mergeCells(wb, sheet, rows = rows, cols = cols);
}

writeValuesTable = function(wb, sheet, values, caption = NULL, crow = 1, ccol = 1, rowNames = FALSE, tableStyle = "tableStyleMedium3", tableName = NULL, withFilter=FALSE, styles = list(), valueStyle = NULL) {
  nrrow = dim(values)[[1]];
  nrcol = dim(values)[[2]];
  addcol = if (rowNames) 1 else 0;
  ecol = ccol + addcol + nrcol - 1;
  if (!missing(caption)) {
      writeTableCaption(wb, sheet, caption, rows = crow, cols = (ccol + addcol):ecol, style = styles$header)
  }

  writeDataTable(wb, sheet, values, startRow = crow + 1, startCol = ccol, colNames = TRUE,
                 rowNames = rowNames, tableStyle = tableStyle,
                 tableName = tableName, withFilter = withFilter, headerStyle = styles$tableHeader)
  if (!missing(valueStyle)) {
    addStyle(wb, sheet, style = valueStyle, rows = (crow + 2):(crow + nrrow + 1), cols = (ccol + addcol):ecol, gridExpand = TRUE, stack = TRUE);
  }
  # width of table is the return value
  nrcol + addcol
};

writePremiumCoefficients = function(wb, sheet, values, tarif = NULL, type = "benefits", crow = crow, ccol = ccol) {
  writeData(wb, sheet, matrix(c(
    "Nettoprämie", "", "Zillmerprämie", "", "Bruttoprämie", "",
    "rel. zu VS", "rel. zu Prämie", "rel. zu VS", "rel. zu Prämie", "rel. zu VS", "rel. zu Prämie"), 6, 2
  ), startCol = ccol, startRow = crow, colNames = FALSE, borders = "rows", borderColour = "gray5", borderStyle = "thin");
  mergeCells(wb, sheet, cols = ccol, rows = crow:(crow + 1));
  mergeCells(wb, sheet, cols = ccol, rows = (crow + 2):(crow + 3));
  mergeCells(wb, sheet, cols = ccol, rows = (crow + 4):(crow + 5));
  addStyle(wb, sheet, style = createStyle(halign = "left", valign = "center",
                                        borderColour = "gray5", border = "LeftBottomTop",
                                        borderStyle = "thin"),
           rows = crow:(crow + 5), cols = ccol);
  addStyle(wb, sheet, style = createStyle(halign = "right", valign = "center",
                                        borderColour = "gray5", border = "RightBottomTop",
                                        borderStyle = "thin"),
           rows = crow:(crow + 5), cols = ccol + 1);

  # The first column of the benefits coefficients is for "age", which we want to remove
  mod = function(vals) { as.data.frame(t(vals)) };
  if (type == "costs") {
    mod = function(vals) {
      vals = setInsuranceValuesLabels(vals);
      newvals = vals;
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

  writeData(wb, sheet, coeff, startCol = ccol + 2, startRow = crow, colNames = FALSE, borders = "rows", borderColour = "gray5", borderStyle = "thin");
  dim(coeff)[[2]]
}

labelsReplace = function(labels) {
  labels[labels == "alpha"] = "α";
  labels[labels == "Zillmer"] = "Zill.";
  labels[labels == "beta"] = "β";
  labels[labels == "gamma"] = "γ";
  labels[labels == "gamma_nopremiums"] = "γ prf.";
  labels[labels == "SumInsured"] = "VS";
  labels[labels == "SumPremiums"] = "PS";
  labels[labels == "GrossPremium"] = "BP";

  # Cash Flows
  labels[labels == "premiums_advance"] = "Präm. vorsch.";
  labels[labels == "premiums_arrears"] = "Präm. nachsch.";
  labels[labels == "guaranteed_advance"] = "Gar. vorsch.";
  labels[labels == "guaranteed_arrears"] = "Gar. nachsch.";
  labels[labels == "survival_advance"] = "Erl. vorsch.";
  labels[labels == "survival_arrears"] = "Erl. nachsch.";

  # Barwerte
  labels[labels == "premiums"] = "Präm.";
  labels[labels == "guaranteed"] = "Gar.";
  labels[labels == "survival"] = "Erl.";
  labels[labels == "death_SumInsured"] = "Abl. VS";
  labels[labels == "death_GrossPremium"] = "Abl. BP";
  labels[labels == "death"] = "Abl.";
  labels[labels == "disease_SumInsured"] = "Krkh.";
  labels[labels == "death_Refund_past"] = "PrRG(verg.)";
  labels[labels == "death_Refund_future"] = "PrRG(zuk.)";

  labels[labels == "death_PremiumFree"] = "Abl. prf";
  labels[labels == "benefits"] = "Abl.Lst.";
  labels[labels == "benefitsAndRefund"] = "Abl. + RG";

  labels[labels == "once"] = "einm."
  labels[labels == "PremiumPeriod"] = "PD"
  labels[labels == "PremiumFree"] = "Pr.Fr."
  labels[labels == "PolicyPeriod"] = "LZ"

  # Rückstellungen
  labels[labels == "adequate"] = "ausr.";
  labels[labels == "contractual"] = "vertragl.";
  labels[labels == "conversion"] = "Umrechn.";
  labels[labels == "alphaRefund"] = "α-Rücktrag";
  labels[labels == "reduction"] = "Sparpr.für DK";
  labels[labels == "PremiumsPaid"] = "Pr.Summe";
  labels[labels == "Surrender"] = "Rückkauf";
  labels[labels == "PremiumFreeSumInsured"] = "Prf.VS";
  labels[labels == "Balance Sheet Reserve"] = "Bilanzreserve"

  # Prämienzerlegung
  labels[labels == "charged"] = "verrechnet"
  labels[labels == "tax"] = "VSt."
  labels[labels == "loading.frequency"] = "UJZ"
  labels[labels == "rebate.premium"] = "Präm.Rab."
  labels[labels == "rebate.partner"] = "Partn.Rab."
  labels[labels == "unitcosts"] = "StkK"
  labels[labels == "profit.advance"] = "Vw.GB"
  labels[labels == "rebate.sum"] = "Summenrab."
  labels[labels == "charge.noMedicalExam"] = "o.ärztl.U."
  labels[labels == "gross"] = "Brutto"
  labels[labels == "alpha.noZillmer"] = "α(ungez.)";
  labels[labels == "alpha.Zillmer"] = "α(gezill.)";
  labels[labels == "net"] = "Netto";
  labels[labels == "risk"] = "Risikopr.";
  labels[labels == "savings"] = "Sparpr.";
  labels[labels == "Zillmer.risk"] = "gez.Risikopr.";
  labels[labels == "Zillmer.savings"] = "gez.Sparpr.";
  labels[labels == "Zillmer.amortization"] = "gez.AK-Tilgung";
  labels[labels == "Zillmer.savings.real"] = "Sparpr.für DK";

  # Vertragseigenschaften
  labels[labels == "InterestRate"] = "i";
  labels[labels == "PolicyDuration"] = "LZ";
  labels[labels == "PremiumPayment"] = "Prämienzhlg.";
  labels[labels == "Premiums"] = "Prämien";
  labels[labels == "age"] = "Alter";

  labels[labels == "time"] = "ZP t";
  labels[labels == "Comment"] = "Bemerkung";
  labels[labels == "Type"] = "Art";


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


#' @export
exportInsuranceContract.xlsx = function(contract, filename) {
  # TODO: argument checking for contract and filename

  ###
  nrrows = dim(contract$Values$cashFlows)[[1]]; # Some vectors are longer(e.g. qx), so determine the max nr or rows
  qp = contract$Values$transitionProbabilities[1:nrrows,]; # extract the probabilities once, will be needed in every sheet

  ################################################
  # Style information
  ################################################
  styles = list(
    header = createStyle(border = "TopBottomLeftRight", borderColour = "#DA9694", borderStyle = "medium",
                         fgFill = "#C0504D", fontColour = "#FFFFFF",
                         halign = "center", valign = "center", textDecoration = "bold"),
    tableHeader = createStyle(#border = "TopLeftRight", borderColour = "#DA9694", borderstyle = "medium",
                              #bgFill = "#C0504D", fontColour = "#FFFFFF",
                              halign = "center", valign = "center", textDecoration = "bold"),
    hide0 = createStyle(numFmt = "General; General; \"\""),
    currency0 = createStyle(numFmt = "[$€-C07] #,##0.00;[red]-[$€-C07] #,##0.00;\"\""),
    cost0 = createStyle(numFmt = "0.0##%; 0.0##%; \"\""),
    pv0 = createStyle(numFmt = "0.00000;-0.00000;\"\""),
    qx = createStyle(numFmt = "0.000000"),
    rate = createStyle(numFmt = "0.00####%"),
    digit = createStyle(numFmt = "0;-0;\"\""),
    wrap = createStyle(wrapText = TRUE),
    center = createStyle(halign = "center", valign = "center")
  );

  ################################################
  # General Workbook setup
  ################################################
  wb = openxlsx::createWorkbook();

  # Print out general Contract and Tariff information, including results
  sheet = "Tarifinformationen"
  addWorksheet(wb, sheet);
  crow = 1;
  writeData(wb, sheet, matrix(c(
    "Tarif:", contract$tarif$tarif,
    "Tarifname:", contract$tarif$name,
    "Description:", contract$tarif$desc
  ), 3, 2, byrow = TRUE), startCol = 1, startRow = 1, colNames = FALSE, rowNames = FALSE);
  mergeCells(wb, sheet, cols = 2:10, rows = 1);
  mergeCells(wb, sheet, cols = 2:10, rows = 2);
  mergeCells(wb, sheet, cols = 2:10, rows = 3);
  addStyle(wb, sheet, style = styles$wrap, rows = 3, cols = 2:10, stack = TRUE);
  addStyle(wb, sheet, style = createStyle(valign = "top"), rows = 1:3, cols = 1:10, gridExpand = TRUE, stack = TRUE);
  setColWidths(wb, sheet, cols = 1:50, widths = "auto", ignoreMergedCells = TRUE);

  crow = crow + 4;

  ################################################
  # Basic parameters
  ################################################
  values = c(
    "Sum insured"         = contract$Parameters$ContractData$sumInsured,
    "Mortality table"     = contract$Parameters$ActuarialBases$mortalityTable@name,
    "YOB"                 = contract$Parameters$ContractData$YOB,
    "Age"                 = contract$Parameters$ContractData$age,
    "Technical Age"       = contract$Parameters$ContractData$technicalAge,
    "Policy duration"     = contract$Parameters$ContractData$policyPeriod,
    "Premium period"      = contract$Parameters$ContractData$premiumPeriod,
    "Deferral period"     = contract$Parameters$ContractData$deferralPeriod,
    "Guaranteed payments" = contract$Parameters$ContractData$guaranteed,
    i                     = contract$Parameters$ActuarialBases$i
  );
  # Some types of tables don't need the birth year -> leave it out rather than throwing an error on opening in Excel!
  if (is.null(values["YOB"])) values["YOB"] = NULL;

  writeData(wb, sheet, "Basisdaten des Vertrags und Tarifs", startCol = 1, startRow = crow);
  mergeCells(wb, sheet, cols = 1:length(values), rows = crow:crow);
  writeDataTable(wb, sheet, setInsuranceValuesLabels(as.data.frame(t(values))),
                 startCol = 1, startRow = crow + 1, colNames = TRUE, rowNames = FALSE,
                 tableStyle = "tableStyleMedium3", withFilter = FALSE, headerStyle = styles$tableHeader);

  crow = crow + 4;

  # Premiums
  writeData(wb, sheet, "Prämien", startCol = 1, startRow = crow);
  mergeCells(wb, sheet, cols = 1:length(contract$Values$premiums), rows = crow:crow);
  writeDataTable(wb, sheet, setInsuranceValuesLabels(as.data.frame(t(contract$Values$premiums))),
                 startCol = 1, startRow = crow + 1, colNames = TRUE, rowNames = FALSE,
                 tableStyle = "tableStyleMedium3", withFilter = FALSE, headerStyle = styles$tableHeader);
  crow = crow + 4;

  # Cost structure:
  costtable = as.data.frame.table(setInsuranceValuesLabels(contract$Parameters$Costs) )
  colnames(costtable) = c("Kostenart", "Basis", "Periode", "Kostensatz");
  costtable = costtable[costtable[,"Kostensatz"] != 0.0000,]
  writeValuesTable(wb, sheet, costtable, crow = crow, ccol = 1, tableName = "Kosten", styles = styles, caption = "Kosten");
  # writeDataTable(wb, sheet, costtable, startCol = 1, startRow = crow + 1, colNames = TRUE, rowNames = FALSE,
                 # tableStyle = "tableStyleMedium3", headerStyle = styles$tableHeader);
  addStyle(wb, sheet, style = styles$cost0, rows = (crow + 2):(crow + dim(costtable)[[1]] + 1), cols = 4, stack = TRUE);
  setColWidths(wb, sheet, cols = 1:50, widths = "auto", ignoreMergedCells = TRUE)
  crow = crow + dim(costtable)[[1]] + 3;

  # Contract history
  # time=t, comment=sprintf("Premium waiver at time %d", t), type = "PremiumWaiver"
  histtime = unlist(lapply(contract$history, function(xl) xl$time));
  histcomment = unlist(lapply(contract$history, function(xl) xl$comment));
  histtype = unlist(lapply(contract$history, function(xl) xl$type));
  writeValuesTable(wb, sheet, setInsuranceValuesLabels(data.frame(time = histtime, Comment = histcomment, type = histtype)),
                   crow = crow, ccol = 1, tableName = "Vertragshistorie", styles = styles,
                   caption = "Vertragshistorie");
  crow = crow + dim(histtime)[[1]] + 3;



  ################################################
  # Print out Basic contract data as time series
  ################################################

  sheet = "Basisdaten";
  addWorksheet(wb, sheet);

  # Age, death and survival probabilities
  ccol = 1;
  crow = 4;
  tbl = qp[,"age", drop = FALSE];
  writeDataTable(wb, sheet, setInsuranceValuesLabels(tbl),
                 startRow = crow + 1, startCol = ccol, colNames = TRUE, rowNames = TRUE,
                 tableStyle = "tableStyleMedium3", withFilter = FALSE, headerStyle = styles$tableHeader);
  freezePane(wb, sheet, firstActiveRow = crow + 2, firstActiveCol = ccol + 2)
  addStyle(wb, sheet, style = styles$center, rows = (crow + 2):(crow + 1 + dim(tbl)[[1]]), cols = ccol:(ccol + 1), gridExpand = TRUE, stack = TRUE);
  ccol = ccol + dim(tbl)[[2]] + 2;

  ccol.table = ccol - 1;
  ccol = ccol + writeValuesTable(wb, sheet, as.data.frame(setInsuranceValuesLabels(contract$Values$basicData)),
                                 crow = crow, ccol = ccol, tableName = "Grunddaten", styles = styles,
                                 caption = "Vertragsgrunddaten im Zeitverlauf") + 1;
  setColWidths(wb, sheet, cols = 1:50, widths = "auto", ignoreMergedCells = TRUE)

  # Change InterestRate column to percent format
  # Change premiumPayment column to single-digit column
  # Change period columnts to normal numbers
  nrrow = dim(contract$Values$basicData)[[1]];
  cnames = colnames(contract$Values$basicData);

  r = (crow + 2):(crow + nrrow + 1);
  addStyle(wb, sheet, style = styles$rate, rows = r, cols = grep("^InterestRate$", cnames) + ccol.table, gridExpand = TRUE, stack = TRUE);
  addStyle(wb, sheet, style = styles$digit, rows = r,
           cols = grep("^(PremiumPayment|PolicyDuration|PremiumPeriod)$", cnames) + ccol.table,
           gridExpand = TRUE, stack = TRUE);
  addStyle(wb, sheet, style = styles$currency0, rows = r, cols = grep("^(SumInsured|Premiums)$", cnames) + ccol.table, gridExpand = TRUE, stack = TRUE);



  ################################################
  # Print out Reserves
  ################################################

  sheet = "Reserven";
  addWorksheet(wb, sheet);

  # Age, death and survival probabilities
  ccol = 1;
  crow = 4;

  ccol = ccol + writeAgeQTable(wb, sheet, probs = qp, crow = crow, ccol = 1, styles = styles);
  ccol = ccol + writeValuesTable(wb, sheet, as.data.frame(setInsuranceValuesLabels(contract$Values$reserves)),
                                 crow = crow, ccol = ccol, tableName = "Reserves", styles = styles,
                                 caption = "Reserven", valueStyle = styles$currency0) + 1;

  oldccol = ccol
  ccol = ccol + writeValuesTable(wb, sheet, as.data.frame(setInsuranceValuesLabels(contract$Values$reservesBalanceSheet)),
                                 crow = crow, ccol = ccol, tableName = "Bilanzreserve", styles = styles,
                                 caption = "Bilanzreserve", valueStyle = styles$currency0) + 1;
  addStyle(wb, sheet, style = createStyle(numFmt = "0.0##"), cols = oldccol, rows = (crow + 2):(crow + 1 + dim(contract$Values$reservesBalanceSheet)[[1]]), gridExpand = TRUE, stack = TRUE);

  setColWidths(wb, sheet, cols = 1:50, widths = "auto", ignoreMergedCells = TRUE)


  ################################################
  # Print out Profit Participation
  ################################################

  sheet = "Gewinnbeteiligung";
  addWorksheet(wb, sheet);

  # Age, death and survival probabilities
  ccol = 1;
  crow = 4;

  ccol = ccol + writeAgeQTable(wb, sheet, probs = qp, crow = crow, ccol = 1, styles = styles);
  ccol.table = ccol - 1;
  ccol = ccol + writeValuesTable(wb, sheet, as.data.frame(setInsuranceValuesLabels(contract$Values$profitParticipation)),
                                 crow = crow, ccol = ccol, tableName = "ProfitParticipation", styles = styles,
                                 # caption = "Gewinnbeteiligung",
                                 valueStyle = styles$currency0) + 1;

  nrrow = dim(contract$Values$profitParticipation)[[1]];

  cnames = colnames(contract$Values$profitParticipation);
  # Make sure "terminalBonusRate" is NOT matched! Need to use a negative lookahead..
  baseCols = grep("^(?!terminal).*Base$", cnames, perl = TRUE);
  rateCols = grep("^(?!terminal).*(Interest|Rate)$", cnames, perl = TRUE);
  profitCols = grep(".*Profit$", cnames);
  terminalBonusCols = grep("^terminal.*", cnames);
  deathCols = grep("^death.*", cnames);
  surrenderCols = grep("^surrender.*", cnames);
  premiumWaiverCols = grep("^premiumWaiver.*", cnames);

  # Rates are displayed in %:
  addStyle(wb, sheet, style = styles$rate, rows = (crow + 2):(crow + nrrow + 1), cols = rateCols + ccol.table, gridExpand = TRUE, stack = TRUE);

  # Add table headers for the various sections:
  if (length(baseCols) > 0) {
      writeTableCaption(wb, sheet, "Basisgrößen", rows = crow, cols = baseCols + ccol.table, style = styles$header);
  }
  if (length(rateCols) > 0) {
      writeTableCaption(wb, sheet, "Gewinnbeteiligungssätze", rows = crow, cols = rateCols + ccol.table, style = styles$header);
  }
  if (length(profitCols) > 0) {
      writeTableCaption(wb, sheet, "GB Zuweisungen", rows = crow, cols = profitCols + ccol.table, style = styles$header);
  }
  if (length(terminalBonusCols) > 0) {
      writeTableCaption(wb, sheet, "Schlussgewinn", rows = crow, cols = terminalBonusCols + ccol.table, style = styles$header);
  }
  if (length(deathCols) > 0) {
      writeTableCaption(wb, sheet, "Todesfallleistung", rows = crow, cols = deathCols + ccol.table, style = styles$header);
  }
  if (length(surrenderCols) > 0) {
      writeTableCaption(wb, sheet, "Rückkauf", rows = crow, cols = surrenderCols + ccol.table, style = styles$header);
  }
  if (length(premiumWaiverCols) > 0) {
      writeTableCaption(wb, sheet, "Prämienfreistellung", rows = crow, cols = premiumWaiverCols + ccol.table, style = styles$header);
  }

  setColWidths(wb, sheet, cols = 1:50, widths = "auto", ignoreMergedCells = TRUE)


  ################################################
  # Print out premium decomposition
  ################################################

  # Age, death and survival probabilities
  crow = 4;
  sheet = "Prämienzerlegung";
  addWorksheet(wb, sheet);

  crow = crow + dim(qp)[[1]] + 4;
  ccol = 1 + writeAgeQTable(wb, sheet, probs = qp, crow = crow, ccol = 1, styles = styles);
  ccol = ccol + writeValuesTable(wb, sheet, as.data.frame(setInsuranceValuesLabels(contract$Values$premiumCompositionSums)),
                                 crow = crow, ccol = ccol, tableName = "Premium_DecompositionSums", styles = styles,
                                 caption = "Prämienzerlegung(Summe zukünftiger Prämien)", valueStyle = styles$currency0) + 1;

  crow = crow + dim(qp)[[1]] + 4;
  ccol = 1 + writeAgeQTable(wb, sheet, probs = qp, crow = crow, ccol = 1, styles = styles);
  ccol = ccol + writeValuesTable(wb, sheet, as.data.frame(setInsuranceValuesLabels(contract$Values$premiumCompositionPV)),
                                 crow = crow, ccol = ccol, tableName = "Premium_DecompositionPV", styles = styles,
                                 caption = "Prämienzerlegung(Barwerte zukünftiger Prämien)", valueStyle = styles$currency0) + 1;

  # Write out the absolute premium decomposition last, because that one freezes the pane
  crow = 4;
  ccol = 1 + writeAgeQTable(wb, sheet, probs = qp, crow = crow, ccol = 1, styles = styles);
  ccol = ccol + writeValuesTable(wb, sheet, as.data.frame(setInsuranceValuesLabels(contract$Values$premiumComposition)),
                                 crow = crow, ccol = ccol, tableName = "Premium_Decomposition", styles = styles,
                                 caption = "Prämienzerlegung", valueStyle = styles$currency0) + 1;

  setColWidths(wb, sheet, cols = 1:50, widths = "auto", ignoreMergedCells = TRUE)

  ################################################
  # Print out absolute values of present values
  ################################################

  sheet = "abs.Barwerte";
  addWorksheet(wb, sheet);

    # Age, death and survival probabilities
  ccol = 1;
  crow = 4;
  ccol = ccol + writeAgeQTable(wb, sheet, probs = qp, crow = crow, ccol = 1, styles = styles);

  ccol = ccol + writeValuesTable(wb, sheet, as.data.frame(setInsuranceValuesLabels(contract$Values$absPresentValues)),
                                 crow = crow, ccol = ccol, tableName = "PresentValues_absolute", styles = styles,
                                 caption = "abs. Leistungs- und Kostenbarwerte", valueStyle = styles$currency0) + 1;
  setColWidths(wb, sheet, cols = 1:50, widths = "auto", ignoreMergedCells = TRUE)


  ################################################
  # Print out absolute values for cash flows
  ################################################

  # Age, death and survival probabilities
  ccol = 1;
  crow = 4;
  sheet = "abs.Cash-Flows";
  addWorksheet(wb, sheet);
  ccol = ccol + writeAgeQTable(wb, sheet, probs = qp, crow = crow, ccol = 1, styles = styles);
  ccol = ccol + writeValuesTable(wb, sheet, as.data.frame(setInsuranceValuesLabels(contract$Values$absCashFlows)),
                                 crow = crow, ccol = ccol, tableName = "CashFlows_absolute", styles = styles,
                                 caption = "abs. Leistungs- und Kostencashflows", valueStyle = styles$currency0) + 1;
  setColWidths(wb, sheet, cols = 1:50, widths = "auto", ignoreMergedCells = TRUE)


  ################################################
  # Print out present values
  ################################################

  sheet = "Barwerte";
  addWorksheet(wb, sheet);

    # Age, death and survival probabilities
  costPV = as.data.frame(contract$tarif$costValuesAsMatrix(setInsuranceValuesLabels(contract$Values$presentValuesCosts)));
  ccol = 1;
  crow = 4;
  # We add six lines before the present values to show the coefficients for the premium calculation
  ccol = ccol + writeAgeQTable(wb, sheet, probs = qp, crow = crow + 6, ccol = 1, styles = styles);

  # Store the start/end columns of the coefficients, since we need them later in the formula for the premiums!
  w1 = writePremiumCoefficients(wb, sheet, contract$Values$premiumCoefficients, type = "benefits", crow = crow, ccol = ccol - 2, tarif = contract$tarif);
  area.premiumcoeff = paste0(int2col(ccol), "%d:", int2col(ccol + w1 - 1), "%d");
  area.premiumvals  = paste0("$", int2col(ccol), "$", crow + 6 + 2, ":$", int2col(ccol + w1 - 1), "$", crow + 6 + 2);
  ccol = ccol + writeValuesTable(wb, sheet, as.data.frame(setInsuranceValuesLabels(contract$Values$presentValues)),
                                 crow = crow + 6, ccol = ccol, tableName = "PresentValues_Benefits", styles = styles,
                                 caption = "Leistungsbarwerte", valueStyle = styles$pv0) + 1;

  w2 = writePremiumCoefficients(wb, sheet, contract$Values$premiumCoefficients, type = "costs", crow = crow, ccol = ccol - 2, tarif = contract$tarif);
  area.costcoeff = paste0(int2col(ccol), "%d:", int2col(ccol + w2 - 1), "%d");
  area.costvals  = paste0("$", int2col(ccol), "$", crow + 6 + 2, ":$", int2col(ccol + w2 - 1), "$", crow + 6 + 2);
  ccol = ccol + writeValuesTable(wb, sheet, as.data.frame(costPV),
                                 crow = crow + 6, ccol = ccol, tableName = "PresentValues_Costs", styles = styles,
                                 caption = "Kostenbarwerte", valueStyle = styles$cost0) + 1;

  # Now print out the formulas for premium calculation into the columns 2 and 3:
  writeData(wb, sheet, as.data.frame(c("Nettoprämie", contract$Values$premiums[["net"]],"Zillmerprämie", contract$Values$premiums[["Zillmer"]], "Bruttoprämie", contract$Values$premiums[["gross"]])), startCol = 1, startRow = crow, colNames = FALSE, borders = "rows");
  for (i in 0:5) {
    writeFormula(wb, sheet, paste0("SUMPRODUCT(", sprintf(area.premiumcoeff, crow + i, crow + i), ", ", area.premiumvals, ") + SUMPRODUCT(", sprintf(area.costcoeff, crow + i, crow + i), ", ", area.costvals, ")"), startCol = 3, startRow = crow + i);
    addStyle(wb, sheet, style = styles$pv0, rows = crow + i, cols = 3, stack = TRUE);
  }
  for (i in c(0,2,4)) {
    writeFormula(wb, sheet, paste0(int2col(3), crow + i, "/", int2col(3), crow + i + 1), startCol = 2, startRow = crow + i);
    addStyle(wb, sheet, style = styles$pv0, rows = crow + i, cols = 2, stack = TRUE);
  }
  for (i in c(1,3,5)) {
    writeFormula(wb, sheet, paste0(int2col(2), crow + i - 1, "*", contract$Parameters$ContractData$sumInsured), startCol = 2, startRow = crow + i);
    addStyle(wb, sheet, style = styles$currency0, rows = crow + i, cols = 1:2, stack = TRUE, gridExpand = TRUE);
  }
  setColWidths(wb, sheet, cols = 1:50, widths = "auto", ignoreMergedCells = TRUE)


  ################################################
  # Print out cash flows
  ################################################

  sheet = "Cash-Flows";
  addWorksheet(wb, sheet);

  # Age, death and survival probabilities
  costCF = as.data.frame(contract$tarif$costValuesAsMatrix(setInsuranceValuesLabels(contract$Values$cashFlowsCosts)));
  ccol = 1;
  crow = 4;
  ccol = ccol + writeAgeQTable(wb, sheet, probs = qp, crow = crow, ccol = 1, styles = styles);
  ccol = ccol + writeValuesTable(wb, sheet, setInsuranceValuesLabels(contract$Values$cashFlows),
                                 crow = crow, ccol = ccol, tableName = "CashFlows_Benefits", styles = styles,
                                 caption = "Leistungscashflows", valueStyle = styles$hide0) + 1;
  ccol = ccol + writeValuesTable(wb, sheet, costCF,
                                 crow = crow, ccol = ccol, tableName = "CashFlows_Costs", styles = styles,
                                 caption = "Kostencashflows", valueStyle = styles$cost0) + 1;
  setColWidths(wb, sheet, cols = 1:50, widths = "auto", ignoreMergedCells = TRUE)



  openxlsx::saveWorkbook(wb, filename, overwrite = TRUE)

}
