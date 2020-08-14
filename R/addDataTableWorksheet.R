#' Add a new worksheet to the excel workbook and export the given data table x to it nicely formatted.
# @export
# addDataTableWorksheet = function(wb, sheet, title = NULL, x = NULL, loopdim = 3, style = NULL, tableStyle = "TableStyleMedium17", comparisons = NULL, comparisonStyle = NULL, ...) {
#     addWorksheet(wb, sheet);
#     rw = 1;
#     if (!missing(title)) {
#         titleStyle = createStyle(
#             fontSize = 26,
#             fontColour = "#365F91", borderColour = "#4F81BD",
#             borderStyle = "medium", border = "Bottom", textDecoration = "bold")
#         writeData(wb, sheet, title, startCol = 1, startRow = rw)
#         addStyle(wb, sheet, style = titleStyle, rows = rw, cols = 1);
#         rw = rw + 2;
#     }
#     if (!missing(x)) {
#         if (loopdim > length(dim(x)))
#             loopdim = length(dim(x));
#         if (loopdim < 0)
#             loopdim = 1;
#         # str(dimnames(x));
#         rnames = dimnames(x)[[1]];
#         cnames = dimnames(x)[[2]];
#         # str("rnames, cnames: ");str(rnames);str(cnames);
#         dmname = names(dimnames(x))[[loopdim]];
#         dmnameStyle = createStyle(
#             fontSize = 20,
#             fontColour = "#4F81BD", #borderColour = "#4F81BD",
#             # borderStyle = "medium", border = "Bottom",
#             textDecoration = "bold")
#         headerStyle = createStyle(halign = "center", valign = "center");
#         lapply(
#             seq_along(dimnames(x)[[loopdim]]),
#             function(i) {
#                 dmheader = paste(dmname, dimnames(x)[[loopdim]][i], sep = " = ")
#                 writeData(wb, sheet, dmheader, startCol = 1, startRow = rw)
#                 addStyle(wb, sheet, style = dmnameStyle, rows = rw, cols = 1);
#                 rw <<- rw + 1;
#                 writeDataTable(
#                     wb, sheet,
#                     x = as.data.frame(x[,,i]), # TODO: Generalize this to use loopdim!
#                     colNames = TRUE, rowNames = TRUE,
#                     withFilter = FALSE,
#                     startCol = 1, startRow = rw,
#                     tableStyle = tableStyle, headerStyle = headerStyle);
#                 if (!is.null(style)) {
#                     addStyle(wb, sheet, style = style, rows = rw + seq_along(rnames), cols = 1 + seq_along(cnames), gridExpand = TRUE, stack = TRUE);
#                 }
#                 cl = 1 + length(cnames) + 2;
#
#                 lapply(
#                     seq_along(comparisons),
#                     function(j) {
#                         writeData(wb, sheet, names(comparisons)[j], startRow = rw - 1, startCol = cl)
#                         addStyle(wb, sheet, style = dmnameStyle, rows = rw - 1, cols = cl);
#
#                         writeDataTable(
#                             wb, sheet,
#                             x = as.data.frame(comparisons[[j]][,,i]), # TODO: Generalize this to use loopdim!
#                             colNames = TRUE, rowNames = TRUE,
#                             withFilter = FALSE,
#                             startCol = cl, startRow = rw,
#                             tableStyle = tableStyle, headerStyle = headerStyle);
#                         if (!is.null(comparisonStyle)) {
#                             addStyle(wb, sheet, style = comparisonStyle, rows = rw + seq_along(rnames), cols = cl + seq_along(cnames), gridExpand = TRUE, stack = TRUE);
#                         }
#                         cl <<- cl + 1 + length(cnames) + 1;
#                     }
#                 )
#                 rw <<- rw + length(rnames) + 1 + 2; # TODO: Generalize this to use loopdim!
#             }
#         );
#     }
# }
