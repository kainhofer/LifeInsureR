frame_files = lapply(sys.frames(), function(x) x$ofile)
frame_files = Filter(Negate(is.null), frame_files)
PATH <- dirname(frame_files[[length(frame_files)]])
rm(frame_files)

setwd(dirname(PATH))

library("gdata")


###############################################################################
# DAV 1994R exact (Male, Female), 1st-order only
###############################################################################

DAV1994R.exakt.data=read.xls(
  "Tafeln/DAV_R.xls", 
  sheet="DAV 1994R", skip=2, #row.names=1, 
  col.names=c("age",
              "qx2000", "qy2000", "trendM", "trendF", "qxAVbase1955", "qyAVbase1955",
              "", "", "", "", "", "")
);

DAV1994R.male=valuationTable.trendProjection(
  name="DAV 1994R male",
  ages=DAV1994R.exakt.data$age, baseYear=2000,
  deathProbs=DAV1994R.exakt.data$qx2000, 
  trend=DAV1994R.exakt.data$trendM
);
DAV1994R.female=valuationTable.trendProjection(
  name="DAV 1994R female",
  ages=DAV1994R.exakt.data$age, baseYear=2000,
  deathProbs=DAV1994R.exakt.data$qy2000, 
  trend=DAV1994R.exakt.data$trendF
);


###############################################################################
# DAV 2004R exact (Male, Female), 1st-order only
###############################################################################

DAV2004R.data.basistafeln=read.xls(
  "Tafeln/DAV_2004_R.xls", 
  sheet="Basistafeln", skip=3, #row.names=1, 
  col.names=c("age",
              "qxSel2Ord", "qySel2Ord", "qxAgg2Ord", "qyAgg2Ord", 
              "qxSelBestand", "qySelBestand", "qxAggBestand", "qyAggBestand", 
              "qxSel", "qySel", "qxAgg", "qyAgg")
);
DAV2004R.data.trend=read.xls(
  "Tafeln/DAV_2004_R.xls", 
  sheet="Trends", skip=4, header=FALSE, #row.names=1, 
  col.names=c("age",
              "trend2Ord.male.start", "trend2Ord.female.start",
              "trend2Ord.male.end", "trend2Ord.female.end",
              "trendBestand.male.start", "trendBestand.female.start",
              "trendBestand.male.end", "trendBestand.female.end",
              "trend1Ord.male", "trend1Ord.female")
);
DAV2004R.data.select=read.xls(
  "Tafeln/DAV_2004_R.xls", 
  sheet="Selektionsfaktoren", skip=2, header=FALSE, #row.names=1, 
  col.names=c("year", "SelectMale", "SelectFemale")
);
DAV2004R.data.av.grundtafeln=read.xls(
  "Tafeln/DAV_2004_R.xls", 
  sheet="Grundtafeln", skip=3, #row.names=1, 
  col.names=c("age",
              "qxBestand", "qyBestand",
              "qxB20", "qyB20",
              "qx1Ord", "qy1Ord")
);
DAV2004R.data.av=read.xls(
  "Tafeln/DAV_2004_R.xls", 
  sheet="Altersverschiebungen", skip=1, row.names=1, 
  col.names=c("YOB", "shiftMBestand", "shiftFBestand",
              "shiftMB20", "shiftFB20",
              "shiftM1Ord", "shiftF1Ord")
);

DAV2004R.male=valuationTable.trendProjection(
  name="DAV 2004R male",
  ages=DAV2004R.exakt.data$age, baseYear=2000,
  deathProbs=DAV2004R.exakt.data$qx2000, 
  trend=DAV2004R.exakt.data$trendM
);
DAV2004R.female=valuationTable.trendProjection(
  name="DAV 2004R female",
  ages=DAV2004R.exakt.data$age, baseYear=2000,
  deathProbs=DAV2004R.exakt.data  $qy2000, 
  trend=DAV2004R.exakt.data$trendF
);
