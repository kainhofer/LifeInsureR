frame_files = lapply(sys.frames(), function(x) x$ofile)
frame_files = Filter(Negate(is.null), frame_files)
PATH <- dirname(frame_files[[length(frame_files)]])
rm(frame_files)

setwd(dirname(PATH))

library("gdata")


# DAV1994T (Male, Female), 1st-order and general mortality 1986/88
dav1994T.data=read.xls(
  "Tafeln/DAV_T.xls", 
  sheet=1, skip=1, #row.names=1, 
  col.names=c("age",
              "", "", 
              "qx2", "qxKI", "qx",
              "", "", "",
              "qy2", "qyKI", "qy"
));

dav1994t.male=valuationTable.period(
  name="DAV 1994T male, loaded",
  ages=dav1994T.data$age, deathProbs=dav1994T.data$qx)
dav1994t.male.2Ord=valuationTable.period(
  name="DAV 1994T male, unloaded",
  ages=dav1994T.data$age, deathProbs=dav1994T.data$qx2)
dav1994t.female=valuationTable.period(
  name="DAV 1994T female, loaded",
  ages=dav1994T.data$age, deathProbs=dav1994T.data$qy)
dav1994t.female.2Ord=valuationTable.period(
  name="DAV 1994T female, unloaded",
  ages=dav1994T.data$age, deathProbs=dav1994T.data$qy2)

rm(dav1994T.data)

######################################################
##  DAV 2008T Aggregat / Smoker / Non-Smoker
######################################################

dav2008T.data=read.xls(
  "Tafeln/DAV_T.xls", 
  sheet=2, skip=2, #row.names=1, 
  col.names=c("age", "", "", "",
              "qx2", "qx2NR", "qx2R",
              "qx1", "qx1NR", "qx1R",
              "", "", "", "",
              "qy2", "qy2NR", "qy2R",
              "qy1", "qy1NR", "qy1R"
));

### DAV 2008T Aggregat (smoker+non-smoker combined)
dav2008t.male=valuationTable.period(
  name="DAV 2008T male, loaded",
  ages=dav2008T.data$age, deathProbs=dav2008T.data$qx1)
dav2008t.male.2Ord=valuationTable.period(
  name="DAV 2008T male, unloaded",
  ages=dav2008T.data$age, deathProbs=dav2008T.data$qx2)
dav2008t.female=valuationTable.period(
  name="DAV 2008T female, loaded",
  ages=dav2008T.data$age, deathProbs=dav2008T.data$qy1)
dav2008t.female.2Ord=valuationTable.period(
  name="DAV 2008T female, unloaded",
  ages=dav2008T.data$age, deathProbs=dav2008T.data$qy2)

### DAV 2008T Smoker
dav2008t.male.smoker=valuationTable.period(
  name="DAV 2008T male smoker, loaded",
  ages=dav2008T.data$age, deathProbs=dav2008T.data$qx1R)
dav2008t.male.smoker.2Ord=valuationTable.period(
  name="DAV 2008T male smoker, unloaded",
  ages=dav2008T.data$age, deathProbs=dav2008T.data$qx2R)
dav2008t.female.smoker=valuationTable.period(
  name="DAV 2008T female smoker, loaded",
  ages=dav2008T.data$age, deathProbs=dav2008T.data$qy1R)
dav2008t.female.smoker.2Ord=valuationTable.period(
  name="DAV 2008T female smoker, unloaded",
  ages=dav2008T.data$age, deathProbs=dav2008T.data$qy2R)

### DAV 2008T Non-Smoker
dav2008t.male.nonsmoker=valuationTable.period(
  name="DAV 2008T male non-smoker, loaded",
  ages=dav2008T.data$age, deathProbs=dav2008T.data$qx1NR)
dav2008t.male.nonsmoker.2Ord=valuationTable.period(
  name="DAV 2008T male non-smoker, unloaded",
  ages=dav2008T.data$age, deathProbs=dav2008T.data$qx2NR)
dav2008t.female.nonsmoker=valuationTable.period(
  name="DAV 2008T female non-smoker, loaded",
  ages=dav2008T.data$age, deathProbs=dav2008T.data$qy1NR)
dav2008t.female.nonsmoker.2Ord=valuationTable.period(
  name="DAV 2008T female non-smoker, unloaded",
  ages=dav2008T.data$age, deathProbs=dav2008T.data$qy2NR)

rm(dav2008T.data);
