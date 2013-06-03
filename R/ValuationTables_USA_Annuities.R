frame_files = lapply(sys.frames(), function(x) x$ofile)
frame_files = Filter(Negate(is.null), frame_files)
PATH <- dirname(frame_files[[length(frame_files)]])
rm(frame_files)
setwd(dirname(PATH))

library("gdata")


###############################################################################
### 1971 IAM individual annuity table; with optional projection scale B_x
###############################################################################

USA1971IAM.data=read.xls(
  "Tafeln/USA_Annuities.xls", 
  sheet="1971 IAM", skip=2, #row.names=1, 
  col.names=c("age","qx", "qy", "B"));

USA1971IAM.male=valuationTable.period (
  name="USA 1971 IAM, male",
  ages=USA1971IAM.data$age,
  deathProbs=USA1971IAM.data$qx)
USA1971IAM.female=valuationTable.period (
  name="USA 1971 IAM, female",
  ages=USA1971IAM.data$age,
  deathProbs=USA1971IAM.data$qy)

USA1971IAM.male.projected=valuationTable.improvementFactors (
  name="USA 1971 IAM, male",
  ages=USA1971IAM.data$age, baseYear=1971,
  deathProbs=USA1971IAM.data$qx,
  improvement=USA1971IAM.data$B)
USA1971IAM.female.projected=valuationTable.improvementFactors (
  name="USA 1971 IAM, female",
  ages=USA1971IAM.data$age, baseYear=1971,
  deathProbs=USA1971IAM.data$qy,
  improvement=USA1971IAM.data$B)

rm(USA1971IAM.data);


###############################################################################
### 1983 Table "a" (individual) and GAM (group annuities), period tables
###############################################################################

USA1983a.data=read.xls(
  "Tafeln/USA_Annuities.xls", 
  sheet="1983a - GAM", skip=2,
  col.names=c("age","qx", "qy", "qxG", "qyG"));

USA1983a.male=valuationTable.period (
  name="USA 1983 Table a, male",
  ages=USA1983a.data$age,
  deathProbs=USA1983a.data$qx)
USA1983a.female=valuationTable.period (
  name="USA 1983 Table a, female",
  ages=USA1983a.data$age,
  deathProbs=USA1983a.data$qy)

USA1983GAM.male=valuationTable.period (
  name="USA 1983 GAM, male",
  ages=USA1983a.data$age,
  deathProbs=USA1983a.data$qxG)
USA1983GAM.female=valuationTable.period (
  name="USA 1983 GAM, female",
  ages=USA1983a.data$age,
  deathProbs=USA1983a.data$qyG)

rm(USA1983a.data);


###############################################################################
### 1994 GAR/GAM group annuity tables, with improvement factors AA_x
###############################################################################

USA1994GAM.data=read.xls(
  "Tafeln/USA_Annuities.xls", 
  sheet="1994 GAR", skip=2,
  col.names=c("age","qx", "AAx", "qy", "AAy", "qxBasic", "qyBasic"));

USA1994GAM.male.basic=valuationTable.period (
  name="USA 1994 GAM basic (unloaded), male",
  ages=USA1994GAM.data$age,
  deathProbs=USA1994GAM.data$qxBasic)
USA1994GAM.female.basic=valuationTable.period (
  name="USA 1994 GAM basic (unloaded), female",
  ages=USA1994GAM.data$age,
  deathProbs=USA1994GAM.data$qyBasic)

USA1994GAR.male=valuationTable.improvementFactors (
  name="USA 1994 GAM, male",
  ages=USA1994GAM.data$age,
  deathProbs=USA1994GAM.data$qx,
  improvement=USA1994GAM.data$AAx)
USA1994GAR.female=valuationTable.improvementFactors (
  name="USA 1994 GAM, female",
  ages=USA1994GAM.data$age,
  deathProbs=USA1994GAM.data$qy,
  improvement=USA1994GAM.data$AAy)

rm(USA1994GAM.data);


###############################################################################
### Annuity 2000 Basic (unloaded) and Mortality (loaded) Tables, PERIOD tables
###############################################################################

USAAnnuity2000.data=read.xls(
  "Tafeln/USA_Annuities.xls", 
  sheet="Annuity 2000", skip=2, #row.names=1, 
  col.names=c("age","qxBasic", "qyBasic", "qx", "qy"));

USAAnnuity2000.basic.male=valuationTable.period (
  name="USA Annuity 2000 basic, male",
  ages=USAAnnuity2000.data$age,
  deathProbs=USAAnnuity2000.data$qxBasic)
USAAnnuity2000.basic.female=valuationTable.period (
  name="USA Annuity 2000 basic, female",
  ages=USAAnnuity2000.data$age,
  deathProbs=USAAnnuity2000.data$qyBasic)

USAAnnuity2000.male.projected=valuationTable.period (
  name="USA Annuity 2000, male",
  ages=USAAnnuity2000.data$age,
  deathProbs=USAAnnuity2000.data$qx)
USAAnnuity2000.female.projected=valuationTable.period (
  name="USA Annuity 2000, female",
  ages=USAAnnuity2000.data$age,
  deathProbs=USAAnnuity2000.data$qy)

rm(USAAnnuity2000.data);


###############################################################################
### 1994 GAR/GAM group annuity tables, with improvement factors AA_x
###############################################################################

USA2012IAM.data=read.xls(
  "Tafeln/USA_Annuities.xls", 
  sheet="2012 IAR", skip=3,
  col.names=c("age","qxBasic", "qyBasic", "qx", "qy", "G2x", "G2y", "", "", ""));

USA2012IAM.male.basic=valuationTable.period (
  name="USA 2012 IAM basic (unloaded), male",
  ages=USA2012IAM.data$age,
  deathProbs=USA2012IAM.data$qxBasic)
USA2012IAM.female.basic=valuationTable.period (
  name="USA 2012 IAM basic (unloaded), female",
  ages=USA2012IAM.data$age, omega=max(USA2012IAM.data$age,rm.na=TRUE)+1,
  deathProbs=USA2012IAM.data$qyBasic)

USA2012IAM.male=valuationTable.improvementFactors (
  name="USA 2012 IAM, male",
  ages=USA2012IAM.data$age, omega=max(USA2012IAM.data$age,rm.na=TRUE)+1,
  deathProbs=USA2012IAM.data$qx,
  improvement=USA2012IAM.data$G2x)
USA2012IAM.female=valuationTable.improvementFactors (
  name="USA 2012 IAM, female",
  ages=USA2012IAM.data$age, omega=max(USA2012IAM.data$age,rm.na=TRUE)+1,
  deathProbs=USA2012IAM.data$qy,
  improvement=USA2012IAM.data$G2y)

rm(USA2012IAM.data);

