frame_files = lapply(sys.frames(), function(x) x$ofile)
frame_files = Filter(Negate(is.null), frame_files)
PATH <- dirname(frame_files[[length(frame_files)]])
rm(frame_files)
setwd(dirname(PATH))

library("gdata")



###############################################################################
### RR67 Rententafel für Männer, 3%
###############################################################################

rr67.data=read.xls(
  "Tafeln/AVOe_R.xls", 
  sheet="OeVM59-61 RR67", skip=1, #row.names=1, 
  col.names=c("age","qx"));

rr67=valuationTable.period(
  name="ÖVM 59/61 RR67", ages=rr67.data$age, deathProbs=rr67.data$qx
);
# rm(rr67.data);


###############################################################################
### EROM/EROF 85 and G 1985 (period and age-shifted generation)
###############################################################################

eromf.data=read.xls(
  "Tafeln/AVOe_R.xls", 
  sheet="EROM-F Basistafeln", skip=2, #row.names=1, 
  col.names=c("age", "EROM85", "EROF85", "EROMG1950", "EROFG1950")
);


erom85.male=valuationTable.period(
  name="EROM 85, male", ages=eromf.data$age, deathProbs=eromf.data$EROM85
);
erom85.female=valuationTable.period(
  name="EROF 85, female", ages=eromf.data$age, deathProbs=eromf.data$EROF85
);

EROM.G1950.male=valuationTable.period(
  name="EROM G 1950 Basistafel, male",
  ages=eromf.data$age,
  deathProbs=eromf.data$EROMG1950
);
EROF.G1950.female=valuationTable.period(
  name="EROF G 1950 Basistafel, female",
  ages=eromf.data$age,
  deathProbs=eromf.data$EROFG1950
);

eromf.data.av=read.xls(
  "Tafeln/AVOe_R.xls", 
  sheet="EROM-F G AV", skip=1, row.names=1, 
 col.names=c("YOB", "shiftM", "shiftF")
);

EROM.G1950.male.av=valuationTable.ageShift(
  name="EROM G 1950 mit Altersverschiebung, male",
  ages=eromf.data$age,
  deathProbs=eromf.data$EROMG1950,
  ageShifts=eromf.data.av[1]
);
EROF.G1950.female.av=valuationTable.ageShift(
  name="EROF G 1950 mit Altersverschiebung, female",
  ages=eromf.data$age,
  deathProbs=eromf.data$EROFG1950,
  ageShifts=eromf.data.av[2]
);


###############################################################################
# AVÖ 1996R exact (Male, Female), 1st-order only
###############################################################################

avoe1996r.exakt.data=read.xls(
  "Tafeln/AVOe_R.xls", 
  sheet="AVOe 1996R exakt", skip=2, #row.names=1, 
  col.names=c("age",
              "q1991M", "trendM.long", "trendM.short", "factorMG", "factorM",
              "",
              "q1991F", "trendF.long", "trendF.short", "factorFG", "factorF",
              rep("",12))
);
avoe1996r.trend.switching=function(year) {
  if (year<=1971) {
    15/(1991-year)
  } else if (1971<year && year<1981) {
    1+(year-1981)^2/(year-1991)/20
  } else if (1981<=year && year<=2000) {
    1
  } else if (2000<year && year<2010) {
    1-(year-2000)^2/(year-1991)/20
  } else if (year>=2010) {
    14/(year-1991)
  }
}

AVÖ1996R.male=valuationTable.trendProjection(
  name="AVÖ 1996R male",
  ages=avoe1996r.exakt.data$age, baseYear=1991,
  deathProbs=avoe1996r.exakt.data$q1991M*avoe1996r.exakt.data$factorM, 
  trend=avoe1996r.exakt.data$trendM.long,
  trend2=avoe1996r.exakt.data$trendM.short,
  dampingFunction=avoe1996r.trend.switching
);
AVÖ1996R.female=valuationTable.trendProjection(
  name="AVÖ 1996R female",
  ages=avoe1996r.exakt.data$age, baseYear=1991,
  deathProbs=avoe1996r.exakt.data$q1991F*avoe1996r.exakt.data$factorF, 
  trend=avoe1996r.exakt.data$trendF.long,
  trend2=avoe1996r.exakt.data$trendF.short,
  dampingFunction=avoe1996r.trend.switching
);
AVÖ1996R.male.group=valuationTable.trendProjection(
  name="AVÖ 1996R male, group",
  ages=avoe1996r.exakt.data$age, baseYear=1991,
  deathProbs=avoe1996r.exakt.data$q1991M*avoe1996r.exakt.data$factorMG, 
  trend=avoe1996r.exakt.data$trendM.long,
  trend2=avoe1996r.exakt.data$trendM.short,
  dampingFunction=avoe1996r.trend.switching
);
AVÖ1996R.female.group=valuationTable.trendProjection(
  name="AVÖ 1996R female, group",
  ages=avoe1996r.exakt.data$age, baseYear=1991,
  deathProbs=avoe1996r.exakt.data$q1991F*avoe1996r.exakt.data$factorFG, 
  trend=avoe1996r.exakt.data$trendF.long,
  trend2=avoe1996r.exakt.data$trendF.short,
  dampingFunction=avoe1996r.trend.switching
);



###############################################################################
# AVÖ 2005R exact (Male, Female, unisex)
# gender-specific tables also have 2nd-order tables, unisex only 1st-order table
###############################################################################

avoe2005r.exakt.data=read.xls(
  "Tafeln/AVOe_R.xls", 
  sheet="AVOe 2005R", skip=1, #row.names=1, 
  header=FALSE,
  col.names=c("age", 
              "q2001M","q2001MG", "trendM", 
              "q2001F", "q2001FG", "trendF", 
              "", 
              "q2001M.2Ord", "2001MG.2Ord", "trendM.2Ord", 
              "q2001F.2Ord", "q2001FG.2Ord", "trendF.2Ord", 
              "", 
              "q2001U", "q2001UG", "trendU")
);

avoe2005r.trend.damping=function(t) {
  100*atan(t/100)
}
avoe2005r_gen=function(nm, probs, trend) {
  with(
    avoe2005r.exakt.data,
    valuationTable.trendProjection(
      name=nm,
      ages=age, baseYear=2001,
      deathProbs=avoe2005r.exakt.data[[probs]], trend=avoe2005r.exakt.data[[trend]],
      dampingFunction=avoe2005r.trend.damping
    )
  )
}

avoe2005r.male  =avoe2005r_gen("AVÖ 2005R male (exact), loaded",   "q2001M", "trendM");
avoe2005r.female=avoe2005r_gen("AVÖ 2005R female (exact), loaded", "q2001F", "trendF");
avoe2005r.unisex=avoe2005r_gen("AVÖ 2005R unisex (exact), loaded", "q2001U", "trendU");
avoe2005r.male.unloaded  =avoe2005r_gen("AVÖ 2005R male (exact), unloaded",   "q2001M.2Ord", "trendM.2Ord");
avoe2005r.female.unloaded=avoe2005r_gen("AVÖ 2005R female (exact), unloaded", "q2001F.2Ord", "trendF.2Ord");
avoe2005r.male.group  =avoe2005r_gen("AVÖ 2005R male group (exact), loaded",   "q2001MG", "trendM");
avoe2005r.female.group=avoe2005r_gen("AVÖ 2005R female group (exact), loaded", "q2001FG", "trendF");
avoe2005r.unisex.group=avoe2005r_gen("AVÖ 2005R unisex group (exact), loaded", "q2001UG", "trendU");


###############################################################################
#AVÖ 2005R with age-shifting (Male, Female, unisex), 1st-order only
###############################################################################

avoe2005r.av.base=read.xls(
  "Tafeln/AVOe_R.xls", 
  sheet="AVOe 2005R AV Basistafel", skip=1, # row.names=1,
  col.names=c("age", "q1965M", "q1965MG", "q1965F", "q1965FG", "q1972U", "q1972UG")
);

avoe2005r.av.verschiebung=read.xls(
  "Tafeln/AVOe_R.xls", 
  sheet="AVOe 2005R AV Verschiebung",skip=2,row.names=1,
  col.names=c("YOB", "shiftM", "shiftMG", "shiftF", "shiftFG", "shiftU", "shiftUG")
)

avoe2005r_gen.av=function(nm, probs, shft) {
  valuationTable.ageShift(
    name=nm,
    ages=avoe2005r.av.base$age,
    deathProbs=avoe2005r.av.base[[probs]],
    ageShifts=na.omit(avoe2005r.av.verschiebung[shft])
  )
}

avoe2005r.male.av  =avoe2005r_gen.av("AVÖ 2005R male (age-shifted), loaded",   "q1965M", "shiftM");
avoe2005r.female.av=avoe2005r_gen.av("AVÖ 2005R female (age-shifted), loaded", "q1965F", "shiftF");
avoe2005r.unisex.av=avoe2005r_gen.av("AVÖ 2005R unisex (age-shifted), loaded", "q1972U", "shiftU");
avoe2005r.male.group.av  =avoe2005r_gen.av("AVÖ 2005R male group (age-shifted), loaded",   "q1965MG", "shiftMG");
avoe2005r.female.group.av=avoe2005r_gen.av("AVÖ 2005R female group (age-shifted), loaded", "q1965FG", "shiftFG");
avoe2005r.unisex.group.av=avoe2005r_gen.av("AVÖ 2005R unisex group (age-shifted), loaded", "q1972UG", "shiftUG");



###############################################################################
