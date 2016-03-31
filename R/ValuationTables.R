library("lifecontingencies");

# (virtual) base class for valuation tables, contains only the name / ID
valuationTable=setClass(
  "valuationTable", 
  slots=list(name="character"), 
  prototype=list(name="Actuarial Valuation Table"), 
  contains="VIRTUAL"
);


# A period life table, giving death probabilities for each age, up to 
# maximum age omega. Optionally apply selection factors to the probabilities
valuationTable.period=setClass(
  "valuationTable.period", 
  slots=list(ages="numeric", deathProbs="numeric"),
  prototype=list(ages=eval(0:120), deathProbs=rep(1,120)),
  contains="valuationTable"
);

# A cohort life table, obtained by age-shifting from a given base table (PODs 
# for a base YOB)
valuationTable.ageShift=setClass(
  "valuationTable.ageShift", 
  slots=list(ageShifts="data.frame"),
  prototype=list(ageShifts=data.frame(YOB=c(), shifts=c())),
  contains="valuationTable.period"
);

# A cohort life table, obtained by a trend projection from a given base table
# (PODs for a given observation year). Typically, the trend is obtained by
# the Lee-Carter method or some other trend estimation.
# The dampingFunction can be used to modify the cumulative years (e.g. G(tau+x) instead of tau+x)
# If trend2 is given, the G(tau+x) gives the weight of the first trend, 1-G(tau+x) the weight of the second trend
valuationTable.trendProjection=setClass(
  "valuationTable.trendProjection", 
  slots=list(baseYear="numeric", trend="numeric", dampingFunction="function", trend2="numeric"),
  prototype=list(baseYear=1980, trend=rep(0,120), dampingFunction=identity, trend2=0),
  contains="valuationTable.period"
);

# A cohort life table, obtained by an improvment factor projection 
# from a given base table (PODs for a given observation year).
valuationTable.improvementFactors=setClass(
  "valuationTable.improvementFactors", 
  slots=list(baseYear="numeric", improvement="numeric"),
  prototype=list(baseYear=2012, improvement=rep(0,120)),
  contains="valuationTable.period"
);

# A cohort life table described by actual observations (data frame of PODs 
# per year and age)
valuationTable.observed=setClass(
  "valuationTable.observed", 
  slots=list(data="data.frame"),
  prototype=list(data=data.frame()),
  contains="valuationTable"
);

# A cohort life table obtained by joining two cohort life tables, each of which
# applies only to certain observation years (e.g. for the past use the observed
# PODs, and project them to the future with the trend projection)
valuationTable.joined=setClass(
  "valuationTable.joined", 
  slots=list(
    table1="valuationTable", yearRange1="numeric", 
    table2="valuationTable", yearRange2="numeric"),
  contains="valuationTable"
);



setGeneric("getOmega", function(object) standardGeneric("getOmega"));
setMethod("getOmega", "valuationTable.period",
          function (object) {
            max(object@ages,na.rm=TRUE);
          })

setGeneric("deathProbabilities", function(object, ..., YOB=1975) standardGeneric("deathProbabilities"));
setMethod("deathProbabilities", "valuationTable.period",
          function(object, ..., YOB=1975) {
            object@deathProbs;
          })
setMethod("deathProbabilities","valuationTable.ageShift",
          function (object,  ..., YOB=1975) {
            qx=object@deathProbs;
            shift.index=match(YOB, object@shifts, 0);
            if (shift.index) {}
 #           shift=
            qx
          })
setMethod("deathProbabilities","valuationTable.trendProjection",
          function (object,  ..., YOB=1975) {
            qx=object@deathProbs;
            if (length(object@trend2)<=1) {
              exp(-object@trend*object@dampingFunction(YOB+0:(length(qx)-1)-object@baseYear))*qx
              #exp(-object@trend*object@dampingFunction(YOB+0:(length(qx)-1)-object@baseYear))
              #qx
            } else {
              # dampingFunction interpolates between the two trends:
              weights=sapply(YOB+0:(length(qx)-1), object@dampingFunction);
              qx*exp(-(object@trend*(1-weights) + object@trend2*(weights))*(YOB+0:(length(qx)-1)-object@baseYear))
            }
          })
setMethod("deathProbabilities","valuationTable.improvementFactors",
          function (object,  ..., YOB=1975) {
            qx=object@deathProbs;
            (1-object@improvement)^(YOB+0:(length(qx)-1)-object@baseYear)*qx
          })
setGeneric("lifeTable", function(object, ...) standardGeneric("lifeTable"));
setMethod("lifeTable","valuationTable",
          function (object,  ...) {
            qx=deathProbabilities(object, ...);
            if (qx[[length(qx)]]!=1) { qx=c(qx, 1, 1); }
            probs2lifetable(qx, type="qx")
          })


