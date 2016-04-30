library("lifecontingencies");
library(ggplot2);

# (virtual) base class for valuation tables, contains only the name / ID
valuationTable=setClass(
  "valuationTable",
  slots=list(name="character", baseYear="numeric", loading="numeric", modification="function"),
  prototype=list(name="Actuarial Valuation Table", baseYear=2000, loading=0, modification=identity),
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
# A cohort life table obtained by mixing two life tables with the given weights
valuationTable.mixed=setClass(
  "valuationTable.mixed",
  slots=c(table1="valuationTable", table2="valuationTable", weight1="numeric", weight2="numeric", loading="numeric"),
  prototype=list(weight1=1/2, weight2=1/2, loading=0),
  contains="valuationTable"
);



setGeneric("getOmega", function(object) standardGeneric("getOmega"));
setMethod("getOmega", "valuationTable.period",
          function (object) {
            max(object@ages,na.rm=TRUE);
          })
setMethod("getOmega", "valuationTable.mixed",
          function (object) {
            getOmega(object@table1);
          })
setMethod("getOmega", "valuationTable.joined",
          function (object) {
            getOmega(object@table1);
          })

setGeneric("ages", function(object, ...) standardGeneric("ages"));
setMethod("ages", "valuationTable.period",
          function (object, ...) {
            object@ages;
          })
setMethod("ages", "valuationTable.mixed",
          function (object, ...) {
            ages(object@table1);
          })
setMethod("ages", "valuationTable.joined",
          function (object, ...) {
            ages(object@table1);
          })

setGeneric("ageShift", function(object, YOB=1975, ...) standardGeneric("ageShift"));
setMethod("ageShift","valuationTable.ageShift",
          function(object, YOB, ...) {
            shift = object@ageShifts[toString(YOB),];
            if (is.na(shift)) {
              # The row names (YOB) are unfortunately strings, so we cannot easily query them.
              # TODO: Change the data.frame to use a real column for the YOB
              firstYOB = head(rownames(object@ageShifts), n=1);
              lastYOB = tail(rownames(object@ageShifts), n=1);
              if (YOB < as.integer(firstYOB)) {
                shift = object@ageShifts[firstYOB,];
              } else if (YOB > as.integer(lastYOB)) {
                shift = object@ageShifts[lastYOB,];
              }
            }
            shift
          })

setGeneric("deathProbabilities", function(object, ..., YOB=1975) standardGeneric("deathProbabilities"));
setMethod("deathProbabilities", "valuationTable.period",
          function(object, ..., YOB=1975) {
            object@modification(object@deathProbs * (1+object@loading));
          })
setMethod("deathProbabilities","valuationTable.ageShift",
          function (object,  ..., YOB=1975) {
            qx=object@deathProbs * (1+object@loading);
            shift = ageShift(object, YOB);
            if (shift>0) {
              qx = c(qx[(shift+1):length(qx)], rep(qx[length(qx)], shift));
            } else if (shift<0) {
              qx = c(rep(0, -shift), qx[1:(length(qx)-(-shift))])
            }
            object@modification(qx)
          })


setMethod("deathProbabilities","valuationTable.trendProjection",
          function (object,  ..., YOB=1975) {
            qx=object@deathProbs * (1+object@loading);
            if (is.null(object@trend2) || length(object@trend2)<=1) {
              ages=0:(length(qx)-1);
              damping=sapply(ages, function (age) { object@dampingFunction(YOB+age-object@baseYear) });
              # print(data.frame(age=0:(length(qx)-1), trend=object@trend, exponent=-object@trend*damping, damping=damping, baseqx=qx, qx=exp(-object@trend*damping)*qx)[66:90,]);
              finalqx=exp(-object@trend*damping)*qx;
            } else {
              # dampingFunction interpolates between the two trends:
              weights=sapply(YOB+0:(length(qx)-1), object@dampingFunction);
              finalqx=qx*exp(-(object@trend*(1-weights) + object@trend2*(weights))*(YOB+0:(length(qx)-1)-object@baseYear))
            }
            object@modification(finalqx)
          })

setMethod("deathProbabilities","valuationTable.improvementFactors",
          function (object,  ..., YOB=1975) {
            qx=object@deathProbs * (1+object@loading);
            finalqx=(1-object@improvement)^(YOB+0:(length(qx)-1)-object@baseYear)*qx;
            object@modification(finalqx)
          })
setMethod("deathProbabilities","valuationTable.mixed",
          function (object,  ..., YOB=1975) {
            qx1=deathProbabilities(object@table1, ..., YOB) * (1+object@loading);
            qx2=deathProbabilities(object@table2, ..., YOB) * (1+object@loading);
            mixedqx=(object@weight1*qx1 + object@weight2*qx2)/(object@weight1 + object@weight2);
            object@modification(mixedqx)
          })


setGeneric("periodDeathProbabilities", function(object, ...) standardGeneric("periodDeathProbabilities"));
setMethod("periodDeathProbabilities", "valuationTable.period",
          function(object, ...) {
            object@modification(object@deathProbs * (1+object@loading));
          })
setMethod("periodDeathProbabilities","valuationTable.ageShift",
          function (object,  ..., Period=1975) {
            # TODO
            qx=object@deathProbs * (1+object@loading);
            shift.index=match(YOB, object@shifts, 0);
            if (shift.index) {}
            #             TODO
            object@modification(qx)
          })
setMethod("periodDeathProbabilities","valuationTable.trendProjection",
          function (object,  ..., Period=1975) {
            qx=object@deathProbs * (1+object@loading);
            if (is.null(object@trend2) || length(object@trend2)<=1) {
              # ages=0:(length(qx)-1);
              damping=object@dampingFunction(Period-object@baseYear);
              # print(data.frame(age=0:(length(qx)-1), trend=object@trend, exponent=-object@trend*damping, damping=damping, baseqx=qx, qx=exp(-object@trend*damping)*qx)[66:90,]);
              finalqx=exp(-object@trend*damping)*qx;
            } else {
              # TODO
              # dampingFunction interpolates between the two trends:
              weights=sapply(YOB+0:(length(qx)-1), object@dampingFunction);
              finalqx=qx*exp(-(object@trend*(1-weights) + object@trend2*(weights))*(YOB+0:(length(qx)-1)-object@baseYear));
            }
            object@modification(finalqx)
          })
# data.frame(x=0:121, qx=deathProbabilities(AVOe2005R.unisex, YOB=1948));
setMethod("periodDeathProbabilities","valuationTable.improvementFactors",
          function (object, ..., Period=1975) {
            qx=object@deathProbs * (1+object@loading);
            finalqx=(1-object@improvement)^(Period-object@baseYear)*qx;
            object@modification(finalqx)
          })
setMethod("periodDeathProbabilities","valuationTable.mixed",
          function (object,  ..., Period=1975) {
            qx1=periodDeathProbabilities(object@table1, ..., Period=Period) * (1+object@loading);
            qx2=periodDeathProbabilities(object@table2, ..., Period=Period) * (1+object@loading);
            mixedqx=(object@weight1*qx1 + object@weight2*qx2)/(object@weight1 + object@weight2);
            object@modification(mixedqx)
          })




setGeneric("lifeTable", function(object, ...) standardGeneric("lifeTable"));
setMethod("lifeTable","valuationTable",
          function (object,  ...) {
            qx=deathProbabilities(object, ...);
            if (qx[[length(qx)]]!=1) { qx=c(qx, 1, 1); }
            probs2lifetable(qx, type="qx")
          })


setGeneric("baseYear", function(object, ...) standardGeneric("baseYear"));
setMethod("baseYear","valuationTable",
          function (object,  ...) {
            object@baseYear
          })
setMethod("baseYear","valuationTable.mixed",
          function (object,  ...) {
            baseYear(object@table1)
          })

setGeneric("baseTable", function(object, ...) standardGeneric("baseTable"));
setMethod("baseTable","valuationTable",
          function (object,  ...) {
            c()
          })
setMethod("baseTable","valuationTable.period",
          function (object,  ...) {
            object@deathProbs
          })


setGeneric("getPeriodTable", function(object, Period, ...) standardGeneric("getPeriodTable"));
setMethod("getPeriodTable","valuationTable",
          function (object, Period, ...) {
            valuationTable.period(
                name = paste(object@name, ", Period ", Period),
                baseYear = Period,
                ages = ages(object),
                deathProbs = periodDeathProbabilities(object, Period=Period)
            )
          })

setGeneric("getCohortTable", function(object, Period, ...) standardGeneric("getCohortTable"));
setMethod("getCohortTable","valuationTable",
          function (object, YOB, ...) {
            valuationTable.period(
                name = paste(object@name, ", YOB ", YOB),
                baseYear = YOB,
                ages=ages(object),
                deathProbs=deathProbabilities(object, YOB=YOB)
            );
          })


setGeneric("undampenTrend", function (object) standardGeneric("undampenTrend"));
setMethod("undampenTrend", "valuationTable.trendProjection",
          function (object) {
            object@dampingFunction=identity;
            object
            });


makeQxDataFrame = function(..., YOB=1972, Period=NA) {
  data=list(...);
  names(data) = lapply(data, function(t) t@name);
  if (missing(Period)) {
    cat("Year of birth: ", YOB, "\n");
    data = lapply(data, function(t) cbind(x=ages(t), y=deathProbabilities(t, YOB=YOB)))
  } else {
    cat("Period: ", Period,"\n");
    data = lapply(data, function(t) cbind(x=ages(t), y=periodDeathProbabilities(t, Period=Period)))
  }

  list.names = names(data)
  lns <- sapply(data, nrow)
  data <- as.data.frame(do.call("rbind", data))
  data$group <- rep(list.names, lns)
  data
}

plotValuationTables = function(data, ..., title = "", legend.position=c(0.9,0.1), legend.key.width = unit(25, "mm")) {
  if (!is.data.frame(data)) {
    data = makeQxDataFrame(data, ...);
  }

  pl = ggplot(data, aes(x = x, y = y, colour = data$group)) +
    theme_bw() +
    theme(
      plot.title = element_text(size=18, face="bold"),
      legend.title = element_text(size=14, face="bold.italic"),
      # legend in bottom right corner of the plot
      legend.justification=c(1,0), legend.position=legend.position,
      # No box around legend entries
      legend.key = element_blank(),
      legend.key.width = legend.key.width,
      legend.background = element_rect(colour="gray50", linetype="solid")
    ) +
    geom_line() +
    scale_y_log10(
      name=expression(paste("Sterbewahrscheinlichkeit  ", q[x])),
      breaks = scales::trans_breaks('log10', function(x) 10^x),
      labels = scales::trans_format('log10', scales::math_format(10^.x))
      #minor_breaks = log(c(sapply(x, function(x) seq(0, x, x/10))), 10)
    ) +
    scale_x_continuous(
      name="Alter",
      #breaks = function (limits) scales::trans_breaks('', function(x) 10^x),
      breaks = function (limits) seq(max(min(limits),0),max(limits),5),
      minor_breaks = function (limits) seq(max(round(min(limits)),0),round(max(limits)),1),
      #labels = scales::trans_format('log10', scales::math_format(10^.x))

    ) +
    annotation_logticks(sides="lr") +
    xlab("Alter") + labs(colour="Sterbetafel");
  if (title != "") {
    pl = pl + ggtitle(title);
  }
  pl# + coord_flip()
}
#
# plotValuationTables(mort.AT.census.1869.male, mort.AT.census.1869.female, mort.AT.census.2011.male, mort.AT.census.2011.female, AVOe2005R.male, AVOe2005R.female, YOB=1972,title="Vergleich österreichische Sterbetafeln, YOB=1972 (bei Generationentafeln)")
#
# plotValuationTables(mort.AT.census.2001.male, AVOe2005R.male, YOB=1972, title="Vergleich österreichische Sterbetafeln")
#  plotValuationTables(getCohortTable(AVOe2005R.male, YOB=1972), getCohortTable(AVOe2005R.male, YOB=2016), title="Vergleich österreichische Sterbetafeln")
