library(R6);
library("lifecontingencies")


# base class for Insurance Tarifs (holding contrat-independent values and 
# providing methods to calculate cash flows, premiums, etc.). Objects of this
# class do NOT contain contract-specific values like age, death probabilities, 
# premiums, reserves, etc.
InsuranceTarif = R6Class(
  "InsuranceTarif", 
  public  = list(
    name  = "Insurance Contract Type",
    tarif = "Generic Tarif",
    desc  = "Description of the contract",
    
    states = c("Lebend", "Tot"),
    mortalityTable = NA,
    interest=0,

    initialize = function (name=NA, mortalityTable=NA, interest=NA,...) {
      if (!missing(name))           self$name = name;
      if (!missing(mortalityTable)) self$mortalityTable = mortalityTable;
      if (!missing(interest))       self$interest = interest;
      cat(paste0("Initializing Insurance Tarif ", self$name, "...\n"))
    },
    getTransitionProbabilities = function (age, ..., YOB=2000) {
      ages = ages(self$mortalityTable, YOB=YOB);
      q = deathProbabilities(self$mortalityTable, YOB=YOB);
      if (age>0) {
        ages = ages[-age:-1];
        q    = q[-age:-1];
      }
      p = 1-q;
      len = length(p);
      df=data.frame(p, q, rep(0,len), rep(1,len), row.names=ages)
      df
    },
    getCashFlowsState = function (age, ..., policyPeriod=inf, deferral=0, maxAge=150) {
      cf = list()
      # TODO
    },
    getCashFlowsTransition = function (age, ..., policyPeriod=inf, deferral=0, maxAge=150) {
      # TODO
    }
  )
);

TestTarif = InsuranceTarif$new(name="Testtarif", mortalityTable=AVOe2005R.male)
str(TestTarif)
TestTarif$getTransitionProbabilities(YOB=1980, age=30)


InsuranceContract = R6Class(
  "InsuranceContract",
  public = list(
    tarif = NA,
    YOB = NA,
    age = NA,
    policyPeriod = inf,
    deferral = 0,
    
    transitionProbabilities = NA,
    cashFlowsState = NA,
    cashFlowsTransition = NA,
    #    cashFlowsState = list(c(0), c(0)),
    #    cashFlowsTransition = list(matrix(c(0,1,0,1), 2, 2, byrow=TRUE)),
    
    initialize = function (tarif, age, policyPeriod=inf, ..., deferral=0, YOB=1975) {
      if (!missing(tarif)) self$tarif = tarif;
      if (!missing(age)) self$age = age;
      if (!missing(YOB)) self$YOB = YOB;
      if (!missing(deferral)) self$deferral = deferral;
      if (!missing(policyPeriod)) self$policyPeriod = policyPeriod;
      self$determineTransitionProbabilities();
      self$determineCashFlows();
    },
    
    determineTransitionProbabilities = function() {
      self$transitionProbabilities = self$tarif$getTransitionProbabilities(self$age, self$YOB);
    },
    
    determineCashFlows = function() {
      self$cashFlowsState = self$tarif$getCashFlowsState(age=self$age, YOB=self$YOB, policyPeriod=self$policyPeriod, deferral=self$deferral, maxAge=self$age+length(self$transitionProbabilities));
      self$cashFlowsTransition = self$tarif$getCashFlowsState(age=self$age, YOB=self$YOB, policyPeriod=self$policyPeriod, deferral=self$deferral, maxAge=self$age+length(self$transitionProbabilities));
      
    }
    
    


  )
);

setGeneric("setYOB", function(scale, ...) standardGeneric("setYOB"));
setMethod("setYOB", "InsuranceScale",
          function (scale, ..., YOB=1975) {
            scale@YOB=YOB;
            scale
          }
)

setGeneric("getTransitionProbabilities", function(scale, ...) standardGeneric("getTransitionProbabilities"));
setMethod("getTransitionProbabilities", "InsuranceScale",
          function (scale, ...) {
            q = deathProbabilities(scale@mortalityTable, scale@YOB);
            p = 1-q;
            len = length(p);
            df=data.frame(p, q, rep(0,len), rep(1,len), row.names=ages(scale@mortalityTable, scale@YOB))
          }
)


setGeneric("calculateTransitionProbabilities", function(scale, ...) standardGeneric("calculateTransitionProbabilities"));
setMethod("calculateTransitionProbabilities", "InsuranceScale",
          function (scale, ...) {
            scale@transitionProbabilities = getTransitionProbabilities(scale, ...);
            scale
          }
)

TestTarif = InsuranceScale(name="Testtarif", YOB=1980, age=30)
#TestTarif = setYOB(TestTarif, YOB=1980)

getTransitionProbabilities(TestTarif)

t=AVOe2005R.unisex
t@ages
t@deathProbs
qqq
qqq["1",]

mort=deathProbabilities(AVOe2005R.male, YOB=1977); mort

mort=deathProbabilities(AVOe2005R.unisex, YOB=1977); mort
q=mort
p=1-mort; p
len=length(p); len
qqq=data.frame(q=t@deathProbs, row.names=t@ages); qqq

df=data.frame("A-A"=p, "A-t"=q, "t-A"=rep(0, len), "t-t"=rep(1, len), row.names=t@ages)
df



# createCostStructure=function(age=35,contractLength=inf,
#   alphaVS,
#   alphaBP,
#   
# 
# CostStructure=setClass(
#   "CostStructure",
#   
# )


calcUnterjährigkeit = function(m=1,i=0, order=0) {
  alpha=1;
  beta=(m-1)/(2*m);
  if (order>=1)     { beta = beta + (m^2-1)/(6*m^2)*i;    }
  if (order == 1.5) { beta = beta + (1-m^2)/(12*m^2)*i^2; }
  if (order >= 2)   { beta = beta + (1-m^2)/(24*m^2)*i^2;
                      alpha= alpha+ (m^2-1)/(12*m^2)*i^2; }
  list(alpha=alpha, beta=beta);
}

setGeneric("createContractCashflows", function(object) standardGeneric("createContractCashflows"))

setGeneric("calculate", function(object) standardGeneric("calculate"));
setMethod("calculate", "InsuranceContract",
          function (object) {
            # 0) Prepare helper values
            # 0a: Unterjährigkeit
            m = object@unterjährigkeit;
            object@cache.uj=calcUnterjährigkeit(m=m, i=object@interest, order=object@unterjährigkeitsapproximation);
            
            
            # 1) Generate mortality table
            if (!is.null(object@contractLength) && is.finite(object@contractLength)) {
              ages = (object@age):(object@contractLength);
            } else {
              ages = (object@age):150;
            }
            qx = deathProbabilities(object@mortalityTable, YOB=object@YOB)[ages+1];
            pxn = cumprod(1-qx);
            object@probabilities = data.frame(ages=ages,qx=qx, pxn=pxn)
            if (!is.null(object@YOB2)) {
              ages2 = ages - object@age + object@age2;
              qx2 = deathProbabilities(object@mortalityTable2, YOB=object@YOB2)[ages2+1];
              pxn2 = cumprod(1-qx2);
              pxyn = pxn * pxn2;
              object@probabilities = data.frame(object@probabilities, ages2=ages2, q2=qx2, pxn2=pxn2, pxyn=pxyn);
            }
            
            
            # 2) Properly set up the payment and cost cash flow data frames
            
            # 3) Calculate all NPVs of the payment and the cost cash flows (recursively)
            # 3a: life-long annuities for person 2 (and person 1+2), in case the death benefit is a life-long widow's annuity
            
            # 4) Set up the coefficients of the NPVs for the premium calculation
            
            # 5) Calculate the gross premium
            # 6) Calculate the net premium and the Zillmer premium
            
            # 7) Calculate all reserves (benefits and costs)
            
            # 8) Calculate Spar- und Risikoprämie from the net premium and the reserves
            
            # 9) Calculate VS after Prämienfreistellung
            # 9a: Calculate all reserves after Prämienfreistellung
            
            # 10) Calculate the Bilanz reserves
            
            # 11) Calculate the Rückkaufswert
            #   max(object@ages,na.rm=TRUE);
            object
          })


beispielvertrag = InsuranceContract(
  name="Beispielvertrag", tarif="Beispieltarif",
  desc="Beispiel zum Testen des Codes",
  YOB=1948, YOB2=1948+65-62,
  age=65,   age2=62,
  #     contractLength=30,
  mortalityTable=AVOe2005R.unisex, mortalityTable2=AVOe2005R.unisex,
  interest=0.0125,
  
  unterjährigkeit=12, unterjährigkeitsapproximation=1.5,
  
  #     deathPayments=list(),
  #     survivalPayments=list(),
  #     costCashflows=data.frame(),
  #     cashflows=data.frame()
);
beispielvertrag=calculate(beispielvertrag)
beispielvertrag
# data.frame(x=0:121, qx=deathProbabilities(AVOe2005R.unisex, YOB=1948))