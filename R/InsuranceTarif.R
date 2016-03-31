library("lifecontingencies");


# (virtual) base class for valuation tables, contains only the name / ID
InsuranceTarif=setRefClass(
  "InsuranceContract", 
  slots=c(
    name="character",
    tarif="character",
    desc="character",
    YOB="numeric",
    YOB2="numeric",
    age="numeric",
    age2="numeric",
    contractLength="numeric",
    mortalityTable="valuationTable",
    mortalityTable2="valuationTable",
    
    interest="numeric",
    
    #     cashflows="data.frame",
    #     deathPayments="list",
    #     survivalPayments="list",
    #     costCashflows="data.frame",
    cashflows="data.frame",
    
    probabilities="data.frame",
    
    
    unterjährigkeit="numeric",
    unterjährigkeitsapproximation="numeric",
    cache.uj="list"
    
    
  ), 
  prototype=list(
    name="Insurance Contract Type",
    tarif="Tariff",
    desc="Description of the contract",
    YOB=1977,
    #     YOB2=1977,
    age=35,
    #     age2=35,
    contractLength=Inf,
    mortalityTable=AVOe2005R.unisex,
    #     mortalityTable2=AVOe2005R.unisex,
    
    interest=0,
    
    deathPayments=list(),
    survivalPayments=list(),
    costCashflows=data.frame(),
    cashflows=data.frame(),
    probabilities=data.frame(),
    
    unterjährigkeit=1,
    unterjährigkeitsapproximation=1
  )
);

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