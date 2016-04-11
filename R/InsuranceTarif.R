library(R6);
library(lifecontingencies)
library(objectProperties)
library(foreach)

TariffTypeEnum = setSingleEnum("TariffType", levels = c("annuity", "wholelife", "endowment", "pureendowment"))
PaymentTimeEnum = setSingleEnum("PaymentTime", levels = c("in advance", "in arrears"))
#PaymentCountEnum = setSingleEnum(PaymentCount, levels = c(1,2,3))


# Initialize a cost matrix with dimensions: [CostType, Basis, Period], with:
#     CostType: alpha, Zillmer, beta, gamma
#     Basis:    SumInsured, SumPremiums, GrossPremium
#     Period:   once, premiumPeriod, policyPeriod
# TODO: gamma an Erlebensleistungen?
initializeCosts = function() {
  dimnm=list(
    c("alpha", "Zillmer", "beta", "gamma", "gamma_nopremiums"),
    c("SumInsured", "SumPremiums", "GrossPremium"),
    c("once", "PremiumPeriod", "PremiumFree", "PolicyPeriod")
  );
  array(0,
        dim=sapply(dimnm, length),
        dimnames=dimnames
      )
}


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

    states = c("alive", "dead"),
    mortalityTable = NA,
    i = 0, # guaranteed interest rate
    v = 1, # discount factor
    tariffType = TariffTypeEnum("wholelife"), # possible values: annuity, wholelife, endowment, pureendowment
    benefitPaymentsPerYearOrder = 0,
    widowFactor = 0,

    premiumRefund = 0,
    premiumRefundSpread = 0,

    advanceBonus = 0,
    sumRebate = 0,

    costs = list(),
    paymentsPerYearSurcharge = list("1" = 0.0, "2" = 0.01, "4" = 0.015, "12" = 0.02),
    surcharges = list("tax" = 0.04, "unitcosts" = 0, "security" = 0, "noMedicalExam" = 0),


    initialize = function(name = NA, mortalityTable = NA, i = NA, type = "wholelife", ..., paymentsPerYearOrder = 0, costs) {
      if (!missing(name))           self$name = name;
      if (!missing(mortalityTable)) self$mortalityTable = mortalityTable;
      if (!missing(i))              self$i = i;
      if (!missing(type))           self$tariffType = type;
      self$costs = if (!missing(costs)) costs else initializeCosts();
      if (!missing(paymentsPerYearOrder)) self$paymentsPerYearOrder = paymentsPerYearOrder;

      self$v = 1/(1+self$i);

      cat(paste0("Initializing Insurance Tarif ", self$name, "...\n"));
    },
    getAges = function(age, ..., YOB = 2000) {
      ages = ages(self$mortalityTable, YOB = YOB);
      if (age > 0) {
        ages = ages[-age:-1];
      }
      ages
    },
    getTransitionProbabilities = function(age, ..., YOB = 2000) {
      ages = self$getAges(age, YOB = YOB);
      q = deathProbabilities(self$mortalityTable, YOB = YOB);
      if (age > 0) {
        q    = q[-age:-1];
      }
      # p = 1 - q;
      # len = length(p);
      # df = data.frame(p, q=q, rep(0,len), rep(1,len), row.names = ages)
      df = data.frame(age=ages, q=q, row.names = ages-age)
      df
    },
    getBasicCashFlows = function(age, ..., guaranteed = 0, policyPeriod = inf, deferral = 0, maxAge = getOmega(self$mortalityTable)) {
      maxlen = min(maxAge - age, policyPeriod);

      cf = list(
        guaranteed = rep(0, maxlen+1),
        survival = rep(0, maxlen+1),
        death = rep(0, maxlen+1)
      );
      if (self$tariffType == "annuity") {
        # guaranteed payments exist only with annuities (first n years of the payment)
        cf$guaranteed = c(rep(0, deferral), rep(1, guaranteed), rep(0, max(0, maxlen+1 - deferral - guaranteed)))
        cf$survival = c(rep(0, deferral + guaranteed), rep(1, max(0, maxlen+1 - deferral - guaranteed)))
      } else {
        if (self$tariffType == "endowment" || self$tariffType == "pureendowment") {
          cf$survival = c(rep(0, policyPeriod), 1);
        }
        if (self$tariffType == "endowment" || self$tariffType == "wholelife") {
          cf$death = c(rep(0, deferral), rep(1, maxlen - deferral), 0);
        }
      }
      cf
    },

    getCashFlows = function(age, ..., premiumPayments = "in advance", benefitPayments = "in advance", guaranteed = 0, policyPeriod=Inf, premiumPaymentPeriod = policyPeriod, deferral=0, maxAge = getOmega(self$mortalityTable), basicCashFlows = NA) {
      if (missing(basicCashFlows)) {
        basicCashFlows = self$getBasicCashFlows(age = age, ..., guaranteed = guaranteed,
              policyPeriod = policyPeriod, deferral = deferral, maxAge = maxAge);
      }
      cflen = length(basicCashFlows$survival);
      zeroes = pad0(0, cflen);
      ages = pad0(self$getAges(age, YOB = YOB), cflen);
      cf = data.frame(
        premiums_advance = zeroes,
        premiums_arrears = zeroes,
        guaranteed_advance = zeroes,
        guaranteed_arrears = zeroes,
        survival_advance = zeroes,
        survival_arrears = zeroes,
        death_SumInsured = zeroes,
        death_GrossPremium = zeroes,
        death_PremiumFree = zeroes,
        row.names = ages-age
      );

      # Premiums:
      premiums = pad0(rep(1, min(premiumPaymentPeriod, policyPeriod)), cflen);
      if (premiumPayments == "in advance") {
        cf$premiums_advance = premiums;
      } else {
        cf$premiums_arrears = premiums;
      }

      # Survival Benefits
      if (benefitPayments == "in advance") {
        cf$guaranteed_advance = pad0(basicCashFlows$guaranteed, cflen);
        cf$survival_advance = pad0(basicCashFlows$survival, cflen);
      } else {
        cf$guaranteed_arrears = pad0(basicCashFlows$guaranteed, cflen);
        cf$survival_arrears = pad0(basicCashFlows$survival, cflen);
      }

      # Death Benefits
      cf$death_SumInsured = pad0(basicCashFlows$death, cflen);
      cf$death_PremiumFree = cf$death_SumInsured;
      # premium refund
      if (self$premiumRefund != 0) {
        totalpremiumcf = cf$premiums_advance + pad0(c(0, cf$premiums_arrears), cflen);

        # death benefit for premium refund is the sum of all premiums so far:
        cf$death_GrossPremium = pad0(Reduce("+", totalpremiumcf[0:policyPeriod], accumulate=TRUE), cflen)
      }

      cf
    },

    getCashFlowsCosts = function(age, ..., policyPeriod=Inf, premiumPaymentPeriod = policyPeriod, maxAge = getOmega(self$mortalityTable)) {
      maxlen = min(maxAge - age, policyPeriod)+1;
      policyPeriod = min(maxAge - age, policyPeriod);
      premiumPeriod = min(policyPeriod, premiumPaymentPeriod);

      dm = dim(self$costs);
      dmnames = dimnames(self$costs);
      cf = array(0, dim=list(maxlen, dm[1], dm[2]), dimnames=list(0:(maxlen-1), dmnames[[1]], dmnames[[2]]));
      cf[1,,] = cf[1,,] + self$costs[,,"once"]
      for (i in 1:premiumPeriod) {
        cf[i,,] = cf[i,,] + self$costs[,,"PremiumPeriod"];
      }
      for (i in (premiumPeriod+1):policyPeriod) {
        cf[i,,] = cf[i,,] + self$costs[,,"PremiumFree"];
      }
      for (i in 1:policyPeriod) {
        cf[i,,] = cf[i,,] + self$costs[,,"PolicyPeriod"];
      }
      cf
    },

    presentValueCashFlows = function(cashflows, age, ..., benefitPaymentsPerYear = 1, maxAge = getOmega(self$mortalityTable)) {
      len = length(cashflows$premiums_advance);
      qq = self$getTransitionProbabilities (age, ...);
      q = pad0(qq$q, len);
      ages = pad0(qq$age, len);
      correctionPaymentsPerYear = correctionPaymentsPerYear(m = benefitPaymentsPerYear, i = self$i, order = self$benefitPaymentsPerYearOrder);

      pv = as.matrix(data.frame(
        age = ages,
        premiums = calculatePVSurvival (q, cashflows$premiums_advance, cashflows$premiums_arrears, v=self$v),
        guaranteed = calculatePVSurvival (q*0, cashflows$guaranteed_advance, cashflows$guaranteed_arrears, m=benefitPaymentsPerYear, mCorrection=correctionPaymentsPerYear, v=self$v),
        survival = calculatePVSurvival (q, cashflows$survival_advance, cashflows$survival_arrears, m=benefitPaymentsPerYear, mCorrection=correctionPaymentsPerYear, v=self$v),
        death_SumInsured = calculatePVDeath (q, cashflows$death_SumInsured, v=self$v),
        death_GrossPremium = calculatePVDeath (q, cashflows$death_GrossPremium, v=self$v),
        death_PremiumFree = calculatePVDeath (q, cashflows$death_PremiumFree, v=self$v),

        row.names = pad0(rownames(qq), len)
      ));
      pv
    },

    presentValueCashFlowsCosts = function(cashflows, age, ..., maxAge = getOmega(self$mortalityTable)) {
      len = dim(cashflows)[1];
      q = self$getTransitionProbabilities (age, ...);
      q = pad0(q$q, len);

      pvc = calculatePVCosts(q, cashflows, v=self$v);
      pvc
    },

    getPremiumCoefficients = function(type="gross", coeffBenefits, coeffCosts, ...,
                                      premiumSum = 0,
                                      premiums = list("gross"=0,"net"=0, "Zillmer"=0)) {
      securityLoading = self$surcharges$security;
      refundAddon = self$premiumRefundSpread;

      coefficients = list(
        "SumInsured" = list("benefits" = coeffBenefits*0, "costs" = coeffCosts*0),
        "Premium"    = list("benefits" = coeffBenefits*0, "costs" = coeffCosts*0)
      );

      coefficients[["Premium"]][["benefits"]][["premiums"]]            = 1;

      coefficients[["SumInsured"]][["benefits"]][["guaranteed"]]       = 1+securityLoading;
      coefficients[["SumInsured"]][["benefits"]][["survival"]]         = 1+securityLoading;
      coefficients[["SumInsured"]][["benefits"]][["death_SumInsured"]] = 1+securityLoading;
      # Premium refund is handled differently for gross and net premiums, because it is proportional to the gross premium
      if (type == "gross") {
        coefficients[["Premium"]][["benefits"]][["death_GrossPremium"]] = -(1+refundAddon) * (1+securityLoading);
      } else if (type=="net" || type=="Zillmer") {
        coefficients[["SumInsured"]][["benefits"]][["death_GrossPremium"]] = premiums$gross * (1+refundAddon) * (1+securityLoading);
      }


      # coefficients for the costs

      if (type=="gross") {
        coefficients[["SumInsured"]][["costs"]]["alpha", "SumInsured"] = 1;
        coefficients[["SumInsured"]][["costs"]]["beta",  "SumInsured"] = 1;
        coefficients[["SumInsured"]][["costs"]]["gamma", "SumInsured"] = 1;
        # TODO: How to handle beta costs proportional to Sum Insured
        coefficients[["Premium"]][["costs"]]["alpha", "SumPremiums"] = -premiumSum;
        coefficients[["Premium"]][["costs"]]["beta",  "SumPremiums"] = -premiumSum;
        coefficients[["Premium"]][["costs"]]["gamma", "SumPremiums"] = -premiumSum;

        coefficients[["Premium"]][["costs"]]["alpha", "GrossPremium"] = -1;
        coefficients[["Premium"]][["costs"]]["beta",  "GrossPremium"] = -1;
        coefficients[["Premium"]][["costs"]]["gamma", "GrossPremium"] = -1;

      } else if (type=="Zillmer") {
        coefficients[["SumInsured"]][["costs"]]["Zillmer","SumInsured"] = 1;
        coefficients[["SumInsured"]][["costs"]]["beta",   "SumInsured"] = 1;
        coefficients[["SumInsured"]][["costs"]]["gamma",  "SumInsured"] = 1;

        coefficients[["SumInsured"]][["costs"]]["Zillmer","SumPremiums"] = premiumSum * premiums$gross;
        coefficients[["SumInsured"]][["costs"]]["beta",   "SumPremiums"] = premiumSum * premiums$gross;
        coefficients[["SumInsured"]][["costs"]]["gamma",  "SumPremiums"] = premiumSum * premiums$gross;

        coefficients[["SumInsured"]][["costs"]]["Zillmer","GrossPremium"] = premiums$gross;
        coefficients[["SumInsured"]][["costs"]]["beta",   "GrossPremium"] = premiums$gross;
        coefficients[["SumInsured"]][["costs"]]["gamma",  "GrossPremium"] = premiums$gross;
      }

      coefficients
    }
    ,

    premiumCalculation = function(pvBenefits, pvCosts, costs=self$costs, premiumSum=0) {
      premiums = c("net" = 0, "gross"= 0, "Zillmer" = 0);

      coeff=self$getPremiumCoefficients("gross", pvBenefits["0",]*0, pvCosts["0",,]*0, premiums=premiums, premiumSum=premiumSum)
      enumerator  = sum(coeff[["SumInsured"]][["benefits"]] * pvBenefits["0",]) + sum(coeff[["SumInsured"]][["costs"]] * pvCosts["0",,]);
      denominator = sum(coeff[["Premium"   ]][["benefits"]] * pvBenefits["0",]) + sum(coeff[["Premium"   ]][["costs"]] * pvCosts["0",,]);
      premiums[["gross"]] = enumerator/denominator;

      coeff=self$getPremiumCoefficients("net", pvBenefits["0",]*0, pvCosts["0",,]*0, premiums=premiums, premiumSum=premiumSum)
      enumerator  = sum(coeff[["SumInsured"]][["benefits"]] * pvBenefits["0",]) + sum(coeff[["SumInsured"]][["costs"]] * pvCosts["0",,]);
      denominator = sum(coeff[["Premium"   ]][["benefits"]] * pvBenefits["0",]) + sum(coeff[["Premium"   ]][["costs"]] * pvCosts["0",,]);
      premiums[["net"]] = enumerator/denominator; premiums

      coeff=self$getPremiumCoefficients("Zillmer", pvBenefits["0",]*0, pvCosts["0",,]*0, premiums=premiums, premiumSum=premiumSum)
      enumerator  = sum(coeff[["SumInsured"]][["benefits"]] * pvBenefits["0",]) + sum(coeff[["SumInsured"]][["costs"]] * pvCosts["0",,]);
      denominator = sum(coeff[["Premium"   ]][["benefits"]] * pvBenefits["0",]) + sum(coeff[["Premium"   ]][["costs"]] * pvCosts["0",,]);
      premiums[["Zillmer"]] = enumerator/denominator;

      premiums
    },



    # Dummy to allow commas
    dummy = 0
  )
);

costs = initializeCosts();
# costs["alpha", "SumInsured",] = c(1,2,5);
# costs["beta", "SumPremiums",] = c(3,2,1);
costs["alpha", "SumPremiums", "once"] = 0.05;
costs["Zillmer", "SumPremiums", "once"] = 0.04;
costs["gamma", "SumInsured", "PremiumPeriod"] = 0.005;
costs["gamma", "SumInsured", "PremiumFree"] = 0.01;
costs["gamma_nopremiums", "SumInsured", "PolicyPeriod"] = 0.01;

costs

TestTarif = InsuranceTarif$new(name = "Testtarif", mortalityTable = AVOe2005R.male, type = "annuity", costs=costs)
q = TestTarif$getTransitionProbabilities(YOB = 1980, age = 30)
TestTarif = InsuranceTarif$new(name = "Testtarif", mortalityTable = AVOe2005R.male, type = "wholelife", costs=costs)
TestTarif$getBasicCashFlows(YOB = 1980, age = 30, policyPeriod = 5, deferral = 3, guaranteed=10)

# Gemischte Versicherung, i=1%, AVÖ2005R Unisex, YOB=1980, age=30, Laufzeit=10, Prämienzahlungsdauer=5,
TestTarif = InsuranceTarif$new(name = "Testtarif", mortalityTable = AVOe2005R.unisex, type = "endowment", costs=costs, i=0.01)
TestTarif$getBasicCashFlows(YOB = 1980, age = 30, premiumPaymentPeriod = 5, policyPeriod = 10, deferral = 0, guaranteed=0)

TestTarif$costs=costs;
TestTarif$premiumRefund = 0;

cf = TestTarif$getCashFlows(YOB = 1980, age = 30, premiumPaymentPeriod = 5, policyPeriod = 10, deferral = 0, guaranteed=0); cf
cfc = TestTarif$getCashFlowsCosts(YOB = 1980, age = 30, premiumPaymentPeriod = 5, policyPeriod = 10, deferral = 0, guaranteed=0); cfc

pv = TestTarif$presentValueCashFlows(cf, age = 30, YOB=1980); pv
pvc = TestTarif$presentValueCashFlowsCosts(cfc, age=30, YOB=1980); pvc

premiums=TestTarif$premiumCalculation(pv, pvc, premiumSum=15); as.array(premiums)*1000

c("net"=1, "gross"=23, "Zillmer"=44)
str(as.matrix( premiums))
as.matrix(premiums)*1000
scf
cfc
dim(cfc)[1]

str(cf$premiums_advance)
calculatePVSurvival(q$q, cf$premiums_advance, cf$premiums_arrears, v=1/1.01)
calculatePVSurvival(q$q, cf$survival_advance, cf$survival_arrears, v=1/1.01)

calculatePVDeath(q$q, cf$death_SumInsured, v=1/1.01)
calculatePVDeath(q$q, cf$death_GrossPremium, v=1/1.01)
calculatePVDeath(q$q, cf$death_PremiumFree, v=1/1.01)



InsuranceContract = R6Class(
  "InsuranceContract",
  public = list(
    tarif = NA,
    YOB = NA,
    age = NA,
    policyPeriod = Inf,
    premiumPaymentPeriod = 1,
    deferral = 0,
    guaranteed = 0,

    premiumPayments = PaymentTimeEnum("in advance"),
    benefitPayments = PaymentTimeEnum("in advance"),

    premiumPaymentsPerYear = 1,
    benefitPaymentsPerYear = 1, # Only for annuities!


    transitionProbabilities = NA,
    cashFlows = NA,
    #    cashFlowsState = list(c(0), c(0)),
    #    cashFlowsTransition = list(matrix(c(0,1,0,1), 2, 2, byrow=TRUE)),

    initialize = function(tarif, age, policyPeriod = inf, ..., deferral = 0, YOB = 1975) {
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
      self$cashFlowsState = self$tarif$getCashFlowsState(age = self$age, YOB = self$YOB, policyPeriod = self$policyPeriod, deferral = self$deferral, maxAge = self$age + length(self$transitionProbabilities));
      self$cashFlowsTransition = self$tarif$getCashFlowsState(age = self$age, YOB = self$YOB, policyPeriod = self$policyPeriod, deferral = self$deferral, maxAge = self$age + length(self$transitionProbabilities));

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
