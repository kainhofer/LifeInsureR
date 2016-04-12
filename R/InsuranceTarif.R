library(R6)
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
    premiumFrequencyOrder = 0,
    benefitFrequencyOrder = 0,
    widowFactor = 0,

    premiumRefund = 0,
    premiumRefundLoading = 0,  # Mindesttodesfallrisiko soll damit erreicht werden, z.B. 105% der einbezahlten Prämien

    advanceBonus = 0,
    sumRebate = 0,

    costs = list(),
    benefitFrequencyLoading = list("1" = 0.0, "2" = 0.0, "4" = 0.0, "12" = 0.0), # TODO: Properly implement this
    premiumFrequencyLoading = list("1" = 0.0, "2" = 0.0, "4" = 0.0, "12" = 0.0), # TODO: Implement this
    loadings = list(    # Loadings can also be function(sumInsured, premiums)    # TODO: Add other possible arguments
        "tax" = 0.04,                      # insurance tax, factor on each premium paid
        "unitcosts" = 0,                   # annual unit cost for each policy (Stückkosten), absolute value
        "security" = 0,                    # Additional security loading on all benefit payments, factor on all benefits
        "noMedicalExam" = 0,               # Loading when no medicial exam is done, % of SumInsured
        "sumRebate" = 0,                   # gross premium reduction for large premiums, % of SumInsured
        "premiumRebate" = 0,               # gross premium reduction for large premiums, % of gross premium # TODO
        "advanceProfitParticipation" = 0,  # Profit participation in advance, % of the premium
        "ongoingAlphaGrossPremium" = 0     # Acquisition cost that increase the gross premium
      ),


    initialize = function(name = NA, mortalityTable = NA, i = NA, type = "wholelife", ..., premiumFrequencyOrder = 0, benefitFrequencyOrder = 0, costs) {
      if (!missing(name))           self$name = name;
      if (!missing(mortalityTable)) self$mortalityTable = mortalityTable;
      if (!missing(i))              self$i = i;
      if (!missing(type))           self$tariffType = type;
      self$costs = if (!missing(costs)) costs else initializeCosts();
      if (!missing(benefitFrequencyOrder)) self$benefitFrequencyOrder = benefitFrequencyOrder;
      if (!missing(premiumFrequencyOrder)) self$premiumFrequencyOrder = premiumFrequencyOrder;

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
      if (premiumPeriod<policyPeriod) {
        for (i in (premiumPeriod+1):policyPeriod) {
          cf[i,,] = cf[i,,] + self$costs[,,"PremiumFree"];
        }
      }
      for (i in 1:policyPeriod) {
        cf[i,,] = cf[i,,] + self$costs[,,"PolicyPeriod"];
      }
      cf
    },

    presentValueCashFlows = function(cashflows, age, ..., premiumFrequency = 1, benefitFrequency = 1, maxAge = getOmega(self$mortalityTable)) {
      len = length(cashflows$premiums_advance);
      qq = self$getTransitionProbabilities (age, ...);
      q = pad0(qq$q, len);
      ages = pad0(qq$age, len);
      benefitFrequencyCorrection = correctionPaymentFrequency(m = benefitFrequency, i = self$i, order = self$benefitFrequencyOrder);
      premiumFrequencyCorrection = correctionPaymentFrequency(m = premiumFrequency, i = self$i, order = self$premiumFrequencyOrder);

      pv = as.matrix(data.frame( # TODO: Find a better way to combine the vectors into a matrix with given row/column names!
        age = ages,
        premiums = calculatePVSurvival (q, cashflows$premiums_advance, cashflows$premiums_arrears, m=premiumFrequency, mCorrection=premiumFrequencyCorrection, v=self$v),
        guaranteed = calculatePVSurvival (q*0, cashflows$guaranteed_advance, cashflows$guaranteed_arrears, m=benefitFrequency, mCorrection=benefitFrequencyCorrection, v=self$v),
        survival = calculatePVSurvival (q, cashflows$survival_advance, cashflows$survival_arrears, m=benefitFrequency, mCorrection=benefitFrequencyCorrection, v=self$v),
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
                                      premiums = c("unit.gross"=0)) {
      securityLoading = self$loadings$security;
      refundAddon = self$premiumRefundLoading;

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
        coefficients[["SumInsured"]][["benefits"]][["death_GrossPremium"]] = premiums[["unit.gross"]] * (1+refundAddon) * (1+securityLoading);
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

        coefficients[["SumInsured"]][["costs"]]["Zillmer","SumPremiums"] = premiumSum * premiums[["unit.gross"]];
        coefficients[["SumInsured"]][["costs"]]["beta",   "SumPremiums"] = premiumSum * premiums[["unit.gross"]];
        coefficients[["SumInsured"]][["costs"]]["gamma",  "SumPremiums"] = premiumSum * premiums[["unit.gross"]];

        coefficients[["SumInsured"]][["costs"]]["Zillmer","GrossPremium"] = premiums[["unit.gross"]];
        coefficients[["SumInsured"]][["costs"]]["beta",   "GrossPremium"] = premiums[["unit.gross"]];
        coefficients[["SumInsured"]][["costs"]]["gamma",  "GrossPremium"] = premiums[["unit.gross"]];
      }

      coefficients
    },

    premiumCalculation = function(pvBenefits, pvCosts, costs=self$costs, premiumSum=0, sumInsured=1, premiumFrequency = 1) {
      premiums = c("unit.net" = 0, "unit.gross"= 0, "unit.Zillmer" = 0, "net" = 0, "gross" = 0, "Zillmer" = 0, "written" = 0);

      # net, gross and Zillmer premiums are calculated from the present values using the coefficients on each present value as described in the formulas document
      coeff=self$getPremiumCoefficients("gross", pvBenefits["0",]*0, pvCosts["0",,]*0, premiums=premiums, premiumSum=premiumSum)
      enumerator  = sum(coeff[["SumInsured"]][["benefits"]] * pvBenefits["0",]) + sum(coeff[["SumInsured"]][["costs"]] * pvCosts["0",,]);
      denominator = sum(coeff[["Premium"   ]][["benefits"]] * pvBenefits["0",]) + sum(coeff[["Premium"   ]][["costs"]] * pvCosts["0",,]);
      ongoingAlphaGrossPremium = self$loadings$ongoingAlphaGrossPremium;
      premiums[["unit.gross"]] = enumerator/denominator * (1 + ongoingAlphaGrossPremium);
      premiums[["gross"]] = premiums[["unit.gross"]] * sumInsured;

      coeff=self$getPremiumCoefficients("net", pvBenefits["0",]*0, pvCosts["0",,]*0, premiums=premiums, premiumSum=premiumSum)
      enumerator  = sum(coeff[["SumInsured"]][["benefits"]] * pvBenefits["0",]) + sum(coeff[["SumInsured"]][["costs"]] * pvCosts["0",,]);
      denominator = sum(coeff[["Premium"   ]][["benefits"]] * pvBenefits["0",]) + sum(coeff[["Premium"   ]][["costs"]] * pvCosts["0",,]);
      premiums[["unit.net"]] = enumerator/denominator; premiums
      premiums[["net"]] = premiums[["unit.net"]] * sumInsured;

      coeff=self$getPremiumCoefficients("Zillmer", pvBenefits["0",]*0, pvCosts["0",,]*0, premiums=premiums, premiumSum=premiumSum)
      enumerator  = sum(coeff[["SumInsured"]][["benefits"]] * pvBenefits["0",]) + sum(coeff[["SumInsured"]][["costs"]] * pvCosts["0",,]);
      denominator = sum(coeff[["Premium"   ]][["benefits"]] * pvBenefits["0",]) + sum(coeff[["Premium"   ]][["costs"]] * pvCosts["0",,]);
      premiums[["unit.Zillmer"]] = enumerator/denominator;
      premiums[["Zillmer"]] = premiums[["unit.Zillmer"]] * sumInsured;

      # The written premium is the gross premium with additional loadings, rebates, unit costs and taxes
      tax           = valueOrFunction(self$loadings$tax,          sumInsured=sumInsured, premiums=premiums);
      unitCosts     = valueOrFunction(self$loadings$unitcosts,    sumInsured=sumInsured, premiums=premiums);
      noMedicalExam = valueOrFunction(self$loadings$noMedicalExam,sumInsured=sumInsured, premiums=premiums);
      sumRebate     = valueOrFunction(self$loadings$sumRebate,    sumInsured=sumInsured, premiums=premiums);
      premiumRebate = valueOrFunction(self$loadings$premiumRebate,sumInsured=sumInsured, premiums=premiums); # TODO: How shall this be included in the premium calculation?
      advanceProfitParticipation = valueOrFunction(self$loadings$advanceProfitParticipation,sumInsured=sumInsured, premiums=premiums);

      frequencyLoading = valueOrFunction(self$premiumFrequencyLoading, sumInsured=sumInsured, premiums=premiums);

      premiumBeforeTax = (premiums[["unit.gross"]] + noMedicalExam - sumRebate)*sumInsured * (1-advanceProfitParticipation) + unitCosts;
      premiumBeforeTax = premiumBeforeTax * (1+frequencyLoading[[toString(premiumFrequency)]]) / premiumFrequency;
      premiums[["written_beforetax"]] = premiumBeforeTax;
      premiums[["tax"]] = premiumBeforeTax * tax;
      premiums[["written"]] = premiumBeforeTax * (1 + tax);

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

premiums=TestTarif$premiumCalculation(pv, pvc, premiumSum=15); premiums



premiums*1000
as.array(premiums)*1000

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

    #### Contract settings
    sumInsured = 1,
    YOB = NA,
    age = NA,
    policyPeriod = Inf,
    premiumPeriod = 1,
    deferral = 0,
    guaranteed = 0,

    premiumPayments = PaymentTimeEnum("in advance"),
    benefitPayments = PaymentTimeEnum("in advance"),

    premiumFrequency = 1,
    benefitFrequency = 1, # Only for annuities!

    #### Caching values for this contract, initialized/calculated when the object is created
    transitionProbabilities = NA,

    cashFlowsBasic = NA,
    cashFlows = NA,
    cashFlowsCosts = NA,
    premiumSum = 0,

    presentValues = NA,
    presentValuesCosts = NA,

    premiums = NA,


    #### The code:

    initialize = function(tarif, age, policyPeriod,
                          premiumPeriod = policyPeriod, sumInsured = 1,
                          ...,
                          premiumPayments = "in advance", benefitPayments = "in advance",
                          premiumFrequency = 1, benefitFrequency = 1,
                          deferral = 0, YOB = 1975) {
      self$tarif = tarif;
      self$age = age;
      self$policyPeriod = policyPeriod;
      self$premiumPeriod = premiumPeriod;
      self$sumInsured = sumInsured;
      if (!missing(deferral))     self$deferral = deferral;
      if (!missing(YOB))          self$YOB = YOB;
      if (!missing(premiumPayments)) self$premiumPayments = premiumPayments;
      if (!missing(benefitPayments)) self$benefitPayments = benefitPayments;
      if (!missing(premiumFrequency)) self$premiumFrequency = premiumFrequency;
      if (!missing(benefitFrequency)) self$benefitFrequency = benefitFrequency;

      self$determineTransitionProbabilities();
      self$determineCashFlows();
      self$calculatePresentValues();
      self$calculatePremiums();
    },

    determineTransitionProbabilities = function() {
      self$transitionProbabilities = self$tarif$getTransitionProbabilities(YOB = self$YOB, age = self$age);
      self$transitionProbabilities
    },

    determineCashFlows = function() {
      self$cashFlowsBasic = self$tarif$getBasicCashFlows(YOB = self$YOB, age = self$age, policyPeriod = self$policyPeriod, premiumPeriod = self$premiumPeriod);
      self$cashFlows = self$tarif$getCashFlows(age = self$age, premiumPayments = self$premiumPayments, benefitPayments = self$benefitPayments, policyPeriod = self$policyPeriod, premiumPaymentPeriod = self$premiumPeriod, basicCashFlows = self$cashFlowsBasic);
      self$premiumSum = sum(self$cashFlows$premiums_advance + self$cashFlows$premiums_arrears);
      self$cashFlowsCosts = self$tarif$getCashFlowsCosts(YOB = self$YOB, age = self$age, premiumPaymentPeriod = self$premiumPeriod, policyPeriod = self$policyPeriod);
      list("benefits"= self$cashFlows, "costs"=self$cashFlowCosts, "premiumSum" = self$premiumSum)
    },

    calculatePresentValues = function() {
      self$presentValues = self$tarif$presentValueCashFlows(self$cashFlows, age = self$age, YOB = self$YOB, premiumFrequency = self$premiumFrequency, benefitFrequency = self$benefitFrequency);
      self$presentValuesCosts = self$tarif$presentValueCashFlowsCosts(self$cashFlowsCosts, age = self$age, YOB = self$YOB);
      list("benefits" = self$presentValues, "costs" = self$presentValuesCosts)
    },

    calculatePremiums = function() {
      self$premiums = self$tarif$premiumCalculation(self$presentValues, self$presentValuesCosts, premiumSum = self$premiumSum, sumInsured = self$sumInsured);
      self$premiums
    },

    dummy=NA
  )
);




