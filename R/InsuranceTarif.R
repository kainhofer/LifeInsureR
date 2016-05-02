library(R6)
# library(lifecontingencies)
library(objectProperties)

TariffTypeEnum = setSingleEnum("TariffType", levels = c("annuity", "wholelife", "endowment", "pureendowment", "terme-fix", "dread-disease"))
PaymentTimeEnum = setSingleEnum("PaymentTime", levels = c("in advance", "in arrears"))
#PaymentCountEnum = setSingleEnum(PaymentCount, levels = c(1,2,3))


# Initialize a cost matrix with dimensions: [CostType, Basis, Period], with:
#     CostType: alpha, Zillmer, beta, gamma, gamma_nopremiums
#     Basis:    SumInsured, SumPremiums, GrossPremium
#     Period:   once, PremiumPeriod, PremiumFree, PolicyPeriod
# TODO: gamma an Erlebensleistungen?
initializeCosts = function() {
  dimnm=list(
    c("alpha", "Zillmer", "beta", "gamma", "gamma_nopremiums"),
    c("SumInsured", "SumPremiums", "GrossPremium"),
    c("once", "PremiumPeriod", "PremiumFree", "PolicyPeriod")
  );
  array(0,
        dim=sapply(dimnm, length),
        dimnames=dimnm
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
    mortalityTable = NULL,
    invalidityTable = NULL,
    i = 0, # guaranteed interest rate
    v = 1, # discount factor
    tariffType = TariffTypeEnum("wholelife"), # possible values: annuity, wholelife, endowment, pureendowment, terme-fix
    premiumFrequencyOrder = 0,
    benefitFrequencyOrder = 0,
    widowFactor = 0,

    defaultPremiumPeriod = NULL,
    premiumRefund = 0,
    premiumRefundLoading = 0,  # Mindesttodesfallrisiko soll damit erreicht werden, z.B. 105% der einbezahlten Prämien
    surrenderValueCalculation = NULL, # By default, not surrender penalties

    costs = list(),
    benefitFrequencyLoading = list("1" = 0.0, "2" = 0.0, "4" = 0.0, "12" = 0.0), # TODO: Properly implement this
    premiumFrequencyLoading = list("1" = 0.0, "2" = 0.0, "4" = 0.0, "12" = 0.0), # TODO: Implement this
    loadings = list(    # Loadings can also be function(sumInsured, premiums)    # TODO: Add other possible arguments
        "ongoingAlphaGrossPremium" = 0,    # Acquisition cost that increase the gross premium
        "tax" = 0.04,                      # insurance tax, factor on each premium paid
        "unitcosts" = 0,                   # annual unit cost for each policy (Stückkosten), absolute value
        "security" = 0,                    # Additional security loading on all benefit payments, factor on all benefits
        "noMedicalExam" = 0,               # Loading when no medicial exam is done, % of SumInsured
        "noMedicalExamRelative" = 0,       # Loading when no medicial exam is done, % of gross premium
        "sumRebate" = 0,                   # gross premium reduction for large premiums, % of SumInsured
        "premiumRebate" = 0,               # gross premium reduction for large premiums, % of gross premium # TODO
        "advanceProfitParticipation" = 0,                # Vorweggewinnbeteiligung (%-Satz der Bruttoprämie)
        "advanceProfitParticipationInclUnitCost" = 0,    # Vorweggewinnbeteiligung (%-Satz der Prämie mit Zu-/Abschlägen, insbesondere nach Stückkosten)
        "partnerRebate" = 0                # Partnerrabatt auf Prämie mit Zu-/Abschlägen, wenn mehr als 1 Vertrag gleichzeitig abgeschlossen wird, additiv mit advanceBonusInclUnitCost and premiumRebate
      ),

    features = list(  #Special cases for the calculations
        "betaGammaInZillmer" = FALSE,      # Whether beta and gamma-costs should be included in the Zillmer premium calculation
        "alphaRefundLinear" = TRUE         # Whether the refund of alpha-costs on surrender is linear in t or follows the NPV of an annuity
      ),


    initialize = function(name = NULL, mortalityTable = NULL, i = NULL, type = "wholelife", ..., loadings=list(), invalidityTable=NULL, features = list(), premiumPeriod = NULL, premiumFrequencyOrder = 0, benefitFrequencyOrder = 0, costs, surrenderValueCalculation) {
      if (!missing(name))           self$name = name;
      if (!missing(mortalityTable)) self$mortalityTable = mortalityTable;
      if (!missing(i))              self$i = i;
      if (!missing(type))           self$tariffType = type;
      self$costs = if (!missing(costs)) costs else initializeCosts();
      if (!missing(benefitFrequencyOrder)) self$benefitFrequencyOrder = benefitFrequencyOrder;
      if (!missing(premiumFrequencyOrder)) self$premiumFrequencyOrder = premiumFrequencyOrder;
      # Set default premiumPeriod, e.g. single premium, to be used when the contract has no explicit premium period
      if (!missing(premiumPeriod))  self$defaultPremiumPeriod = premiumPeriod;
      if (!missing(features))       self$features = c(features, self$features);
      if (!missing(surrenderValueCalculation)) self$surrenderValueCalculation = surrenderValueCalculation;
      if (!missing(invalidityTable)) self$invalidityTable = invalidityTable;
      if (!missing(loadings))       self$loadings = self$getLoadings(loadings=loadings);

      self$v = 1/(1+self$i);

      cat(paste0("Initializing Insurance Tarif ", self$name, "...\n"));
    },

    # Merge a possibly passed loadings override with this tariff's default loadings:
    getLoadings = function(loadings=list(), ...) {
      c(loadings, self$loadings)
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
      if (!is.null(self$invalidityTable)) {
        i = deathProbabilities(self$invalidityTable, YOB=YOB);
        if (age > 0) {
          i    = i[-age:-1];
        }
      } else {
        i = rep(0, length(q));
      }
      i = pad0(i, length(q));
      df = data.frame(age=ages, q=q, i=i, p=1-q-i, row.names = ages-age)
      df
    },
    getBasicCashFlows = function(age, ..., guaranteed = 0, policyPeriod = inf, deferral = 0, maxAge = getOmega(self$mortalityTable)) {
      maxlen = min(maxAge - age, policyPeriod);
      cf = data.frame(
        guaranteed = rep(0, maxlen+1),
        survival = rep(0, maxlen+1),
        death = rep(0, maxlen+1),
        disease = rep(0, maxlen+1)
      );
      if (self$tariffType == "annuity") {
        # guaranteed payments exist only with annuities (first n years of the payment)
        cf$guaranteed = c(rep(0, deferral), rep(1, guaranteed), rep(0, max(0, maxlen+1 - deferral - guaranteed)))
        cf$survival = c(rep(0, deferral + guaranteed), rep(1, max(0, maxlen - deferral - guaranteed)), 0)
      } else if (self$tariffType == "terme-fix") {
        cf$guaranteed = c(rep(0, policyPeriod), 1);
      } else if (self$tariffType == "dread-disease") {
        cf$disease = c(rep(0, deferral), rep(1, maxlen - deferral), 0);
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

    getCashFlows = function(age, ..., premiumPayments = "in advance", benefitPayments = "in advance", guaranteed = 0, policyPeriod=Inf, premiumPeriod = policyPeriod, deferral=0, maxAge = getOmega(self$mortalityTable), cashFlowsBasic = NULL, premiumWaiver = FALSE) {
      if (missing(cashFlowsBasic)) {
        cashFlowsBasic = self$getBasicCashFlows(age = age, ..., guaranteed = guaranteed,
              policyPeriod = policyPeriod, deferral = deferral, maxAge = maxAge);
      }
      cflen = length(cashFlowsBasic$survival);
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
        disease_SumInsured = zeroes,
        death_GrossPremium = zeroes,
        death_Refund_past = zeroes,
        death_PremiumFree = zeroes,
        row.names = ages-age
      );

      # Premiums:
      if (!premiumWaiver) {
        premiums = pad0(rep(1, min(premiumPeriod, policyPeriod)), cflen);
        if (premiumPayments == "in advance") {
          cf$premiums_advance = premiums;
        } else {
          cf$premiums_arrears = premiums;
        }
      }

      # Survival Benefits
      if (benefitPayments == "in advance") {
        cf$guaranteed_advance = pad0(cashFlowsBasic$guaranteed, cflen);
        cf$survival_advance = pad0(cashFlowsBasic$survival, cflen);
      } else {
        cf$guaranteed_arrears = pad0(cashFlowsBasic$guaranteed, cflen);
        cf$survival_arrears = pad0(cashFlowsBasic$survival, cflen);
      }

      # Death Benefits
      cf$death_SumInsured = pad0(cashFlowsBasic$death, cflen);
      cf$disease_SumInsured = pad0(cashFlowsBasic$disease, cflen);
      cf$death_PremiumFree = cf$death_SumInsured;
      # premium refund
      if (self$premiumRefund != 0) {
        totalpremiumcf = cf$premiums_advance + pad0(c(0, cf$premiums_arrears), cflen);

        # death benefit for premium refund is the sum of all premiums so far:
        cf$death_GrossPremium = pad0(Reduce("+", totalpremiumcf[0:policyPeriod], accumulate=TRUE), cflen)
        cf$death_Refund_past = cf$death_GrossPremium
        cf$death_Refund_past[(cf$death_GrossPremium >0)] = 1;
      }

      cf
    },

    getCashFlowsCosts = function(age, ..., policyPeriod=Inf, premiumPeriod = policyPeriod, premiumWaiver = FALSE, maxAge = getOmega(self$mortalityTable)) {
      maxlen = min(maxAge - age, policyPeriod)+1;
      policyPeriod = min(maxAge - age, policyPeriod);
      premiumPeriod = min(policyPeriod, premiumPeriod);

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

      # After premiums are waived, use the gamma_nopremiums instead of gamma:
      if (premiumWaiver) {
        cf[,"gamma",] = cf[,"gamma_nopremiums",];
      }
      cf
    },

    presentValueCashFlows = function(cashFlows, age, ..., premiumFrequency = 1, benefitFrequency = 1, maxAge = getOmega(self$mortalityTable)) {
      len = length(cashFlows$premiums_advance);
      qq = self$getTransitionProbabilities (age, ...);
      qx = pad0(qq$q, len);
      ix = pad0(qq$i, len);
      px = pad0(qq$p, len);
      benefitFrequencyCorrection = correctionPaymentFrequency(m = benefitFrequency, i = self$i, order = self$benefitFrequencyOrder);
      premiumFrequencyCorrection = correctionPaymentFrequency(m = premiumFrequency, i = self$i, order = self$premiumFrequencyOrder);

      pvRefund = calculatePVDeath (px, qx, cashFlows$death_GrossPremium, v=self$v);
      pvRefundPast = calculatePVDeath (px, qx, cashFlows$death_Refund_past, v=self$v) * (cashFlows[,"death_GrossPremium"]-cashFlows[,"premiums_advance"]);

      pv = cbind(
        premiums = calculatePVSurvival (px, qx, cashFlows$premiums_advance, cashFlows$premiums_arrears, m=premiumFrequency, mCorrection=premiumFrequencyCorrection, v=self$v),
        guaranteed = calculatePVGuaranteed (cashFlows$guaranteed_advance, cashFlows$guaranteed_arrears, m=benefitFrequency, mCorrection=benefitFrequencyCorrection, v=self$v),
        survival = calculatePVSurvival (px, qx, cashFlows$survival_advance, cashFlows$survival_arrears, m=benefitFrequency, mCorrection=benefitFrequencyCorrection, v=self$v),
        death_SumInsured = calculatePVDeath (px, qx, cashFlows$death_SumInsured, v=self$v),
        disease_SumInsured = calculatePVDisease(px, qx, ix, cashFlows$disease_SumInsured, v=self$v),
        death_GrossPremium = pvRefund,
        death_Refund_past = pvRefundPast,
        death_Refund_future = pvRefund - pvRefundPast,
        death_PremiumFree = calculatePVDeath (px, qx, cashFlows$death_PremiumFree, v=self$v)
      );

      rownames(pv) <- pad0(rownames(qq), len);
      pv
    },

    presentValueCashFlowsCosts = function(cashFlowsCosts, age, ..., maxAge = getOmega(self$mortalityTable)) {
      len = dim(cashFlowsCosts)[1];
      q = self$getTransitionProbabilities (age, ...);
      qx = pad0(q$q, len);
      px = pad0(q$p, len);

      pvc = calculatePVCosts(px, qx, cashFlowsCosts, v=self$v);
      pvc
    },

    # Cost values (CF, present values, etc.) are an Tx5x3 matrix => convert to Tx15 matrix (alpha | Zillmer | beta | gamma)
    costValuesAsMatrix = function (costValues) {
      dm = dim(costValues);
      nm = dimnames(costValues);
      colnames=t(outer(nm[[2]], nm[[3]], paste, sep="."));

      res = aperm(costValues, c(1,3,2));
      dim(res) = c(dm[[1]], dm[[2]]*dm[[3]]);
      dimnames(res) = list(nm[[1]], colnames)
      res
    },

    getAbsCashFlows = function(cashFlows, cashFlowsCosts, premiums, sumInsured=1, premiumSum=0, ...) {
      refundAddon = self$premiumRefundLoading;

      # TODO: Set up a nice list with coefficients for each type of cashflow, rather than multiplying each item manually (this also mitigates the risk of forgetting a dimension, because then the dimensions would not match, while here it's easy to overlook a multiplication)
      # Multiply each CF column by the corresponding basis
      cashFlows[,c("premiums_advance", "premiums_arrears")] = cashFlows[,c("premiums_advance", "premiums_arrears")] * premiums[["gross"]];
      cashFlows[,c("guaranteed_advance", "guaranteed_arrears", "survival_advance", "survival_arrears", "death_SumInsured", "death_PremiumFree", "disease_SumInsured")] =
        cashFlows[,c("guaranteed_advance", "guaranteed_arrears", "survival_advance", "survival_arrears", "death_SumInsured", "death_PremiumFree", "disease_SumInsured")] * sumInsured;
      cashFlows[,c("death_GrossPremium", "death_Refund_past")] = cashFlows[,c("death_GrossPremium","death_Refund_past")] * premiums[["gross"]] * (1+refundAddon);

      # Sum all death-related payments to "death"  and remove the death_GrossPremium column
      cashFlows[,"death_SumInsured"] = cashFlows[,"death_SumInsured"] + cashFlows[,"death_GrossPremium"]
      colnames(cashFlows)[colnames(cashFlows)=="death_SumInsured"] = "death";
      # cashFlows[,"death_GrossPremium"] = NULL;

      cashFlowsCosts = cashFlowsCosts[,,"SumInsured"] * sumInsured +
        cashFlowsCosts[,,"SumPremiums"] * premiumSum * premiums[["gross"]] +
        cashFlowsCosts[,,"GrossPremium"] * premiums[["gross"]];

      cbind(cashFlows, cashFlowsCosts)
    },

    getAbsPresentValues = function(presentValues, premiums, sumInsured=1, premiumSum=0, ...) {
      refundAddon = self$premiumRefundLoading;
      pv = presentValues;

      #pv[,"age"] = pv[,"premiums"];
      #colnames(pv)[colnames(pv)=="age"] = "premiums.unit";

      # Multiply each CF column by the corresponding basis
      pv[,"premiums"] = pv[,"premiums"] * premiums[["gross"]];
      pv[,c("guaranteed", "survival", "death_SumInsured", "disease_SumInsured", "death_PremiumFree")] =
        pv[,c("guaranteed", "survival", "death_SumInsured", "disease_SumInsured", "death_PremiumFree")] * sumInsured;
      pv[,c("death_GrossPremium", "death_Refund_past", "death_Refund_future")] = pv[,c("death_GrossPremium", "death_Refund_past", "death_Refund_future")] * premiums[["gross"]] * (1+refundAddon);
      pv[,c("benefits", "benefitsAndRefund", "alpha", "Zillmer", "beta", "gamma", "gamma_nopremiums")] =
        pv[,c("benefits", "benefitsAndRefund", "alpha", "Zillmer", "beta", "gamma", "gamma_nopremiums")] * sumInsured;

      # Sum all death-related payments to "death"  and remove the death_SumInsured column
      pv[,"death_SumInsured"] = pv[,"death_SumInsured"] + pv[,"death_GrossPremium"]
      colnames(pv)[colnames(pv)=="death_SumInsured"] = "death";

      cbind("premiums.unit"=presentValues[,"premiums"], pv)
    },


    presentValueBenefits = function(presentValues, presentValuesCosts, premiums, sumInsured=1, premiumSum=0, ...) {
      refundAddon = self$premiumRefundLoading;
      # TODO: Here we don't use the securityLoading parameter => Shall it be used or are these values to be understood without additional security loading?
      benefits    = presentValues[,"survival"] + presentValues[,"death_SumInsured"] + presentValues[,"disease_SumInsured"];
      allBenefits = presentValues[,"survival"] + presentValues[,"death_SumInsured"] + presentValues[,"disease_SumInsured"] + presentValues[,"death_GrossPremium"] * premiums[["unit.gross"]] * (1+refundAddon);

      benefitsCosts = presentValuesCosts[,,"SumInsured"] +
        presentValuesCosts[,,"SumPremiums"] * premiumSum * premiums[["unit.gross"]] +
        presentValuesCosts[,,"GrossPremium"] * premiums[["unit.gross"]];

      cbind(
        benefits=benefits,
        benefitsAndRefund=allBenefits,
        benefitsCosts)
    },

    getPremiumCoefficients = function(type="gross", coeffBenefits, coeffCosts, ...,
                                      premiumSum = 0,premiums = c("unit.gross"=0), loadings=list()) {
      # Merge a possibly passed loadings override with the defaults of this class:
      loadings = self$getLoadings(loadings=loadings);
      securityLoading = loadings$security;
      refundAddon = self$premiumRefundLoading;

      coeff = list(
        "SumInsured" = list("benefits" = coeffBenefits*0, "costs" = coeffCosts*0),
        "Premium"    = list("benefits" = coeffBenefits*0, "costs" = coeffCosts*0)
      );

      coeff[["Premium"]][["benefits"]][["premiums"]]            = 1;

      coeff[["SumInsured"]][["benefits"]][["guaranteed"]]       = 1+securityLoading;
      coeff[["SumInsured"]][["benefits"]][["survival"]]         = 1+securityLoading;
      coeff[["SumInsured"]][["benefits"]][["death_SumInsured"]] = 1+securityLoading;
      coeff[["SumInsured"]][["benefits"]][["disease_SumInsured"]] = 1+securityLoading;

      # Premium refund is handled differently for gross and net premiums, because it is proportional to the gross premium
      if (type == "gross") {
        coeff[["Premium"]][["benefits"]][["death_GrossPremium"]] = -(1+refundAddon) * (1+securityLoading);
      } else if (type=="net" || type=="Zillmer") {
        coeff[["SumInsured"]][["benefits"]][["death_GrossPremium"]] = premiums[["unit.gross"]] * (1+refundAddon) * (1+securityLoading);
      }


      # coefficients for the costs

      if (type=="gross") {
        coeff[["SumInsured"]][["costs"]]["alpha", "SumInsured"] = 1;
        coeff[["SumInsured"]][["costs"]]["beta",  "SumInsured"] = 1;
        coeff[["SumInsured"]][["costs"]]["gamma", "SumInsured"] = 1;
        # TODO: How to handle beta costs proportional to Sum Insured
        coeff[["Premium"]][["costs"]]["alpha", "SumPremiums"] = -premiumSum;
        coeff[["Premium"]][["costs"]]["beta",  "SumPremiums"] = -premiumSum;
        coeff[["Premium"]][["costs"]]["gamma", "SumPremiums"] = -premiumSum;

        coeff[["Premium"]][["costs"]]["alpha", "GrossPremium"] = -1;
        coeff[["Premium"]][["costs"]]["beta",  "GrossPremium"] = -1;
        coeff[["Premium"]][["costs"]]["gamma", "GrossPremium"] = -1;

      } else if (type=="Zillmer") {
        coeff[["SumInsured"]][["costs"]]["Zillmer","SumInsured"] = 1;
        coeff[["SumInsured"]][["costs"]]["Zillmer","SumPremiums"] = premiumSum * premiums[["unit.gross"]];
        coeff[["SumInsured"]][["costs"]]["Zillmer","GrossPremium"] = premiums[["unit.gross"]];
        if (self$features$betaGammaInZillmer) {
          coeff[["SumInsured"]][["costs"]]["beta",  "SumInsured"] = 1;
          coeff[["SumInsured"]][["costs"]]["gamma", "SumInsured"] = 1;
          coeff[["SumInsured"]][["costs"]]["beta",  "SumPremiums"] = premiumSum * premiums[["unit.gross"]];
          coeff[["SumInsured"]][["costs"]]["gamma", "SumPremiums"] = premiumSum * premiums[["unit.gross"]];
          coeff[["SumInsured"]][["costs"]]["beta",  "GrossPremium"] = premiums[["unit.gross"]];
          coeff[["SumInsured"]][["costs"]]["gamma", "GrossPremium"] = premiums[["unit.gross"]];
        }
      }

      coeff
    },

    premiumCalculation = function(presentValues, presentValuesCosts, costs=self$costs, premiumSum=0, sumInsured=1, premiumFrequency = 1, loadings=list(), ...) {
      # Merge a possibly passed loadings override with the defaults of this class:
      loadings = self$getLoadings(loadings=loadings);
      premiums = c("unit.net" = 0, "unit.Zillmer" = 0, "unit.gross"= 0, "net" = 0, "Zillmer" = 0, "gross" = 0, "written" = 0);
      coefficients = list("gross"=c(), "Zillmer"=c(), "net"=c());

      # net, gross and Zillmer premiums are calculated from the present values using the coefficients on each present value as described in the formulas document
      coeff=self$getPremiumCoefficients("gross", presentValues["0",]*0, presentValuesCosts["0",,]*0, premiums=premiums, premiumSum=premiumSum, loadings=loadings)
      enumerator  = sum(coeff[["SumInsured"]][["benefits"]] * presentValues["0",]) + sum(coeff[["SumInsured"]][["costs"]] * presentValuesCosts["0",,]);
      denominator = sum(coeff[["Premium"   ]][["benefits"]] * presentValues["0",]) + sum(coeff[["Premium"   ]][["costs"]] * presentValuesCosts["0",,]);
      ongoingAlphaGrossPremium = loadings$ongoingAlphaGrossPremium;
      premiums[["unit.gross"]] = enumerator/denominator * (1 + ongoingAlphaGrossPremium);
      premiums[["gross"]] = premiums[["unit.gross"]] * sumInsured;
      coefficients[["gross"]] = coeff;

      coeff=self$getPremiumCoefficients("net", presentValues["0",]*0, presentValuesCosts["0",,]*0, premiums=premiums, premiumSum=premiumSum)
      enumerator  = sum(coeff[["SumInsured"]][["benefits"]] * presentValues["0",]) + sum(coeff[["SumInsured"]][["costs"]] * presentValuesCosts["0",,]);
      denominator = sum(coeff[["Premium"   ]][["benefits"]] * presentValues["0",]) + sum(coeff[["Premium"   ]][["costs"]] * presentValuesCosts["0",,]);
      premiums[["unit.net"]] = enumerator/denominator; premiums
      premiums[["net"]] = premiums[["unit.net"]] * sumInsured;
      coefficients[["net"]] = coeff;

      coeff=self$getPremiumCoefficients("Zillmer", presentValues["0",]*0, presentValuesCosts["0",,]*0, premiums=premiums, premiumSum=premiumSum)
      enumerator  = sum(coeff[["SumInsured"]][["benefits"]] * presentValues["0",]) + sum(coeff[["SumInsured"]][["costs"]] * presentValuesCosts["0",,]);
      denominator = sum(coeff[["Premium"   ]][["benefits"]] * presentValues["0",]) + sum(coeff[["Premium"   ]][["costs"]] * presentValuesCosts["0",,]);
      premiums[["unit.Zillmer"]] = enumerator/denominator;
      premiums[["Zillmer"]] = premiums[["unit.Zillmer"]] * sumInsured;
      coefficients[["Zillmer"]] = coeff;


      # The written premium is the gross premium with additional loadings, rebates, unit costs and taxes
      tax           = valueOrFunction(loadings$tax,          sumInsured=sumInsured, premiums=premiums);
      unitCosts     = valueOrFunction(loadings$unitcosts,    sumInsured=sumInsured, premiums=premiums);
      noMedicalExam = valueOrFunction(loadings$noMedicalExam,sumInsured=sumInsured, premiums=premiums);
      noMedicalExam.relative = valueOrFunction(loadings$noMedicalExamRelative,sumInsured=sumInsured, premiums=premiums);
      sumRebate     = valueOrFunction(loadings$sumRebate,    sumInsured=sumInsured, premiums=premiums);
      premiumRebate = valueOrFunction(loadings$premiumRebate,sumInsured=sumInsured, premiums=premiums);
      advanceProfitParticipation = valueOrFunction(loadings$advanceProfitParticipation,sumInsured=sumInsured, premiums=premiums);
      advanceProfitParticipationUnitCosts = valueOrFunction(loadings$advanceProfitParticipationInclUnitCost, sumInsured=sumInsured, premiums=premiums);
      partnerRebate = valueOrFunction(loadings$partnerRebate,sumInsured=sumInsured, premiums=premiums);

      frequencyLoading = valueOrFunction(self$premiumFrequencyLoading, sumInsured=sumInsured, premiums=premiums);

      premiumBeforeTax = (premiums[["unit.gross"]]*(1+noMedicalExam.relative) + noMedicalExam - sumRebate)*sumInsured * (1-advanceProfitParticipation) + unitCosts;
      premiumBeforeTax = premiumBeforeTax * (1-premiumRebate-advanceProfitParticipationUnitCosts-partnerRebate);
      premiumBeforeTax = premiumBeforeTax * (1+frequencyLoading[[toString(premiumFrequency)]]) / premiumFrequency;
      premiums[["written_beforetax"]] = premiumBeforeTax;
      premiums[["tax"]] = premiumBeforeTax * tax;
      premiums[["written"]] = premiumBeforeTax * (1 + tax);

      list("premiums"=premiums, "coefficients"=coefficients)
    },

    reserveCalculation = function (premiums, absPresentValues, absCashFlows, sumInsured=1, premiumSum=0, policyPeriod = 1, age = 0, ..., reserves = c(), loadings=list(), surrenderPenalty = TRUE) {
      # Merge a possibly passed loadings override with the defaults of this class:
      loadings = self$getLoadings(loadings=loadings);
      # Net, Zillmer and Gross reserves
      resNet = absPresentValues[,"benefitsAndRefund"] * (1+loadings$security) - premiums[["net"]] * absPresentValues[,"premiums.unit"];
      BWZcorr = absPresentValues["0", "Zillmer"] / absPresentValues["0", "premiums"] * absPresentValues[,"premiums"];
      resZ = resNet - BWZcorr;

      resAdeq = absPresentValues[,"benefitsAndRefund"] * (1+loadings$security) +
        absPresentValues[,"alpha"] + absPresentValues[,"beta"] + absPresentValues["gamma"] -
        premiums[["gross"]] * absPresentValues[,"premiums.unit"];

      #premiums[["Zillmer"]] * absPresentValues[,"premiums"];
      resGamma = absPresentValues[,"gamma"] - absPresentValues["0", "gamma"] / absPresentValues["0", "premiums"] * absPresentValues[,"premiums"]


      resConversion = (resZ + resGamma) * (1-loadings$advanceProfitParticipation);

      # Alpha refund: Distribute alpha-costs to 5 year (or if shorter, the policy period):
      r = min(policyPeriod, 5);
      ZillmerSoFar = Reduce("+", absCashFlows$Zillmer, accumulate = TRUE);
      ZillmerTotal = sum(absCashFlows$Zillmer);
      len = length(ZillmerSoFar);
      if (self$features$alphaRefundLinear) {
        ZillmerVerteilungCoeff = pad0((0:r)/r, len, 1);
      } else {
        q = self$getTransitionProbabilities (age, ...);
        # vector of all ä_{x+t, r-t}
        pvAlphaTmp = calculatePVSurvival(q = pad0(q$q, len), advance = pad0(rep(1,r), len), v = self$v);
        ZillmerVerteilungCoeff = (1-pvAlphaTmp/pvAlphaTmp[[1]]);
      }
      alphaRefund = ZillmerSoFar - ZillmerVerteilungCoeff * ZillmerTotal;

      # Reduction Reserve: Reserve used for contract modifications:
      resReduction = pmax(0, resZ+resGamma+alphaRefund) # V_{x,n}^{Rkf}

      # Collect all reserved to one large matrix
      res = cbind("net"=resNet, "Zillmer"=resZ, "adequate"= resAdeq, "gamma"=resGamma,
                  "contractual"=resZ+resGamma, "conversion"=resConversion, "alphaRefund"=alphaRefund, "reduction"=resReduction
                  #, "Reserve.premiumfree"=res.premiumfree, "Reserve.gamma.premiumfree"=res.gamma.premiumfree);
      );
      rownames(res) <- rownames(absPresentValues);

      # The surrender value functions can have arbitrary form, so we store a function
      # here in the tarif and call that, passing the reduction reserve as
      # starting point, but also all reserves, cash flows, premiums and present values
      if (!surrenderPenalty) {
        # No surrender penalty any more (has already been applied to the first contract change!)
        surrenderValue = resReduction;
      } else if (!is.null(self$surrenderValueCalculation)) {
        surrenderValue = self$surrenderValueCalculation(
          resReduction, reserves=res, premiums=premiums, absPresentValues=absPresentValues,
          absCashFlows=absCashFlows, sumInsured=sumInsured, premiumSum=premiumSum,
          policyPeriod = policyPeriod, age = age, loadings=loadings, ...);
      } else {
        # by default, refund the full reduction reserve, except the advance profit participation, which is also included in the reserve, but not charged on the premium!
        surrenderValue = resReduction * (1-loadings$advanceProfitParticipationInclUnitCost);
      }


      # Calculate new sum insured after premium waiver
      Storno = 0; # TODO: Implement storno costs
      newSI = (surrenderValue - absPresentValues[,"death_Refund_past"] * (1+loadings$security) - c(Storno)) /
        (absPresentValues[, "benefits"] * (1+loadings$security) + absPresentValues[, "gamma_nopremiums"]) * sumInsured;

      cbind(res,
            "PremiumsPaid"=Reduce("+", absCashFlows$premiums_advance, accumulate = TRUE),
            "Surrender"=surrenderValue,
            "PremiumFreeSumInsured" = newSI
      )
    },

    getBasicDataTimeseries = function(premiums, reserves, absCashFlows, absPresentValues, sumInsured=1, policyPeriod, premiumPeriod, ...) {
      res=cbind(
        "PremiumPayment" = c(rep(1, premiumPeriod), rep(0, policyPeriod-premiumPeriod+1)),
        "SumInsured" = c(rep(sumInsured, policyPeriod), 0),
        "Premiums" = absCashFlows$premiums_advance + absCashFlows$premiums_arrears,
        "InterestRate" = rep(self$i, policyPeriod+1),
        "PolicyDuration" = rep(policyPeriod, policyPeriod+1),
        "PremiumPeriod" = rep(premiumPeriod, policyPeriod+1)
      );
      rownames(res) = 0:policyPeriod;
      res
    },

    premiumDecomposition = function(premiums, reserves, absCashFlows, absPresentValues, transitionProbabilities, sumInsured=1, ...) {
      l = dim(reserves)[[1]];
      premium.savings = getSavingsPremium(reserves[,"Zillmer"], self$v) + getSavingsPremium(reserves[,"gamma"], self$v);
      # TODO: Switch to use the Ziller or net or adequate reserve!
      premium.risk    = self$v * (absCashFlows[,"death"] - c(reserves[,"Zillmer"][-1], 0)) * pad0(transitionProbabilities$q, l) +
        self$v * (absCashFlows[,"disease_SumInsured"] - c(reserves[,"Zillmer"][-1], 0)) * pad0(transitionProbabilities$i, l);
      # premium.risk    = self$v * (absCashFlows[,"death"] - c(reserves[,"Zillmer"][-1], 0)) * transitionProbabilities$q;


      res = cbind("savings"=premium.savings, "risk"=premium.risk, "savings+risk"= premium.savings+premium.risk, "gamma"=absCashFlows[,"gamma"]);
      rownames(res) <- rownames(premiums);
      res
    },






    # Dummy to allow commas
    dummy = 0
  )
);
