#' @include HelperFunctions.R
library(R6)
# library(lifecontingencies)
library(objectProperties)
library(lubridate)

TariffTypeEnum = setSingleEnum("TariffType", levels = c("annuity", "wholelife", "endowment", "pureendowment", "terme-fix", "dread-disease"))


# base class for Insurance Tarifs (holding contrat-independent values and
# providing methods to calculate cash flows, premiums, etc.). Objects of this
# class do NOT contain contract-specific values like age, death probabilities,
# premiums, reserves, etc.
InsuranceTarif = R6Class(
  "InsuranceTarif",
  public  = list(
    name  = "Insurance Contract Type",
    tarif = NA,
    desc  = NA,
    tariffType = TariffTypeEnum("wholelife"), # possible values: annuity, wholelife, endowment, pureendowment, terme-fix

    Parameters = InsuranceContract.ParameterStructure,

    initialize = function(name = NULL, type = "wholelife", tarif = "Generic Tarif", desc = "Description of tarif", ...) {
      if (!missing(name))           self$name = name;
      if (!missing(type))           self$tariffType = type;
      if (!missing(tarif))          self$tarif = tarif;
      if (!missing(desc))           self$desc = desc;

      # TODO: Handle costs
      # self$costs = if (!missing(costs)) costs else initializeCosts();

      # Initialize the parameter structure with default values
      self$Parameters = InsuranceContract.ParametersFill(
        i = 0,
        premiumFrequencyOrder = 0,
        benefitFrequencyOrder = 0,
        widowFactor = 0,
        premiumRefund = 0,

        balanceSheetDate = as.Date("1900-12-31"),  # Balance sheet date (for the calculation of the balance sheet reserves)
        balanceSheetMethod = "30/360",

        benefitFrequencyLoading = list("1" = 0.0, "2" = 0.0, "4" = 0.0, "12" = 0.0), # TODO: Properly implement this
        premiumFrequencyLoading = list("1" = 0.0, "2" = 0.0, "4" = 0.0, "12" = 0.0), # TODO: Implement this
        ongoingAlphaGrossPremium = 0,    # Acquisition cost that increase the gross premium
        tax = 0.04,                      # insurance tax, factor on each premium paid
        unitcosts = 0,                   # annual unit cost for each policy (Stückkosten), absolute value
        security = 0,                    # Additional security loading on all benefit payments, factor on all benefits
        noMedicalExam = 0,               # Loading when no medicial exam is done, % of SumInsured
        noMedicalExamRelative = 0,       # Loading when no medicial exam is done, % of gross premium
        sumRebate = 0,                   # gross premium reduction for large premiums, % of SumInsured
        premiumRebate = 0,               # gross premium reduction for large premiums, % of gross premium # TODO
        advanceProfitParticipation = 0,                # Vorweggewinnbeteiligung (%-Satz der Bruttoprämie)
        advanceProfitParticipationInclUnitCost = 0,    # Vorweggewinnbeteiligung (%-Satz der Prämie mit Zu-/Abschlägen, insbesondere nach Stückkosten)
        partnerRebate = 0,

        betaGammaInZillmer = FALSE,      # Whether beta and gamma-costs should be included in the Zillmer premium calculation
        alphaRefundLinear = TRUE         # Whether the refund of alpha-costs on surrender is linear in t or follows the NPV of an annuity
      );
      # Apply the passed arguments to the tariff parameters
      self$Parameters = InsuranceContract.ParametersFill(self$Parameters, ...)

      cat(paste0("Initializing Insurance Tarif ", self$name, "...\n"));
print("InsuranceTariff Default Parameters:");
str(self$Parameters);
    },

    # Retrieve the default Parameters for this tariff (can be overridden for each contract)
    getParameters = function() {
      self$Parameters
    },

    getAges = function(params) {
      ages = ages(params$ActuarialBases$mortalityTable, YOB = params$ContractData$YOB);
      if (age > 0) {
        ages = ages[-age:-1];
      }
      ages
    },

    getTransitionProbabilities = function(params) {
      age = params$ContractData$age;
      ages = self$getAges(params);
      q = deathProbabilities(params$ActuarialBases$mortalityTable, YOB = params$ContractData$YOB);
      if (age > 0) {
        q    = q[-age:-1];
      }
      if (!is.null(params$ActuarialBases$invalidityTable)) {
        i = deathProbabilities(params$ActuarialBases$invalidityTable, YOB=params$ContractData$YOB);
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

    getBasicCashFlows = function(params) {
      age = params$ContractData$age;
      maxAge = getOmega(params$ActuarialBases$mortalityTable)
      maxlen = min(maxAge - age, params$ContractData$policyPeriod);
      cf = data.frame(
        guaranteed = rep(0, maxlen+1),
        survival = rep(0, maxlen+1),
        death = rep(0, maxlen+1),
        disease = rep(0, maxlen+1)
      );
      if (self$tariffType == "annuity") {
        # guaranteed payments exist only with annuities (first n years of the payment)
        cf$guaranteed = c(
            rep(0, params$ContractData$deferral),
            rep(1, params$ContractData$guaranteed),
            rep(0, max(0, maxlen+1 - params$ContractData$deferral - params$ContractData$guaranteed)));
        cf$survival = c(
            rep(0, params$ContractData$deferral + params$ContractData$guaranteed),
            rep(1, max(0, maxlen - params$ContractData$deferral - params$ContractData$guaranteed)),
            0)
      } else if (self$tariffType == "terme-fix") {
        cf$guaranteed = c(rep(0, params$ContractData$policyPeriod), 1);
      } else if (self$tariffType == "dread-disease") {
        cf$disease = c(
            rep(0, params$ContractData$deferral),
            rep(1, maxlen - params$ContractData$deferral),
            0);
      } else {
        if (self$tariffType == "endowment" || self$tariffType == "pureendowment") {
          cf$survival = c(rep(0, params$ContractData$policyPeriod), 1);
        }
        if (self$tariffType == "endowment" || self$tariffType == "wholelife") {
          cf$death = c(
              rep(0, params$ContractData$deferral),
              rep(1, maxlen - params$ContractData$deferral),
              0);
        }
      }
      cf
    },

    getCashFlows = function(params, cashFlowsBasic = NULL, premiumWaiver = FALSE) {
      age = params$ContractData$age;
      maxAge = getOmega(params$ActuarialBases$mortalityTable)

      if (missing(cashFlowsBasic)) {
        cashFlowsBasic = self$getBasicCashFlows(params);
      }
      cflen = length(cashFlowsBasic$survival);
      zeroes = pad0(0, cflen);
      ages = pad0(self$getAges(params), cflen);
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
        premiums = pad0(rep(1, min(params$ContractData$premiumPeriod, params$ContractData$policyPeriod)), cflen);
        if (params$ContractData$premiumPayments == "in advance") {
          cf$premiums_advance = premiums;
        } else {
          cf$premiums_arrears = premiums;
        }
      }

      # Survival Benefits
      if (params$ContractData$benefitPayments == "in advance") {
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
      if (params$ContractData$premiumRefund != 0) {
        totalpremiumcf = cf$premiums_advance + pad0(c(0, cf$premiums_arrears), cflen);

        # death benefit for premium refund is the sum of all premiums so far:
        cf$death_GrossPremium = pad0(Reduce("+", totalpremiumcf[0:params$ContractData$policyPeriod], accumulate=TRUE), cflen)
        cf$death_Refund_past = cf$death_GrossPremium
        cf$death_Refund_past[(cf$death_GrossPremium >0)] = 1;
      }

      cf
    },

    getCashFlowsCosts = function(params, premiumWaiver = FALSE) {
      age = params$ContractData$age;
      maxAge = getOmega(params$ActuarialBases$mortalityTable)
      policyPeriod = params$ContractData$policyPeriod;
      premiumPeriod = params$ContractData$premiumPeriod;

      maxlen = min(maxAge - age, policyPeriod)+1;
      policyPeriod = min(maxAge - age, policyPeriod);
      premiumPeriod = min(policyPeriod, premiumPeriod);

      dm = dim(params$Costs);
      dmnames = dimnames(params$Costs);
      cf = array(0, dim=list(maxlen, dm[1], dm[2]), dimnames=list(0:(maxlen-1), dmnames[[1]], dmnames[[2]]));
      cf[1,,] = cf[1,,] + params$Costs[,,"once"]
      for (i in 1:premiumPeriod) {
        cf[i,,] = cf[i,,] + params$Costs[,,"PremiumPeriod"];
      }
      if (premiumPeriod<policyPeriod) {
        for (i in (premiumPeriod+1):policyPeriod) {
          cf[i,,] = cf[i,,] + params$Costs[,,"PremiumFree"];
        }
      }
      for (i in 1:policyPeriod) {
        cf[i,,] = cf[i,,] + params$Costs[,,"PolicyPeriod"];
      }

      # After premiums are waived, use the gamma_nopremiums instead of gamma:
      if (premiumWaiver) {
        cf[,"gamma",] = cf[,"gamma_nopremiums",];
      }
      cf
    },

    presentValueCashFlows = function(cashFlows, params) {

      len = length(cashFlows$premiums_advance);
      qq = self$getTransitionProbabilities (params);
      qx = pad0(qq$q, len);
      ix = pad0(qq$i, len);
      px = pad0(qq$p, len);

      i = params$ActurialBases$i;
      v = 1/(1+i);
      benefitFreqCorr = correctionPaymentFrequency(
            m = params$ContractData$benefitFrequency, i = i,
            order = params$ActuarialBases$benefitFrequencyOrder);
      premiumFreqCorr = correctionPaymentFrequency(
            m = params$ContractData$premiumFrequency, i = i,
            order = params$ActuarialBases$premiumFrequencyOrder);

      pvRefund = calculatePVDeath (
            px, qx,
            cashFlows$death_GrossPremium,
            v=v);
      pvRefundPast = calculatePVDeath (
            px, qx,
            cashFlows$death_Refund_past,
            v=v) * (cashFlows[,"death_GrossPremium"]-cashFlows[,"premiums_advance"]);

      pv = cbind(
        premiums = calculatePVSurvival (
              px, qx,
              cashFlows$premiums_advance, cashFlows$premiums_arrears,
              m=params$ContractData$premiumFrequency, mCorrection=premiumFreqCorr,
              v=v),
        guaranteed = calculatePVGuaranteed (
              cashFlows$guaranteed_advance, cashFlows$guaranteed_arrears,
              m=params$ContractData$benefitFrequency, mCorrection=benefitFreqCorr,
              v=v),
        survival = calculatePVSurvival (
              px, qx,
              cashFlows$survival_advance, cashFlows$survival_arrears,
              m=params$ContractData$benefitFrequency, mCorrection=benefitFreqCorr,
              v=v),
        death_SumInsured = calculatePVDeath (
              px, qx,
              cashFlows$death_SumInsured,
              v=v),
        disease_SumInsured = calculatePVDisease(
              px, qx, ix,
              cashFlows$disease_SumInsured, v=v),
        death_GrossPremium = pvRefund,
        death_Refund_past = pvRefundPast,
        death_Refund_future = pvRefund - pvRefundPast,
        death_PremiumFree = calculatePVDeath (
              px, qx,
              cashFlows$death_PremiumFree, v=v)
      );

      rownames(pv) <- pad0(rownames(qq), len);
      pv
    },

    presentValueCashFlowsCosts = function(cashFlowsCosts, params) {
      len = dim(cashFlowsCosts)[1];
      q = self$getTransitionProbabilities (params);
      qx = pad0(q$q, len);
      px = pad0(q$p, len);

      v = 1/(1+params$ActuarialBases$i)
      pvc = calculatePVCosts(px, qx, cashFlowsCosts, v=v);
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

    getAbsCashFlows = function(cashFlows, cashFlowsCosts, premiums, premiumSum=0, params) {

      # TODO: Set up a nice list with coefficients for each type of cashflow, rather than multiplying each item manually (this also mitigates the risk of forgetting a dimension, because then the dimensions would not match, while here it's easy to overlook a multiplication)
      # Multiply each CF column by the corresponding basis
      cashFlows[,c("premiums_advance", "premiums_arrears")] = cashFlows[,c("premiums_advance", "premiums_arrears")] * premiums[["gross"]];
      cashFlows[,c("guaranteed_advance", "guaranteed_arrears", "survival_advance", "survival_arrears", "death_SumInsured", "death_PremiumFree", "disease_SumInsured")] =
        cashFlows[,c("guaranteed_advance", "guaranteed_arrears", "survival_advance", "survival_arrears", "death_SumInsured", "death_PremiumFree", "disease_SumInsured")] * params$ContractData$sumInsured;
      cashFlows[,c("death_GrossPremium", "death_Refund_past")] = cashFlows[,c("death_GrossPremium","death_Refund_past")] * premiums[["gross"]] * params$ContractData$premiumRefund;

      # Sum all death-related payments to "death"  and remove the death_GrossPremium column
      cashFlows[,"death_SumInsured"] = cashFlows[,"death_SumInsured"] + cashFlows[,"death_GrossPremium"]
      colnames(cashFlows)[colnames(cashFlows)=="death_SumInsured"] = "death";
      # cashFlows[,"death_GrossPremium"] = NULL;

      cashFlowsCosts = cashFlowsCosts[,,"SumInsured"] * params$ContractData$sumInsured +
        cashFlowsCosts[,,"SumPremiums"] * premiumSum * premiums[["gross"]] +
        cashFlowsCosts[,,"GrossPremium"] * premiums[["gross"]];

      cbind(cashFlows, cashFlowsCosts)
    },

    getAbsPresentValues = function(presentValues, premiums, premiumSum=0, params) {
      pv = presentValues;

      #pv[,"age"] = pv[,"premiums"];
      #colnames(pv)[colnames(pv)=="age"] = "premiums.unit";

      # Multiply each CF column by the corresponding basis
      pv[,"premiums"] = pv[,"premiums"] * premiums[["gross"]];
      pv[,c("guaranteed", "survival", "death_SumInsured", "disease_SumInsured", "death_PremiumFree")] =
        pv[,c("guaranteed", "survival", "death_SumInsured", "disease_SumInsured", "death_PremiumFree")] * params$ContractData$sumInsured;
      pv[,c("death_GrossPremium", "death_Refund_past", "death_Refund_future")] = pv[,c("death_GrossPremium", "death_Refund_past", "death_Refund_future")] * premiums[["gross"]] * params$ContractData$premiumRefund;
      pv[,c("benefits", "benefitsAndRefund", "alpha", "Zillmer", "beta", "gamma", "gamma_nopremiums")] =
        pv[,c("benefits", "benefitsAndRefund", "alpha", "Zillmer", "beta", "gamma", "gamma_nopremiums")] * params$ContractData$sumInsured;

      # Sum all death-related payments to "death"  and remove the death_SumInsured column
      pv[,"death_SumInsured"] = pv[,"death_SumInsured"] + pv[,"death_GrossPremium"]
      colnames(pv)[colnames(pv)=="death_SumInsured"] = "death";

      cbind("premiums.unit"=presentValues[,"premiums"], pv)
    },


    presentValueBenefits = function(presentValues, presentValuesCosts, premiums, premiumSum=0, params) {
      # TODO: Here we don't use the securityLoading parameter => Shall it be used or are these values to be understood without additional security loading?
      benefits    = presentValues[,"survival"] + presentValues[,"death_SumInsured"] + presentValues[,"disease_SumInsured"];
      allBenefits = presentValues[,"survival"] + presentValues[,"death_SumInsured"] + presentValues[,"disease_SumInsured"] + presentValues[,"death_GrossPremium"] * premiums[["unit.gross"]] * params$ContractData$premiumRefund;

      benefitsCosts = presentValuesCosts[,,"SumInsured"] +
        presentValuesCosts[,,"SumPremiums"] * premiumSum * premiums[["unit.gross"]] +
        presentValuesCosts[,,"GrossPremium"] * premiums[["unit.gross"]];

      cbind(
        benefits=benefits,
        benefitsAndRefund=allBenefits,
        benefitsCosts)
    },

    getPremiumCoefficients = function(type="gross", coeffBenefits, coeffCosts, premiumSum = 0, premiums = c("unit.gross"=0), params) {
      # Merge a possibly passed loadings override with the defaults of this class:
      securityLoading = params$Loadings$security;

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
        coeff[["Premium"]][["benefits"]][["death_GrossPremium"]] = -params$ContractData$premiumRefund * (1+securityLoading);
      } else if (type=="net" || type=="Zillmer") {
        coeff[["SumInsured"]][["benefits"]][["death_GrossPremium"]] = premiums[["unit.gross"]] * params$ContractData$premiumRefund * (1+securityLoading);
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
        if (params$Features$betaGammaInZillmer) {
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

    premiumCalculation = function(presentValues, presentValuesCosts, premiumSum=0, params) {

      loadings = params$Loadings;
      sumInsured = params$ContractData$sumInsured
      premiums = c("unit.net" = 0, "unit.Zillmer" = 0, "unit.gross"= 0, "net" = 0, "Zillmer" = 0, "gross" = 0, "written" = 0);
      coefficients = list("gross"=c(), "Zillmer"=c(), "net"=c());

      # net, gross and Zillmer premiums are calculated from the present values using the coefficients on each present value as described in the formulas document
      coeff=self$getPremiumCoefficients("gross", presentValues["0",]*0, presentValuesCosts["0",,]*0, premiums=premiums, premiumSum=premiumSum, params)
      enumerator  = sum(coeff[["SumInsured"]][["benefits"]] * presentValues["0",]) + sum(coeff[["SumInsured"]][["costs"]] * presentValuesCosts["0",,]);
      denominator = sum(coeff[["Premium"   ]][["benefits"]] * presentValues["0",]) + sum(coeff[["Premium"   ]][["costs"]] * presentValuesCosts["0",,]);
      premiums[["unit.gross"]] = enumerator/denominator * (1 + loadings$ongoingAlphaGrossPremium);
      premiums[["gross"]] = premiums[["unit.gross"]] * sumInsured;
      coefficients[["gross"]] = coeff;

      coeff=self$getPremiumCoefficients("net", presentValues["0",]*0, presentValuesCosts["0",,]*0, premiums=premiums, premiumSum=premiumSum, params)
      enumerator  = sum(coeff[["SumInsured"]][["benefits"]] * presentValues["0",]) + sum(coeff[["SumInsured"]][["costs"]] * presentValuesCosts["0",,]);
      denominator = sum(coeff[["Premium"   ]][["benefits"]] * presentValues["0",]) + sum(coeff[["Premium"   ]][["costs"]] * presentValuesCosts["0",,]);
      premiums[["unit.net"]] = enumerator/denominator;
      premiums[["net"]] = premiums[["unit.net"]] * sumInsured;
      coefficients[["net"]] = coeff;

      coeff=self$getPremiumCoefficients("Zillmer", presentValues["0",]*0, presentValuesCosts["0",,]*0, premiums=premiums, premiumSum=premiumSum, params)
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

      frequencyLoading = valueOrFunction(loadings$premiumFrequencyLoading, sumInsured=sumInsured, premiums=premiums);

      premiumBeforeTax = (premiums[["unit.gross"]]*(1+noMedicalExam.relative) + noMedicalExam - sumRebate)*sumInsured * (1-advanceProfitParticipation) + unitCosts;
      premiumBeforeTax = premiumBeforeTax * (1-premiumRebate-advanceProfitParticipationUnitCosts-partnerRebate);
      premiumBeforeTax = premiumBeforeTax * (1+frequencyLoading[[toString(params$ContractData$premiumFrequency)]]) / params$ContractData$premiumFrequency;
      premiums[["written_beforetax"]] = premiumBeforeTax;
      premiums[["tax"]] = premiumBeforeTax * tax;
      premiums[["written"]] = premiumBeforeTax * (1 + tax);

      list("premiums"=premiums, "coefficients"=coefficients)
    },

    reserveCalculation = function (premiums, absPresentValues, absCashFlows, premiumSum=0, reserves = c(), params) {
      securityFactor = (1 + params$Loadings$security);

      # Net, Zillmer and Gross reserves
      resNet = absPresentValues[,"benefitsAndRefund"] * securityFactor - premiums[["net"]] * absPresentValues[,"premiums.unit"];
      BWZcorr = absPresentValues["0", "Zillmer"] / absPresentValues["0", "premiums"] * absPresentValues[,"premiums"];
      resZ = resNet - BWZcorr;

      resAdeq = absPresentValues[,"benefitsAndRefund"] * securityFactor +
        absPresentValues[,"alpha"] + absPresentValues[,"beta"] + absPresentValues["gamma"] -
        premiums[["gross"]] * absPresentValues[,"premiums.unit"];

      #premiums[["Zillmer"]] * absPresentValues[,"premiums"];
      resGamma = absPresentValues[,"gamma"] - absPresentValues["0", "gamma"] / absPresentValues["0", "premiums"] * absPresentValues[,"premiums"]


      resConversion = (resZ + resGamma) * (1 - params$Loadings$advanceProfitParticipation);

      # Alpha refund: Distribute alpha-costs to 5 year (or if shorter, the policy period):
      r = min(params$ContractData$policyPeriod, 5);
      ZillmerSoFar = Reduce("+", absCashFlows$Zillmer, accumulate = TRUE);
      ZillmerTotal = sum(absCashFlows$Zillmer);
      len = length(ZillmerSoFar);
      if (params$Features$alphaRefundLinear) {
        ZillmerVerteilungCoeff = pad0((0:r)/r, len, 1);
      } else {
        q = self$getTransitionProbabilities (params);
        # vector of all ä_{x+t, r-t}
        pvAlphaTmp = calculatePVSurvival(q = pad0(q$q, len), advance = pad0(rep(1,r), len), v = 1/(1+params$ActuarialBases$i));
        ZillmerVerteilungCoeff = (1-pvAlphaTmp/pvAlphaTmp[[1]]);
      }
      alphaRefund = ZillmerSoFar - ZillmerVerteilungCoeff * ZillmerTotal;

      # Reduction Reserve: Reserve used for contract modifications:
      resReduction = pmax(0, resZ+resGamma+alphaRefund) # V_{x,n}^{Rkf}

      # Collect all reserves to one large matrix
      res = cbind("net"=resNet, "Zillmer"=resZ, "adequate"= resAdeq, "gamma"=resGamma,
                  "contractual"=resZ+resGamma, "conversion"=resConversion, "alphaRefund"=alphaRefund, "reduction"=resReduction
                  #, "Reserve.premiumfree"=res.premiumfree, "Reserve.gamma.premiumfree"=res.gamma.premiumfree);
      );
      rownames(res) <- rownames(absPresentValues);

      # The surrender value functions can have arbitrary form, so we store a function
      # here in the tarif and call that, passing the reduction reserve as
      # starting point, but also all reserves, cash flows, premiums and present values
      if (!params$ContractState$surrenderPenalty) {
        # No surrender penalty any more (has already been applied to the first contract change!)
        surrenderValue = resReduction;
      } else if (!is.null(params$ActuarialBases$surrenderValueCalculation)) {
        surrenderValue = params$ActuarialBases$surrenderValueCalculation(
          resReduction, reserves=res, premiums=premiums, absPresentValues=absPresentValues,
          absCashFlows=absCashFlows, premiumSum=premiumSum, params);
      } else {
        # by default, refund the full reduction reserve, except the advance profit participation, which is also included in the reserve, but not charged on the premium!
        surrenderValue = resReduction * (1-params$Loadings$advanceProfitParticipationInclUnitCost);
      }


      # Calculate new sum insured after premium waiver
      Storno = 0; # TODO: Implement storno costs
      newSI = (surrenderValue - absPresentValues[,"death_Refund_past"] * securityFactor - c(Storno)) /
        (absPresentValues[, "benefits"] * securityFactor + absPresentValues[, "gamma_nopremiums"]) * params$ContractData$sumInsured;

      cbind(res,
            "PremiumsPaid"=Reduce("+", absCashFlows$premiums_advance, accumulate = TRUE),
            "Surrender"=surrenderValue,
            "PremiumFreeSumInsured" = newSI
      )
    },

    getBalanceSheetReserveFactor = function(params, years=1) {
      balanceDate = params$ActuarialBases$balanceSheetDate
      year(balanceDate) = year(params$ContractData$contractClosing);
      if (balanceDate < params$ContractData$contractClosing) {
        balanceDate = balanceDate + years(1);
      }

      contractDates = params$ContractData$contractClosing + years(1:years);
      balanceDates = balanceDate + years(1:years);

      if (params$ActuarialBases$balanceSheetMethod == "30/360") {
        baf = ((month(balanceDates + days(1)) - month(contractDates) - 1)%%12 + 1) / 12
      } else if (params$ActuarialBases$balanceSheetMethod == "act/act") {
        baf = as.numeric((balanceDates + days(1)) - contractDates, units="days" ) / as.numeric(balanceDates - (balanceDates - years(1)), units="days")
      } else if (params$ActuarialBases$balanceSheetMethod == "act/360") {
        baf = pmin(as.numeric((balanceDates + days(1)) - contractDates, units="days" ) / 360, 1)
      } else if (params$ActuarialBases$balanceSheetMethod == "act/365") {
        baf = pmin(as.numeric((balanceDates + days(1)) - contractDates, units="days" ) / 365, 1)
      }
      baf
    },

    reserveCalculationBalanceSheet = function (reserves, params) {
      years = length(reserves[,"Zillmer"]);
      # Balance sheet reserves:
      baf = self$getBalanceSheetReserveFactor(params, years = years);
      resZ_BS = (1-baf)*reserves[,"Zillmer"] + baf*c(reserves[-1,"Zillmer"], 0);
      resGamma_BS = (1-baf)*reserves[,"gamma"] + baf*c(reserves[-1,"gamma"], 0);
      res_BS = resZ_BS + resGamma_BS;

      # Collect all reserves to one large matrix
      res = cbind("time" = baf + (1:years) - 1,
                  "Zillmer"               = pmax(resZ_BS,0),
                  "gamma"                 = pmax(resGamma_BS,0),
                  "Balance Sheet Reserve" = pmax(res_BS,0)
      );
      rownames(res) <- rownames(reserves);
      res
    },

    getBasicDataTimeseries = function(premiums, reserves, absCashFlows, absPresentValues, params) {
# sumInsured=1, policyPeriod, premiumPeriod, ...
      res=cbind(
        "PremiumPayment" = c(
              rep(1, params$ContractData$premiumPeriod),
              rep(0, params$ContractData$policyPeriod - params$ContractData$premiumPeriod+1)),
        "SumInsured" = c(
              rep(sumInsured, params$ContractData$policyPeriod),
              0),
        "Premiums" = absCashFlows$premiums_advance + absCashFlows$premiums_arrears,
        "InterestRate" = rep(params$ActuarialBases$i, params$ContractData$policyPeriod+1),
        "PolicyDuration" = rep(params$ContractData$policyPeriod, params$ContractData$policyPeriod+1),
        "PremiumPeriod" = rep(params$ContractData$premiumPeriod, params$ContractData$policyPeriod+1)
      );
      rownames(res) = 0:params$ContractData$policyPeriod;
      res
    },

    premiumDecomposition = function(premiums, reserves, absCashFlows, absPresentValues, transitionProbabilities, params) {
      loadings = params$Loadings;
      sumInsured = params$ContractData$sumInsured;
      v = 1/(1+params$ActuarialBases$i);
      l = dim(reserves)[[1]];

      premium.gross    = absCashFlows[,"premiums_advance"];

      # First get the charges and rebates that are added to the gross premium to obtain the charged premium:

      # charge for no medical exam:
      noMedExam        = valueOrFunction(loadings$noMedicalExam,sumInsured=sumInsured, premiums=premiums);
      noMedExam.rel    = valueOrFunction(loadings$noMedicalExamRelative,sumInsured=sumInsured, premiums=premiums);
      withMedExam      = premium.gross * (1+noMedExam.rel) + noMedExam*sumInsured;
      charge.noMedicalExam = withMedExam - premium.gross;

      # sum rebate:
      sumRebate        = valueOrFunction(loadings$sumRebate,    sumInsured=sumInsured, premiums=premiums);
      afterSumRebate   = withMedExam - sumRebate * sumInsured; # calculate the charge as the difference, because we want a vector!
      rebate.sum       = afterSumRebate - withMedExam;

      # advance profit participation has two parts, one before and one after unit costs. Part 1:
      advanceProfitParticipation = valueOrFunction(loadings$advanceProfitParticipation,sumInsured=sumInsured, premiums=premiums);
      afterProfit      = afterSumRebate * (1+advanceProfitParticipation);
      profits.advance  = afterProfit - afterSumRebate;

      # unit costs
      unitCosts        = valueOrFunction(loadings$unitcosts,    sumInsured=sumInsured, premiums=premiums);
      # unit costs are only charged if a premium is paid, so exclude all times with premium==0!
      afterUnitCosts   = afterProfit + (afterProfit!=0)*unitCosts;
      unitcosts        = afterUnitCosts - afterProfit;

      # advance profit participation, Part 2:
      advanceProfitParticipationUnitCosts = valueOrFunction(loadings$advanceProfitParticipationInclUnitCost, sumInsured=sumInsured, premiums=premiums);
      afterProfit      = afterUnitCosts * (1-advanceProfitParticipationUnitCosts);
      profits.advance  = profits.advance + afterProfit - afterUnitCosts;

      # premium rebate
      premiumRebate    = valueOrFunction(loadings$premiumRebate,sumInsured=sumInsured, premiums=premiums);
      afterPremiumRebate = afterUnitCosts * (1-premiumRebate);
      rebate.premium   = afterPremiumRebate - afterUnitCosts;

      # partner rebate
      partnerRebate    = valueOrFunction(loadings$partnerRebate,sumInsured=sumInsured, premiums=premiums);
      afterPartnerRebate = afterUnitCosts * (1-partnerRebate);
      rebate.partner   = afterPartnerRebate - afterUnitCosts;

      # value after all rebates
      afterRebates     = afterProfit + rebate.premium + rebate.partner;

      # premium frequency loading
      frequencyLoading = valueOrFunction(params$Loadings$premiumFrequencyLoading, sumInsured=sumInsured, premiums=premiums);
      afterFrequency   = afterRebates * (1 + frequencyLoading[[toString(params$ContractData$premiumFrequency)]]);
      charge.frequency = afterFrequency - afterRebates;

      # insurance tax
      taxRate          = valueOrFunction(loadings$tax,          sumInsured=sumInsured, premiums=premiums);
      premium.charged  = afterFrequency * (1+taxRate);
      tax              = premium.charged - afterFrequency;


      # Gross premium = net + zillmeredAlpha + unzillmeredAlpha + beta + gamma premium
      unit.premiumCF   = premium.gross / premiums[["gross"]];
      premium.gamma    = unit.premiumCF * absPresentValues["0", "gamma"] / absPresentValues["0", "premiums.unit"];
      premium.beta     = unit.premiumCF * absPresentValues["0", "beta"] / absPresentValues["0", "premiums.unit"];
      premium.alpha    = unit.premiumCF * absPresentValues["0", "alpha"] / absPresentValues["0", "premiums.unit"];
      premium.Zillmer  = unit.premiumCF * premiums[["Zillmer"]];
      premium.alpha.Zillmer = unit.premiumCF * absPresentValues["0", "Zillmer"] / absPresentValues["0", "premiums.unit"];
      premium.alpha.noZ = premium.alpha - premium.alpha.Zillmer; # ungezillmerter Teil der Abschlusskosten

      premium.net      = unit.premiumCF * premiums[["net"]];

      premium.risk     = v * (absCashFlows[,"death"] - c(reserves[,"net"][-1], 0)) * pad0(transitionProbabilities$q, l);
      premium.savings  = getSavingsPremium(
              reserves[,"net"], v=v,
              survival_advance=absCashFlows[,"survival_advance"]+absCashFlows[,"guaranteed_advance"],
              survival_arrears=absCashFlows[,"survival_arrears"]+absCashFlows[,"guaranteed_arrears"]
      );

      premium.Zillmer.risk     = v * (absCashFlows[,"death"] - c(reserves[,"Zillmer"][-1], 0)) * pad0(transitionProbabilities$q, l);
      premium.Zillmer.savings  = getSavingsPremium(
              reserves[,"Zillmer"], v=v,
              survival_advance=absCashFlows[,"survival_advance"]+absCashFlows[,"guaranteed_advance"],
              survival_arrears=absCashFlows[,"survival_arrears"]+absCashFlows[,"guaranteed_arrears"]
      );
      premium.Zillmer.amortization = getSavingsPremium(
              pmin(0, reserves[,"Zillmer"]), v=v
      );
      premium.Zillmer.actsavings = getSavingsPremium(
              pmax(0, reserves[,"Zillmer"]), v=v,
              survival_advance=absCashFlows[,"survival_advance"]+absCashFlows[,"guaranteed_advance"],
              survival_arrears=absCashFlows[,"survival_arrears"]+absCashFlows[,"guaranteed_arrears"]
      );

      res = cbind(
        "charged"         = premium.charged,
        "tax"             = tax,
        "loading.frequency" = charge.frequency,
        "rebate.premium"  = rebate.premium,
        "rebate.partner"  = rebate.partner,
        "unitcosts"       = unitcosts,
        "profit.advance"  = profits.advance,
        "rebate.sum"      = rebate.sum,
        "charge.noMedicalExam" = charge.noMedicalExam,
        "gross"           = premium.gross,

        "gamma"   = premium.gamma,
        "beta"    = premium.beta,
        "alpha"   = premium.alpha,
        "alpha.noZillmer" = premium.alpha.noZ,
        "alpha.Zillmer" = premium.alpha.Zillmer,
        "Zillmer" = premium.Zillmer,

        "net"     = premium.net,

        "risk"    = premium.risk,
        "savings" = premium.savings,

        "Zillmer.risk"    = premium.Zillmer.risk,
        "Zillmer.savings" = premium.Zillmer.savings,
        "Zillmer.amortization" = premium.Zillmer.amortization,
        "Zillmer.savings.real" = premium.Zillmer.actsavings
      )
      rownames(res) <- rownames(premiums);
      res
    },

    calculateFutureSums = function(values, ...) {
      rcumsum = function(vec) rev(cumsum(rev(vec)));
      apply(values, 2, rcumsum)
    },
    calculatePresentValues = function(values, params) {
      len = dim(values)[1];
      q = self$getTransitionProbabilities (params);
      pv = function(vec) calculatePVSurvival(px=pad0(q$p, len), advance=vec, v=1/(1+params$ActuarialBases$i));
      apply(values, 2, pv)
    },




    # Dummy to allow commas
    dummy = 0
  )
);
