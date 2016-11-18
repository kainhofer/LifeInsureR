#' @include HelperFunctions.R InsuranceParameters.R
#'
#' @import MortalityTables
#' @import R6
#' @importFrom lubridate year month years days
NULL


TariffTypeEnum = setSingleEnum("TariffType", levels = c("annuity", "wholelife", "endowment", "pureendowment", "terme-fix", "dread-disease"))


#' Base class for Insurance Tarifs, providing calculation functions to the contract
#'
#' This is a base class for holding contract-independent values and
#' providing methods to calculate cash flows, premiums, etc. Objects of this
#' class do NOT contain contract-specific values like age, death probabilities,
#' premiums, reserves, etc. Rather, they are the calculation kernels that will
#' be called by the \code{\link{InsuranceContract}} objects to make the actual,
#' tariff-specific calculations.
#'
#' @export
InsuranceTarif = R6Class(
  "InsuranceTarif",
  public  = list(
    name  = "Insurance Contract Type",
    tarif = NULL,
    desc  = NULL,
    tariffType = TariffTypeEnum("wholelife"), # possible values: annuity, wholelife, endowment, pureendowment, terme-fix

    Parameters = InsuranceContract.ParameterStructure,

    initialize = function(name = NULL, type = "wholelife", tarif = "Generic Tarif", desc = "Description of tarif", ...) {
      if (!missing(name))           self$name = name;
      if (!missing(type))           self$tariffType = type;
      if (!missing(tarif))          self$tarif = tarif;
      if (!missing(desc))           self$desc = desc;

      # Set the passed arguments as tariff parameters
      self$Parameters = InsuranceContract.ParametersFill(self$Parameters, ...)

      # Use the profit participation's parameters as fallback for initialized parameters
      ppScheme = self$Parameters$ProfitParticipation$profitParticipationScheme;
      if (!is.null(ppScheme)) {
          self$Parameters$ProfitParticipation = InsuranceContract.ParametersFallback(self$Parameters$ProfitParticipation, ppScheme$Parameters)
      }

      # Fill all remaining uninitialized values with their defaults
      self$Parameters = InsuranceContract.ParametersFallback(self$Parameters, InsuranceContract.ParameterDefaults);
    },

    createModification = function(name  = NULL, tarif = NULL, desc  = NULL, tariffType = NULL, ...) {
      cloned = self$clone();
      if (!missing(name))       cloned$name = name;
      if (!missing(tarif))      cloned$tarif = tarif;
      if (!missing(desc))       cloned$desc = desc;
      if (!missing(tariffType)) cloned$tariffType = tariffType;

      cloned$Parameters = InsuranceContract.ParametersFill(cloned$Parameters, ...);
      cloned
    },

    # Retrieve the default Parameters for this tariff (can be overridden for each contract)
    getParameters = function() {
      self$Parameters
    },

    getAges = function(params) {
      ages = ages(params$ActuarialBases$mortalityTable, YOB = params$ContractData$YOB);
      age = params$ContractData$technicalAge;
      if (age > 0) {
        ages = ages[-age:-1];
      }
      ages
    },

    getTransitionProbabilities = function(params) {
      age = params$ContractData$technicalAge;
      ages = self$getAges(params);
      q = deathProbabilities(params$ActuarialBases$mortalityTable, YOB = params$ContractData$YOB);
      if (age > 0) {
        q    = q[-age:-1];
      }
      if (!is.null(params$ActuarialBases$invalidityTable)) {
        i = deathProbabilities(params$ActuarialBases$invalidityTable, YOB = params$ContractData$YOB);
        if (age > 0) {
          i    = i[-age:-1];
        }
      } else {
        i = rep(0, length(q));
      }
      i = pad0(i, length(q));
      df = data.frame(age = ages, q = q, i = i, p = 1 - q - i, row.names = ages - age)
      df
    },

    getCostValues = function(costs, params) {
        valueOrFunction(costs, params = params, values = NULL)
    },

    # Get the unit premium cash flow for the whole contract period.
    #   - For constant premiums it will be rep(1, premiumPeriod),
    #   - for single premiums it will be c(1, 0, 0, ...),
    #   - for increasing premiums it will be (1+increase)^(0:(premiumPeriod-1))
    # and 0 after the premium period
    getPremiumCF = function(len, params, values) {
        premPeriod = min(params$ContractData$premiumPeriod, params$ContractData$policyPeriod, len);
        if (is.null(params$ContractData$premiumIncrease)) {
            pad0(rep(1, premPeriod - 1), len);
        } else {
            inc = valueOrFunction(params$ContractData$premiumIncrease, premiumPeriod = premPeriod, params = params, values = values)
            if (is.vector(inc) && length(inc) > 1) {
                # If premiumIncrease is (or returns) a vector, treat it as
                # relative premium amounts, ie. c(1, 1.1, 1.2) means +10% of
                # the initial premium for the second and third year
                pad0(inc, len)
            } else {
                pad0(inc ^ (0:(premPeriod - 1)), len)
            }
        }
    },

    # Get the unit annuity cash flow (guaranteed and contingent) for the whole annuity payment period.
    #   - For constant annuity it will be rep(1, annuityPeriod),
    #   - for increasing annuities it will be (1+increase)^(0:(premiumPeriod-1))
    # and 0 after the premium period
    getAnnuityCF = function(len, params, values) {
        annuityPeriod = min(params$ContractData$policyPeriod - params$ContractData$deferralPeriod, len);
        if (is.null(params$ContractData$annuityIncrease)) {
            pad0(rep(1, annuityPeriod), len);
        } else {
            inc = valueOrFunction(params$ContractData$annuityIncrease, annuityPeriod = annuityPeriod, params = params, values = values)
            if (is.vector(inc) && length(inc) > 1) {
                # If annuityIncrease is (or returns) a vector, treat it as
                # relative annuity amounts, ie. c(1, 1.1, 1.2) means +10% of
                # the initial annuity for the second and third year
                pad0(inc, len)
            } else {
                # a numeric value means constant yearly increases (multiplicative)
                pad0(inc ^ (0:annuityPeriod), len)
            }
        }
    },

    # Get the unit death cash flow for the whole protection period.
    #   - For constant death benefit it will be rep(1, policyPeriod),
    #   - for linearly decreasing sum insured it will be (policyPeriod:0)/policyPeriod
    getDeathCF = function(len, params, values) {
        period = params$ContractData$policyPeriod - params$ContractData$deferralPeriod;
        if (is.null(params$ContractData$deathBenefit)) {
            pad0(rep(1, period), len)
        } else {
            benefit = valueOrFunction(params$ContractData$deathBenefit, len = len, params = params, values = values)
            if (is.vector(benefit) && length(benefit) > 1) {
                # If deathBenefit is (or returns) a vector, treat it as
                # relative annuity amounts, ie. c(1, 1.1, 1.2) means +10% of
                # the initial annuity for the second and third year
                pad0(benefit, len)
            } else {
                # constant death benefit
                pad0(rep(benefit, period), len)
            }
        }
    },

    getBasicCashFlows = function(params) {
        age = params$ContractData$technicalAge;
        maxAge = getOmega(params$ActuarialBases$mortalityTable)
        policyPeriod = params$ContractData$policyPeriod;
        deferralPeriod = params$ContractData$deferralPeriod;
        guaranteedPeriod = params$ContractData$guaranteedPeriod;
        maxlen = min(maxAge - age, policyPeriod);

        cf = data.frame(
            guaranteed = rep(0, maxlen + 1),
            survival = rep(0, maxlen + 1),
            death = rep(0, maxlen + 1),
            disease = rep(0, maxlen + 1),
            sumInsured = rep(1, maxlen + 1)
        );
        if (self$tariffType == "annuity") {
            annuityPeriod = maxlen - deferralPeriod;
            annuityCF = self$getAnnuityCF(len = annuityPeriod, params = params, values = values)
            # guaranteed payments exist only with annuities (first n years of the payment)
            cf$guaranteed = c(
                rep(0, deferralPeriod),
                head(annuityCF, n = guaranteedPeriod),
                rep(0, max(0, maxlen + 1 - deferralPeriod - guaranteedPeriod)));
            cf$survival = c(
                rep(0, deferralPeriod + guaranteedPeriod),
                if (guaranteedPeriod > 0) tail(annuityCF, n = -guaranteedPeriod) else annuityCF,
                # rep(1, max(0, maxlen - deferralPeriod - guaranteedPeriod)),
                0)
            cf$sumInsured = c(
                rep(1, deferralPeriod), # increases/decreases of annuities happen only after deferral!
                annuityCF,
                0)

        } else if (self$tariffType == "terme-fix") {
            cf$guaranteed = c(rep(0, policyPeriod), 1);

        } else if (self$tariffType == "dread-disease") {
            cf$disease = c(
                rep(0, deferralPeriod),
                rep(1, maxlen - deferralPeriod),
                0);
        } else {
            # For endowments, use the death factor here in the basic death CF
            # to fix the relation of death to survival benefit
            deathCF = self$getDeathCF(maxlen - deferralPeriod, params = params, values = values)

            if (self$tariffType == "endowment" || self$tariffType == "pureendowment") {
                cf$survival = c(rep(0, policyPeriod), 1);
            }
            if (self$tariffType == "endowment" || self$tariffType == "wholelife") {
                cf$death = c(
                    rep(0, deferralPeriod),
                    deathCF,
                    0);
                cf$sumInsured = c(
                    rep(0, deferralPeriod),
                    deathCF,
                    1);
            }
        }
        cf
    },

    getCashFlows = function(params, values) {
      age = params$ContractData$technicalAge;
      maxAge = getOmega(params$ActuarialBases$mortalityTable)

      if (is.null(values$cashFlowsBasic)) {
        values$cashFlowsBasic = self$getBasicCashFlows(params);
      }
      cflen = length(values$cashFlowsBasic$survival);
      zeroes = pad0(0, cflen);
      ages = pad0(self$getAges(params), cflen);
      cf = data.frame(
        premiums_advance   = zeroes,
        premiums_arrears   = zeroes,
        guaranteed_advance = zeroes,
        guaranteed_arrears = zeroes,
        survival_advance   = zeroes,
        survival_arrears   = zeroes,
        death_SumInsured   = zeroes,
        disease_SumInsured = zeroes,
        death_GrossPremium = zeroes,
        death_Refund_past  = zeroes,
        death_PremiumFree  = zeroes,
        row.names          = ages - age
      );

      # Premiums:
      if (!params$ContractState$premiumWaiver) {
        premiums = self$getPremiumCF(len = cflen, params = params, values = values)
        if (params$ContractData$premiumPayments == "in advance") {
          cf$premiums_advance = premiums;
        } else {
          cf$premiums_arrears = premiums;
        }
      }

      # Survival Benefits
      if (params$ContractData$benefitPayments == "in advance") {
        cf$guaranteed_advance = pad0(values$cashFlowsBasic$guaranteed, cflen);
        cf$survival_advance = pad0(values$cashFlowsBasic$survival, cflen);
      } else {
        cf$guaranteed_arrears = pad0(values$cashFlowsBasic$guaranteed, cflen);
        cf$survival_arrears = pad0(values$cashFlowsBasic$survival, cflen);
      }

      # Death Benefits
      cf$death_SumInsured = pad0(values$cashFlowsBasic$death, cflen);
      cf$disease_SumInsured = pad0(values$cashFlowsBasic$disease, cflen);
      cf$death_PremiumFree = cf$death_SumInsured;
      # premium refund
      if (params$ContractData$premiumRefund != 0) {
        totalpremiumcf = cf$premiums_advance + pad0(c(0, cf$premiums_arrears), cflen);

        # death benefit for premium refund is the sum of all premiums so far:
        cf$death_GrossPremium = pad0(Reduce("+", totalpremiumcf[0:params$ContractData$policyPeriod], accumulate = TRUE), cflen)
        cf$death_Refund_past = cf$death_GrossPremium
        cf$death_Refund_past[(cf$death_GrossPremium > 0)] = 1;
      }

      cf
    },

    getCashFlowsCosts = function(params, values) {
      age = params$ContractData$technicalAge;
      maxAge = getOmega(params$ActuarialBases$mortalityTable)
      policyPeriod = params$ContractData$policyPeriod;
      premiumPeriod = params$ContractData$premiumPeriod;

      maxlen = min(maxAge - age, policyPeriod) + 1;
      policyPeriod = min(maxAge - age, policyPeriod);
      premiumPeriod = min(policyPeriod, premiumPeriod);

      dm = dim(params$Costs);
      dmnames = dimnames(params$Costs);
      cf = array(0, dim = list(maxlen, dm[1], dm[2]), dimnames = list(0:(maxlen - 1), dmnames[[1]], dmnames[[2]]));
      cf[1,,] = cf[1,,] + params$Costs[,,"once"]
      for (i in 1:premiumPeriod) {
        cf[i,,] = cf[i,,] + params$Costs[,,"PremiumPeriod"];
      }
      if (premiumPeriod < policyPeriod) {
        for (i in (premiumPeriod + 1):policyPeriod) {
          cf[i,,] = cf[i,,] + params$Costs[,,"PremiumFree"];
        }
      }
      for (i in 1:policyPeriod) {
        cf[i,,] = cf[i,,] + params$Costs[,,"PolicyPeriod"];
      }

      # After premiums are waived, use the gamma_nopremiums instead of gamma:
      if (params$ContractState$premiumWaiver) {
        cf[,"gamma",] = cf[,"gamma_nopremiums",];
      }

      # some values like sumInsured or gross premium might change over time,
      # so multiply them with the unit cash flows stored in values$cashFlows
      cf[,,"SumInsured"] = cf[,,"SumInsured"] * values$cashFlowsBasic$sumInsured

      cf
    },

    presentValueCashFlows = function(cashFlows, params, values) {

      len = length(values$cashFlows$premiums_advance);
      qq = self$getTransitionProbabilities(params);
      qx = pad0(qq$q, len);
      ix = pad0(qq$i, len);
      px = pad0(qq$p, len);

      i = params$ActuarialBases$i;
      v = 1/(1 + i);
      benefitFreqCorr = correctionPaymentFrequency(
            m = params$ContractData$benefitFrequency, i = i,
            order = params$ActuarialBases$benefitFrequencyOrder);
      premiumFreqCorr = correctionPaymentFrequency(
            m = params$ContractData$premiumFrequency, i = i,
            order = params$ActuarialBases$premiumFrequencyOrder);

      pvRefund = calculatePVDeath(
            px, qx,
            values$cashFlows$death_GrossPremium,
            v = v);
      pvRefundPast = calculatePVDeath(
            px, qx,
            values$cashFlows$death_Refund_past,
            v = v) * (values$cashFlows[,"death_GrossPremium"] - values$cashFlows[,"premiums_advance"]);

      pv = cbind(
        premiums = calculatePVSurvival(
              px, qx,
              values$cashFlows$premiums_advance, values$cashFlows$premiums_arrears,
              m = params$ContractData$premiumFrequency, mCorrection = premiumFreqCorr,
              v = v),
        guaranteed = calculatePVGuaranteed(
              values$cashFlows$guaranteed_advance, values$cashFlows$guaranteed_arrears,
              m = params$ContractData$benefitFrequency, mCorrection = benefitFreqCorr,
              v = v),
        survival = calculatePVSurvival(
              px, qx,
              values$cashFlows$survival_advance, values$cashFlows$survival_arrears,
              m = params$ContractData$benefitFrequency, mCorrection = benefitFreqCorr,
              v = v),
        death_SumInsured = calculatePVDeath(
              px, qx,
              values$cashFlows$death_SumInsured,
              v = v),
        disease_SumInsured = calculatePVDisease(
              px, qx, ix,
              values$cashFlows$disease_SumInsured, v = v),
        death_GrossPremium = pvRefund,
        death_Refund_past = pvRefundPast,
        death_Refund_future = pvRefund - pvRefundPast,
        death_PremiumFree = calculatePVDeath(
              px, qx,
              values$cashFlows$death_PremiumFree, v = v)
      );

      rownames(pv) <- pad0(rownames(qq), len);
      pv
    },

    presentValueCashFlowsCosts = function(params, values) {
      len = dim(values$cashFlowsCosts)[1];
      q = self$getTransitionProbabilities(params);
      qx = pad0(q$q, len);
      px = pad0(q$p, len);

      v = 1/(1 + params$ActuarialBases$i)
      pvc = calculatePVCosts(px, qx, values$cashFlowsCosts, v = v);
      pvc
    },

    # Cost values (CF, present values, etc.) are an Tx5x3 matrix => convert to Tx15 matrix (alpha | Zillmer | beta | gamma)
    costValuesAsMatrix = function(costValues) {
      dm = dim(costValues);
      nm = dimnames(costValues);
      colnames = t(outer(nm[[2]], nm[[3]], paste, sep = "."));

      res = aperm(costValues, c(1,3,2));
      dim(res) = c(dm[[1]], dm[[2]]*dm[[3]]);
      dimnames(res) = list(nm[[1]], colnames)
      res
    },

    getAbsCashFlows = function(params, values) {

        # TODO: Set up a nice list with coefficients for each type of cashflow,
        # rather than multiplying each item manually (this also mitigates the risk
        # of forgetting a dimension, because then the dimensions would not match,
        # while here it's easy to overlook a multiplication)
        # Multiply each CF column by the corresponding basis
        #
        # All propSI cash flows are already set up with the correct multiple
        # of the sumInsured (in cashFlowsBasic) for non-constant sums insured.
        # So here, we don't need to multiply with  values$cashFlowsBasic$sumInsured!
        propGP = c("premiums_advance", "premiums_arrears");
        propSI = c("guaranteed_advance", "guaranteed_arrears",
                   "survival_advance", "survival_arrears", "death_SumInsured",
                   "death_PremiumFree", "disease_SumInsured");
        propPS = c("death_GrossPremium", "death_Refund_past");
      values$cashFlows[,propGP] = values$cashFlows[,propGP] * values$premiums[["gross"]];
      values$cashFlows[,propSI] = values$cashFlows[,propSI] * params$ContractData$sumInsured;
      values$cashFlows[,propPS] = values$cashFlows[,propPS] * values$premiums[["gross"]] * params$ContractData$premiumRefund;

      # Sum all death-related payments to "death"  and remove the death_GrossPremium column
      values$cashFlows[,"death_SumInsured"] = values$cashFlows[,"death_SumInsured"] + values$cashFlows[,"death_GrossPremium"]
      colnames(values$cashFlows)[colnames(values$cashFlows) == "death_SumInsured"] = "death";
      # cashFlows[,"death_GrossPremium"] = NULL;

      # costs relative to sumInsured are already set up as the correct multiple
      # of the original SI, including the dynamic changes over time!
      values$cashFlowsCosts = values$cashFlowsCosts[,,"SumInsured"] * params$ContractData$sumInsured +
        values$cashFlowsCosts[,,"SumPremiums"] * values$unitPremiumSum * values$premiums[["gross"]] +
        values$cashFlowsCosts[,,"GrossPremium"] * values$premiums[["gross"]];

      cbind(values$cashFlows, values$cashFlowsCosts)
    },

    getAbsPresentValues = function(params, values) {
      pv = values$presentValues;

      #pv[,"age"] = pv[,"premiums"];
      #colnames(pv)[colnames(pv)=="age"] = "premiums.unit";

      # Multiply each CF column by the corresponding basis
      pv[,"premiums"] = pv[,"premiums"] * values$premiums[["gross"]];
      pv[,c("guaranteed", "survival", "death_SumInsured", "disease_SumInsured", "death_PremiumFree")] =
        pv[,c("guaranteed", "survival", "death_SumInsured", "disease_SumInsured", "death_PremiumFree")] * params$ContractData$sumInsured;
      pv[,c("death_GrossPremium", "death_Refund_past", "death_Refund_future")] = pv[,c("death_GrossPremium", "death_Refund_past", "death_Refund_future")] * values$premiums[["gross"]] * params$ContractData$premiumRefund;
      pv[,c("benefits", "benefitsAndRefund", "alpha", "Zillmer", "beta", "gamma", "gamma_nopremiums")] =
        pv[,c("benefits", "benefitsAndRefund", "alpha", "Zillmer", "beta", "gamma", "gamma_nopremiums")] * params$ContractData$sumInsured;

      # Sum all death-related payments to "death"  and remove the death_SumInsured column
      pv[,"death_SumInsured"] = pv[,"death_SumInsured"] + pv[,"death_GrossPremium"]
      colnames(pv)[colnames(pv) == "death_SumInsured"] = "death";

      cbind("premiums.unit" = values$presentValues[,"premiums"], pv)
    },


    presentValueBenefits = function(params, values) {
      # TODO: Here we don't use the securityLoading parameter => Shall it be used or are these values to be understood without additional security loading?
      benefits    = values$presentValues[,"survival"] +
                    values$presentValues[,"guaranteed"] +
                    values$presentValues[,"death_SumInsured"] +
                    values$presentValues[,"disease_SumInsured"];
      allBenefits = benefits +
          values$presentValues[,"death_GrossPremium"] * values$premiums[["unit.gross"]] * params$ContractData$premiumRefund;

      benefitsCosts = values$presentValuesCosts[,,"SumInsured"] +
        values$presentValuesCosts[,,"SumPremiums"] * values$unitPremiumSum * values$premiums[["unit.gross"]] +
        values$presentValuesCosts[,,"GrossPremium"] * values$premiums[["unit.gross"]];

      cbind(
        benefits = benefits,
        benefitsAndRefund = allBenefits,
        benefitsCosts)
    },

    # When getPremiumCoefficients is called, the values$premiums array has NOT been filled! Instead,
    # some of the premium fields (all required for the current calculation) have
    # been set in the passed "premiums" argument.
    getPremiumCoefficients = function(type="gross", coeffBenefits, coeffCosts, premiums, params, values) {
      # Merge a possibly passed loadings override with the defaults of this class:
      securityLoading = params$Loadings$security;

      coeff = list(
        "SumInsured" = list("benefits" = coeffBenefits*0, "costs" = coeffCosts*0),
        "Premium"    = list("benefits" = coeffBenefits*0, "costs" = coeffCosts*0)
      );

      coeff[["Premium"]][["benefits"]][["premiums"]]            = 1;

      coeff[["SumInsured"]][["benefits"]][["guaranteed"]]       = 1 + securityLoading;
      coeff[["SumInsured"]][["benefits"]][["survival"]]         = 1 + securityLoading;
      coeff[["SumInsured"]][["benefits"]][["death_SumInsured"]] = 1 + securityLoading;
      coeff[["SumInsured"]][["benefits"]][["disease_SumInsured"]] = 1 + securityLoading;

      # Premium refund is handled differently for gross and net premiums, because it is proportional to the gross premium
      if (type == "gross") {
        coeff[["Premium"]][["benefits"]][["death_GrossPremium"]] = -params$ContractData$premiumRefund * (1 + securityLoading);
      } else if (type == "net" || type == "Zillmer") {
        coeff[["SumInsured"]][["benefits"]][["death_GrossPremium"]] = premiums[["unit.gross"]] * params$ContractData$premiumRefund * (1 + securityLoading);
      }


      # coefficients for the costs

      if (type == "gross") {
        coeff[["SumInsured"]][["costs"]]["alpha", "SumInsured"] = 1;
        coeff[["SumInsured"]][["costs"]]["beta",  "SumInsured"] = 1;
        coeff[["SumInsured"]][["costs"]]["gamma", "SumInsured"] = 1;
        # TODO: How to handle beta costs proportional to Sum Insured
        coeff[["Premium"]][["costs"]]["alpha", "SumPremiums"] = -values$unitPremiumSum;
        coeff[["Premium"]][["costs"]]["beta",  "SumPremiums"] = -values$unitPremiumSum;
        coeff[["Premium"]][["costs"]]["gamma", "SumPremiums"] = -values$unitPremiumSum;

        coeff[["Premium"]][["costs"]]["alpha", "GrossPremium"] = -1;
        coeff[["Premium"]][["costs"]]["beta",  "GrossPremium"] = -1;
        coeff[["Premium"]][["costs"]]["gamma", "GrossPremium"] = -1;

      } else if (type == "Zillmer") {
        coeff[["SumInsured"]][["costs"]]["Zillmer","SumInsured"] = 1;
        coeff[["SumInsured"]][["costs"]]["Zillmer","SumPremiums"] = values$unitPremiumSum * premiums[["unit.gross"]];
        coeff[["SumInsured"]][["costs"]]["Zillmer","GrossPremium"] = premiums[["unit.gross"]];
        if (params$Features$betaGammaInZillmer) {
          coeff[["SumInsured"]][["costs"]]["beta",  "SumInsured"] = 1;
          coeff[["SumInsured"]][["costs"]]["gamma", "SumInsured"] = 1;
          coeff[["SumInsured"]][["costs"]]["beta",  "SumPremiums"] = values$unitPremiumSum * premiums[["unit.gross"]];
          coeff[["SumInsured"]][["costs"]]["gamma", "SumPremiums"] = values$unitPremiumSum * premiums[["unit.gross"]];
          coeff[["SumInsured"]][["costs"]]["beta",  "GrossPremium"] = premiums[["unit.gross"]];
          coeff[["SumInsured"]][["costs"]]["gamma", "GrossPremium"] = premiums[["unit.gross"]];
        }
      }

      coeff
    },

    premiumCalculation = function(params, values) {
      loadings = params$Loadings;
      sumInsured = params$ContractData$sumInsured
      values$premiums = c("unit.net" = 0, "unit.Zillmer" = 0, "unit.gross" = 0, "net" = 0, "Zillmer" = 0, "gross" = 0, "written" = 0);
      coefficients = list("gross" = c(), "Zillmer" = c(), "net" = c());

      # net, gross and Zillmer premiums are calculated from the present values using the coefficients on each present value as described in the formulas document
      coeff = self$getPremiumCoefficients("gross", values$presentValues["0",]*0, values$presentValuesCosts["0",,]*0, premiums = values$premiums, params = params, values = values)
      enumerator  = sum(coeff[["SumInsured"]][["benefits"]] * values$presentValues["0",]) + sum(coeff[["SumInsured"]][["costs"]] * values$presentValuesCosts["0",,]);
      denominator = sum(coeff[["Premium"   ]][["benefits"]] * values$presentValues["0",]) + sum(coeff[["Premium"   ]][["costs"]] * values$presentValuesCosts["0",,]);
      values$premiums[["unit.gross"]] = enumerator/denominator * (1 + loadings$ongoingAlphaGrossPremium);
      values$premiums[["gross"]] = values$premiums[["unit.gross"]] * sumInsured;
      coefficients[["gross"]] = coeff;

      coeff = self$getPremiumCoefficients("net", values$presentValues["0",]*0, values$presentValuesCosts["0",,]*0, premiums = values$premiums, params = params, values = values)
      enumerator  = sum(coeff[["SumInsured"]][["benefits"]] * values$presentValues["0",]) + sum(coeff[["SumInsured"]][["costs"]] * values$presentValuesCosts["0",,]);
      denominator = sum(coeff[["Premium"   ]][["benefits"]] * values$presentValues["0",]) + sum(coeff[["Premium"   ]][["costs"]] * values$presentValuesCosts["0",,]);
      values$premiums[["unit.net"]] = enumerator/denominator;
      values$premiums[["net"]] = values$premiums[["unit.net"]] * sumInsured;
      coefficients[["net"]] = coeff;

      coeff = self$getPremiumCoefficients("Zillmer", values$presentValues["0",]*0, values$presentValuesCosts["0",,]*0, premiums = values$premiums, params = params, values = values);
      enumerator  = sum(coeff[["SumInsured"]][["benefits"]] * values$presentValues["0",]) + sum(coeff[["SumInsured"]][["costs"]] * values$presentValuesCosts["0",,]);
      denominator = sum(coeff[["Premium"   ]][["benefits"]] * values$presentValues["0",]) + sum(coeff[["Premium"   ]][["costs"]] * values$presentValuesCosts["0",,]);
      values$premiums[["unit.Zillmer"]] = enumerator/denominator;
      values$premiums[["Zillmer"]] = values$premiums[["unit.Zillmer"]] * sumInsured;
      coefficients[["Zillmer"]] = coeff;


      # The written premium is the gross premium with additional loadings, rebates, unit costs and taxes
      tax           = valueOrFunction(loadings$tax,          params = params, values = values);
      unitCosts     = valueOrFunction(loadings$unitcosts,    params = params, values = values);
      noMedicalExam = valueOrFunction(loadings$noMedicalExam,params = params, values = values);
      noMedicalExam.relative = valueOrFunction(loadings$noMedicalExamRelative,params = params, values = values);
      extraRebate   = valueOrFunction(loadings$extraRebate,  params = params, values = values);
      sumRebate     = valueOrFunction(loadings$sumRebate,    params = params, values = values);
      premiumRebate = valueOrFunction(loadings$premiumRebate,params = params, values = values);
      extraChargeGrossPremium = valueOrFunction(loadings$extraChargeGrossPremium, params = params, values = values);
      advanceProfitParticipation = 0;
      advanceProfitParticipationUnitCosts = 0;
      ppScheme      = params$ProfitParticipation$profitParticipationScheme;
      if (!is.null(ppScheme)) {
          advanceProfitParticipation = ppScheme$getAdvanceProfitParticipation(params = params, values = values)
          advanceProfitParticipationUnitCosts = ppScheme$getAdvanceProfitParticipationAfterUnitCosts(params = params, values = values)
      }
      if (is.null(advanceProfitParticipation)) advanceProfitParticipation = 0;
      if (is.null(advanceProfitParticipationUnitCosts)) advanceProfitParticipationUnitCosts = 0;

      partnerRebate = valueOrFunction(loadings$partnerRebate, params = params, values = values);

      frequencyLoading = valueOrFunction(loadings$premiumFrequencyLoading, params = params, values = values);
      premiumBeforeTax = (values$premiums[["unit.gross"]]*(1 + noMedicalExam.relative + extraChargeGrossPremium) + noMedicalExam - sumRebate - extraRebate) * sumInsured * (1 - advanceProfitParticipation) + unitCosts;
      premiumBeforeTax = premiumBeforeTax * (1 - premiumRebate - advanceProfitParticipationUnitCosts - partnerRebate);
      # TODO / FIXME: Add a check that frequencyLoading has an entry for the premiumFrequency -> Otherwise do not add any loading (currently NULL is returned, basically setting all premiums to NULL)
      premiumBeforeTax = premiumBeforeTax * (1 + frequencyLoading[[toString(params$ContractData$premiumFrequency)]]) / params$ContractData$premiumFrequency;
      values$premiums[["written_beforetax"]] = premiumBeforeTax;
      values$premiums[["tax"]] = premiumBeforeTax * tax;
      values$premiums[["written"]] = premiumBeforeTax * (1 + tax);

      list("premiums" = values$premiums, "coefficients" = coefficients)
    },

    reserveCalculation = function(params, values) {
      securityFactor = (1 + params$Loadings$security);
      ppScheme      = params$ProfitParticipation$profitParticipationScheme;

      # Net, Zillmer and Gross reserves
      resNet = values$absPresentValues[,"benefitsAndRefund"] * securityFactor - values$premiums[["net"]] * values$absPresentValues[,"premiums.unit"];
      BWZcorr = values$absPresentValues["0", "Zillmer"] / values$absPresentValues["0", "premiums"] * values$absPresentValues[,"premiums"];
      resZ = resNet - BWZcorr;

      resAdeq = values$absPresentValues[,"benefitsAndRefund"] * securityFactor +
        values$absPresentValues[,"alpha"] + values$absPresentValues[,"beta"] + values$absPresentValues["gamma"] -
        values$premiums[["gross"]] * values$absPresentValues[,"premiums.unit"];

      #values$premiums[["Zillmer"]] * values$absPresentValues[,"premiums"];
      resGamma = values$absPresentValues[,"gamma"] - values$absPresentValues["0", "gamma"] / values$absPresentValues["0", "premiums"] * values$absPresentValues[,"premiums"]

      advanceProfitParticipation = 0;
      if (!is.null(ppScheme)) {
          advanceProfitParticipation = ppScheme$getAdvanceProfitParticipation(params = params, values = values)
      }
      resConversion = (resZ + resGamma) * (1 - advanceProfitParticipation);

      # Alpha refund: Distribute alpha-costs to 5 year (or if shorter, the policy period):
      r = min(params$ContractData$policyPeriod, 5);
      ZillmerSoFar = Reduce("+", values$absCashFlows$Zillmer, accumulate = TRUE);
      ZillmerTotal = sum(values$absCashFlows$Zillmer);
      len = length(ZillmerSoFar);
      if (params$Features$alphaRefundLinear) {
        ZillmerVerteilungCoeff = pad0((0:r)/r, len, 1);
      } else {
        q = self$getTransitionProbabilities(params);
        # vector of all ä_{x+t, r-t}
        pvAlphaTmp = calculatePVSurvival(q = pad0(q$q, len), advance = pad0(rep(1,r), len), v = 1/(1 + params$ActuarialBases$i));
        ZillmerVerteilungCoeff = (1 - pvAlphaTmp/pvAlphaTmp[[1]]);
      }
      alphaRefund = ZillmerSoFar - ZillmerVerteilungCoeff * ZillmerTotal;

      # Reduction Reserve: Reserve used for contract modifications:
      resReduction = pmax(0, resZ + resGamma + alphaRefund) # V_{x,n}^{Rkf}

      # Collect all reserves to one large matrix
      res = cbind(
            "net"         = resNet,
            "Zillmer"     = resZ,
            "adequate"    = resAdeq,
            "gamma"       = resGamma,
            "contractual" = resZ + resGamma,
            "conversion"  = resConversion,
            "alphaRefund" = alphaRefund,
            "reduction"   = resReduction
            #, "Reserve.premiumfree"=res.premiumfree, "Reserve.gamma.premiumfree"=res.gamma.premiumfree);
      );
      rownames(res) <- rownames(values$absPresentValues);
      values$reserves = res;

      # The surrender value functions can have arbitrary form, so we store a function
      # here in the tarif and call that, passing the reduction reserve as
      # starting point, but also all reserves, cash flows, premiums and present values
      if (!params$ContractState$surrenderPenalty) {
          # No surrender penalty any more (has already been applied to the first contract change!)
          surrenderValue = resReduction;
      } else if (!is.null(params$ActuarialBases$surrenderValueCalculation)) {
          surrenderValue = params$ActuarialBases$surrenderValueCalculation(resReduction, params, values);
      } else {
          # by default, refund the full reduction reserve, except the advance profit participation, which is also included in the reserve, but not charged on the premium!
          advanceProfitParticipationUnitCosts = 0;
          ppScheme      = params$ProfitParticipation$profitParticipationScheme;
          if (!is.null(ppScheme)) {
              advanceProfitParticipationUnitCosts = ppScheme$getAdvanceProfitParticipationAfterUnitCosts(params = params, values = values)
          }
          partnerRebate = valueOrFunction(params$Loadings$partnerRebate, params = params, values = values);
          surrenderValue = resReduction * (1 - advanceProfitParticipationUnitCosts - partnerRebate);
      }


      # Calculate new sum insured after premium waiver
      Storno = 0; # TODO: Implement storno costs
      newSI = (surrenderValue - values$absPresentValues[,"death_Refund_past"] * securityFactor - c(Storno)) /
        (values$absPresentValues[, "benefits"] * securityFactor + values$absPresentValues[, "gamma_nopremiums"]) * params$ContractData$sumInsured;

      cbind(res,
            "PremiumsPaid" = Reduce("+", values$absCashFlows$premiums_advance, accumulate = TRUE),
            "Surrender" = surrenderValue,
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
        baf = ((month(balanceDates + days(1)) - month(contractDates) - 1) %% 12 + 1) / 12
      } else if (params$ActuarialBases$balanceSheetMethod == "act/act") {
        baf = as.numeric((balanceDates + days(1)) - contractDates, units = "days" ) / as.numeric(balanceDates - (balanceDates - years(1)), units = "days")
      } else if (params$ActuarialBases$balanceSheetMethod == "act/360") {
        baf = pmin(as.numeric((balanceDates + days(1)) - contractDates, units = "days" ) / 360, 1)
      } else if (params$ActuarialBases$balanceSheetMethod == "act/365") {
        baf = pmin(as.numeric((balanceDates + days(1)) - contractDates, units = "days" ) / 365, 1)
      }
      baf
    },

    reserveCalculationBalanceSheet = function(params, values) {
      reserves = values$reserves;
      years = length(reserves[,"Zillmer"]);
      # Balance sheet reserves:
      baf = self$getBalanceSheetReserveFactor(params, years = years);
      resZ_BS = (1 - baf) * reserves[,"Zillmer"] + baf * c(reserves[-1, "Zillmer"], 0);
      resGamma_BS = (1 - baf) * reserves[,"gamma"] + baf * c(reserves[-1, "gamma"], 0);
      res_BS = resZ_BS + resGamma_BS;

      # Premium transfer / unearned premium:
      bm = month(params$ContractData$contractClosing)
      freq = params$ContractData$premiumFrequency
      # TODO: We have no vector of actual written premiums (implicit assumption
      # seems to be that the premium stays constant!). Once we have such a vector,
      # rewrite the following code
      fact = (bm - 1) %% (12/freq) / 12 * freq
      unearnedPremiums = fact * values$cashFlows$premiums_advance * values$premiums[["written_beforetax"]] # TODO
      # If advance profit participation is granted, unearned premiums still apply to the whole gross premium without PP and partner rebate!
      ppScheme      = params$ProfitParticipation$profitParticipationScheme;
      if (!is.null(ppScheme)) {
          partnerRebate = valueOrFunction(params$Loadings$partnerRebate, params = params, values = values);
          advanceProfitParticipation = ppScheme$getAdvanceProfitParticipationAfterUnitCosts(params = params, values = values);
          unearnedPremiums = unearnedPremiums / (1 - partnerRebate - advanceProfitParticipation);
      }

      # Collect all reserves to one large matrix
      res = cbind("time" = baf + (1:years) - 1,
                  "Zillmer"               = pmax(resZ_BS,0),
                  "gamma"                 = pmax(resGamma_BS,0),
                  "Balance Sheet Reserve" = pmax(res_BS,0),
                  "unearned Premiums"     = unearnedPremiums
      );
      rownames(res) <- rownames(reserves);
      res
    },

    calculateProfitParticipation = function(params, values, ...) {
        ppScheme = params$ProfitParticipation$profitParticipationScheme;
        if (!is.null(ppScheme)) {
            ppScheme$getProfitParticipation(params = params, values = values, ...)
        }
    },

    reservesWithProfit = function(params, values) {
        # TODO
    },


    getBasicDataTimeseries = function(params, values) {
        policyPeriod  = params$ContractData$policyPeriod;
        premiumPeriod = params$ContractData$premiumPeriod;
        res = cbind(
            "PremiumPayment" = c(
                rep(1, premiumPeriod),
                rep(0, policyPeriod - premiumPeriod + 1)),
            "SumInsured" = c(
                rep(params$ContractData$sumInsured, policyPeriod),
                0),
            "Premiums" = c(
                values$absCashFlows$premiums_advance + values$absCashFlows$premiums_arrears,
                rep(0, policyPeriod - length(values$absCashFlows$premiums_advance) + 1)),
            "InterestRate" = rep(params$ActuarialBases$i, policyPeriod + 1),
            "PolicyDuration" = rep(policyPeriod, policyPeriod + 1),
            "PremiumPeriod" = rep(premiumPeriod, policyPeriod + 1)
        );
        rownames(res) = 0:policyPeriod;
        res
    },

    premiumDecomposition = function(params, values) {
      loadings   = params$Loadings;
      sumInsured = params$ContractData$sumInsured;
      premiums   = values$premiums;
      v          = 1/(1 + params$ActuarialBases$i);
      l          = dim(values$reserves)[[1]];
      ppScheme   = params$ProfitParticipation$profitParticipationScheme;

      # TODO: This assumes all premiums are paid in advance!
      premium.gross    = values$absCashFlows[,"premiums_advance"];

      # First get the charges and rebates that are added to the gross premium to obtain the charged premium:

      # charge for no medical exam:
      extraChargeGrossPremium = valueOrFunction(loadings$extraChargeGrossPremium, params = params, values = values);
      noMedExam        = valueOrFunction(loadings$noMedicalExam,params = params, values = values);
      noMedExam.rel    = valueOrFunction(loadings$noMedicalExamRelative,params = params, values = values);
      withMedExam      = premium.gross * (1 + noMedExam.rel + extraChargeGrossPremium) + noMedExam * sumInsured;
      charge.noMedicalExam = withMedExam - premium.gross;

      # sum rebate:
      sumRebate        = valueOrFunction(loadings$sumRebate,    params = params, values = values);
      extraRebate      = valueOrFunction(loadings$extraRebate,  params = params, values = values);
      afterSumRebate   = withMedExam - (sumRebate + extraRebate) * sumInsured; # calculate the charge as the difference, because we want a vector!
      rebate.sum       = afterSumRebate - withMedExam;

      # advance profit participation has two parts, one before and one after unit costs. Part 1:
      advanceProfitParticipation = 0;
      if (!is.null(ppScheme)) {
          advanceProfitParticipation = ppScheme$getAdvanceProfitParticipation(params = params, values = values)
      }
      afterProfit      = afterSumRebate * (1 + advanceProfitParticipation);
      profits.advance  = afterProfit - afterSumRebate;

      # unit costs
      unitCosts        = valueOrFunction(loadings$unitcosts,    params = params, values = values);
      # unit costs are only charged if a premium is paid, so exclude all times with premium==0!
      afterUnitCosts   = afterProfit + (afterProfit != 0)*unitCosts;
      unitcosts        = afterUnitCosts - afterProfit;

      # advance profit participation, Part 2:
      advanceProfitParticipationUnitCosts = 0;
      if (!is.null(ppScheme)) {
          advanceProfitParticipationUnitCosts = ppScheme$getAdvanceProfitParticipationAfterUnitCosts(params = params, values = values)
      }
      afterProfit      = afterUnitCosts * (1 - advanceProfitParticipationUnitCosts);
      profits.advance  = profits.advance + afterProfit - afterUnitCosts;

      # premium rebate
      premiumRebate    = valueOrFunction(loadings$premiumRebate,params = params, values = values);
      afterPremiumRebate = afterUnitCosts * (1 - premiumRebate);
      rebate.premium   = afterPremiumRebate - afterUnitCosts;

      # partner rebate
      partnerRebate    = valueOrFunction(loadings$partnerRebate,params = params, values = values);
      afterPartnerRebate = afterUnitCosts * (1 - partnerRebate);
      rebate.partner   = afterPartnerRebate - afterUnitCosts;

      # value after all rebates
      afterRebates     = afterProfit + rebate.premium + rebate.partner;

      # premium frequency loading
      frequencyLoading = valueOrFunction(params$Loadings$premiumFrequencyLoading, params = params, values = values);

      afterFrequency   = afterRebates * (1 + frequencyLoading[[toString(params$ContractData$premiumFrequency)]]);
      charge.frequency = afterFrequency - afterRebates;

      # insurance tax
      taxRate          = valueOrFunction(loadings$tax,          params = params, values = values);
      premium.charged  = afterFrequency * (1 + taxRate);
      tax              = premium.charged - afterFrequency;


      # Gross premium = net + zillmeredAlpha + unzillmeredAlpha + beta + gamma premium
      unit.premiumCF   = premium.gross / premiums[["gross"]];
      premium.gamma    = unit.premiumCF * values$absPresentValues["0", "gamma"] / values$absPresentValues["0", "premiums.unit"];
      premium.beta     = unit.premiumCF * values$absPresentValues["0", "beta"]  / values$absPresentValues["0", "premiums.unit"];
      premium.alpha    = unit.premiumCF * values$absPresentValues["0", "alpha"] / values$absPresentValues["0", "premiums.unit"];
      premium.Zillmer  = unit.premiumCF * premiums[["Zillmer"]];
      premium.alpha.Zillmer = unit.premiumCF * values$absPresentValues["0", "Zillmer"] / values$absPresentValues["0", "premiums.unit"];
      premium.alpha.noZ = premium.alpha - premium.alpha.Zillmer; # ungezillmerter Teil der Abschlusskosten

      premium.net      = unit.premiumCF * premiums[["net"]];

      premium.risk     = v * (values$absCashFlows[,"death"] - c(values$reserves[,"net"][-1], 0)) * pad0(values$transitionProbabilities$q, l);
      premium.savings  = getSavingsPremium(
        values$reserves[,"net"], v = v,
              survival_advance = values$absCashFlows[,"survival_advance"] + values$absCashFlows[,"guaranteed_advance"],
              survival_arrears = values$absCashFlows[,"survival_arrears"] + values$absCashFlows[,"guaranteed_arrears"]
      );

      # premium.Zillmer.risk     = v * (values$absCashFlows[,"death"] - c(values$reserves[,"Zillmer"][-1], 0)) * pad0(values$transitionProbabilities$q, l);
      # premium.Zillmer.savings  = getSavingsPremium(
      #         values$reserves[,"Zillmer"], v = v,
      #         survival_advance = values$absCashFlows[,"survival_advance"] + values$absCashFlows[,"guaranteed_advance"],
      #         survival_arrears = values$absCashFlows[,"survival_arrears"] + values$absCashFlows[,"guaranteed_arrears"]
      # );
      premium.Zillmer.risk     = v * (values$absCashFlows[,"death"] - c(values$reserves[,"contractual"][-1], 0)) * pad0(values$transitionProbabilities$q, l);
      premium.Zillmer.savings  = getSavingsPremium(
          values$reserves[,"contractual"], v = v,
          survival_advance = values$absCashFlows[,"survival_advance"] + values$absCashFlows[,"guaranteed_advance"],
          survival_arrears = values$absCashFlows[,"survival_arrears"] + values$absCashFlows[,"guaranteed_arrears"]
      );
      premium.Zillmer.amortization = getSavingsPremium(
              pmin(0, values$reserves[,"contractual"]), v = v
      );
      premium.Zillmer.actsavings = getSavingsPremium(
              pmax(0, values$reserves[,"contractual"]), v = v,
              survival_advance = values$absCashFlows[,"survival_advance"] + values$absCashFlows[,"guaranteed_advance"],
              survival_arrears = values$absCashFlows[,"survival_arrears"] + values$absCashFlows[,"guaranteed_arrears"]
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
      q = self$getTransitionProbabilities(params);
      pv = function(vec) calculatePVSurvival(px = pad0(q$p, len), advance = vec, v = 1/(1 + params$ActuarialBases$i));
      apply(values, 2, pv)
    },




    # Dummy to allow commas
    dummy = 0
  )
)

