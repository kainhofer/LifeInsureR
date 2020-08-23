#' @include HelperFunctions.R InsuranceParameters.R InsuranceTarif.R ProfitParticipation.R
#'
#' @import MortalityTables
#' @import R6
NULL

############ Class InsuranceContract ###########################################
#' Base Class for Insurance Contracts
#'
#' R6 class that models a complete, general insurance contract.
#' The corresponding tariff and the profit participation scheme, as well as
#' all other relevant contract parameters (if not defined by the tariff or
#' explicitly overridden by the contract) can be given in the constructor.
#'
#' ## Usage
#'
#' The typical usage of this class is to simply call [InsuranceContract$new()].
#'
#' All parameters from the [InsuranceContract.ParameterDefaults] can be passed
#' to the constructor of the class (i.e. the [InsuranceContract$new()]-call).
#' Parameters not explicitly given, will be taken from the tariff or as a fall-back
#' mechanism from the [InsuranceContract.ParameterDefaults] defaults.
#'
#' Immediately upon construction, all premiums, reserves and cash flows for the
#' whole contract period are calculated and can be accessed via the \code{Values}
#' field of the object.
#'
#'
#' ## Calculation approach: Cash Flows
#'
#' An insurance contract is basically defined by the (unit) cash flows it produces:
#' \itemize{
#'   \item Premium payments (in advance or in arrears) at each timestep
#'   \item Survival payments at each timestep
#'   \item Death benefits at each timestep
#'   \item Disease benefits at each timestep
#'   \item Guaranteed payments at each timestep
#' }
#' Together with the transition probabilities (mortalityTable parameter)
#' the present values can be calculated, from which the premiums follow and
#' finally the reserves and a potential profit sharing.
#'
#' For example, a term life insurance with regular premiums would have the following
#' cash flows:
#'
#' * premium cash flows: 1, 1, 1, 1, 1, ...
#' * survival cash flows: 0, 0, 0, 0, 0, ...
#' * guaranteed cash flows: 0, 0, 0, 0, 0, ...
#' * death benefit cash flows: 1, 1, 1, 1, 1, ...
#'
#' A single-premium term life insurance would look similar, except for the premiums:
#'
#' * premium cash flows: 1, 0, 0, 0, 0, ...
#'
#' A pure endowment has no death benefits, but a survival benefit of 1 at the
#' maturity of the contract:
#'
#' * premium cash flows: 1, 1, 1, 1, 1, ...
#' * survival cash flows: 0, 0, ..., 0, 1
#' * guaranteed cash flows: 0, 0, 0, 0, 0, ...
#' * death benefit cash flows: 0, 0, 0, 0, 0, ...
#'
#' An endowment has also death benefits during the contract duration:
#'
#' * premium cash flows: 1, 1, 1, 1, 1, ...
#' * survival cash flows: 0, 0, ..., 0, 1
#' * guaranteed cash flows: 0, 0, 0, 0, 0, ...
#' * death benefit cash flows: 1, 1, 1, 1, 1, ...
#'
#' A (deferred) annuity has premium cash flows only during the deferral peroid
#' and only survival cash flows during the annuity payment phase. Often, in case
#' of death during the deferral period, all premiums paid are refunded as a death
#' benefit.:
#'
#' * premium cash flows: 1, 1, ...,  1, 0, 0, 0, ...
#' * survival cash flows: 0, 0, ..., 0, 1, 1, 1,...
#' * guaranteed cash flows: 0, 0, 0, 0, 0, ...
#' * death benefit cash flows: 1, 2, 3, 4, 5, ..., 0, 0, ...
#'
#' A terme-fix insurance has a guaranteed payment at maturity, even if the insured
#' has already died. The premiums, however, are only paid until death (which is
#' not reflected in the contingent cash flows, but rather in the transition
#' probabilities):
#'
#' * premium cash flows: 1, 1, 1, 1, ...,  1
#' * survival cash flows: 0, 0, 0, 0, ..., 0
#' * guaranteed cash flows: 0, 0, 0, ..., 0, 1
#' * death benefit cash flows: 0, 0, 0, 0, ..., 0
#'
#' ## Calculation approach: Valuation
#'
#' The calculation of all contract values is controlled by the function
#' [InsuranceContract$calculateContract()] (using methods of the [InsuranceTarif]
#' object) and follows the following logic:
#'
#' 1. Once the (unit) cash flows and the transition probbilities are determined,
#' the actuarial equivalence principle states that at time of inception, the
#' (net and gross) premium must be determined in a way that the present value
#' of the future benefits and costs minus the present value of the future premiums
#' must be equal, i.e. in expectation the future premiums ove the whole lifetime
#' of the contract will exactly cover the benefits and costs. Similarly, at all
#' later time steps, the difference between these two present values needs to be
#' reserved (i.e. has already been paid by the customer by previous premiums).
#' 2. This allows the premiums to be calculated by first calculating the present
#' values for all of the benefit and costs cash flow vectors.
#' 3. The formulas
#' to calculate the gross, Zillmer and net premiums involve simple linear
#' combinations of these present values, so the coefficients of these formulas
#' is determined next.
#' 4. With the coefficients of the premium formulas calculated, all premiums
#' can be calculated (first the gross premium, because due to potential gross
#' premium refunds in case of death, the formula for the net premium requires
#' the gross premium, which the formula for the gross premium involves no other
#' type of premuim).
#' 5. With premiums determined, all unit cash flows and unit present values can
#' now be expressed in monetary terms (i.e. the actual Euro-amount that flows
#' rather than a percentage).
#' 6. As described above, the difference between the present values of premiums
#' and present values of benefits and costs is defined as the required amount
#' of reserves, so the reserves (net, gross, administration cost, balance sheet)
#' and all values derived from them (i.e. surrender value, sum insured in case of
#' premium waiver, etc.) are calculated.
#' 7. The decomposition of the premium into parts dedicated to specific purposes
#' (tax, rebates, net premium, gross premium, Zillmer premium, cost components,
#' risk premium, savings premium, etc.) can be done once the reserves are
#' ready (since e.g. the savings premium is defined as the difference of
#' discounted reserves at times $t$ and $t+1$).
#' 8. If the contract has (discretionary or obligatory) profit sharing mechanisms
#' included, the corresponding [ProfitParticipation] object can calculate that
#' profit sharing amounts, once all guaranteed values are calculated. This can
#' also be triggered manually (with custom profit sharing rates) by calling
#' the methods [InsuranceContract$profitScenario()] or [InsuranceContract$addProfitScenario()].
#'
#'
#'
#' @export
InsuranceContract = R6Class(
    "InsuranceContract",

    ######################### PUBLIC METHODS ##################################
    public = list(
        #' @field tarif
        #' The [InsuranceTarif] underlying this contract. The tarif is the abstract
        #' product description (i.e. defining the type of insurance, fixing tpyes
        #' of benefits, specifying costs, guaranteed interest rate, mortality tables,
        #' potential profit sharing mechanisms, etc.), while the contract holds
        #' the individual parts like age, sum insured, contract duration, premium
        #' payment frequency, etc.
        tarif = NULL,
        #' @field parent
        #' A pointer to the parent contract. Some contracts consist of multiple
        #' parts (e.g. a main savings contract with a dread-disease rider, or
        #' a contract with multiple dynamic increases). These are internally
        #' represented by one [InsuranceContract] object per contract part, plus
        #' one contract object combining them and deriving combined premiums,
        #' reserves and profit participation. The child contracts (i.e. the
        #' objects representing the individual parts) have a pointer to their
        #' parent, while the overall contract holds a list of all its child contract parts.
        parent = NULL,

        #' @field ContractParameters
        #' Insurance contract parameters explicitly specified in the contract
        #' (i.e. parameters that are NOT taken from the tariff of the defaults).
        ContractParameters = InsuranceContract.ParameterStructure, # Only values explicitly given for this contract, not including fallback values from the tariff
        #' @field Parameters
        #' Full set of insurance contract parameters applying to this contract.
        #' The set of parameters is a combination of explicitly given (contract-specific)
        #' values, parameters determined by the tariff and default values.
        Parameters = InsuranceContract.ParameterStructure,         # The whole parameter set, including values given by the tariff

        #' @field Values
        #' List of all contract values (cash flows, present values, premiums,
        #' reserves, premium decomposition, profit participation, etc.). These
        #' values will be calculated and filled when the contract is created
        #' and updated whenever the contract is changed.
        Values = InsuranceContract.Values,

        #' @field blocks
        #' For contracts with multiple contract parts: List of all tariff blocks
        #' (independently calculated [InsuranceContract] objects, that are combined
        #' to one contract, e.g. dynamic/sum increases). If this field is empty,
        #' this object describes a contract block (calculated as a stand-alone
        #' tariff), otherwise it will simply be the sum of its blocks (adjusted
        #' to span the same time periods)
        blocks = list(),

        #' @field history
        #' A list keeping track of all contract changes (including the whole
        #' contract state and its values before the change).
        history = list(),


        #### The code:

        initialize = function(tarif, parent = NULL, calculate = "all", profitid = "default", ...) {
            private$initParams = c(list(tarif = tarif, parent = parent, calculate = calculate, profitid = profitid), list(...))
            self$tarif = tarif;
            self$parent = parent;

            # TODO-block: If parent exists, use its parameters as fallback, too
            self$ContractParameters = InsuranceContract.ParametersFill(
                ...,
                premiumWaiver = FALSE,
                surrenderPenalty = TRUE,
                alphaRefunded = FALSE
            );
            # Set default values for required contract-specific data
            # First, take the tariff defaults, then the  ProfitParticipation
            # defaults, so a tariff can override the profit participation scheme
            self$Parameters = self$ContractParameters;
            self$Parameters = InsuranceContract.ParametersFallback(
                self$ContractParameters,
                self$tarif$getParameters()
            );

            ppScheme = self$Parameters$ProfitParticipation$profitParticipationScheme;
            if (!is.null(ppScheme)) {
                self$Parameters$ProfitParticipation = fallbackFields(
                    self$Parameters$ProfitParticipation,
                    ppScheme$Parameters);
                self$Parameters$ProfitParticipation$scenarios[[profitid]] = list()
            }

            private$consolidateContractData(tarif = tarif, ...);
            self$calculateContract(calculate = calculate);


            invisible(self)
        },

        addHistorySnapshot = function(time = 0, comment = "Initial contract values", type = "Contract", params = self$Parameters, values = self$Values, tarif = self$tarif, blocks = self$blocks) {
            self$history = rbind(
                self$history,
                list(
                    time = list(
                        "time"    = time,
                        "comment" = comment,
                        "type"    = type,
                        "params"  = params,
                        "values"  = values,
                        "tarif"   = tarif,
                        "blocks"  = blocks
                    )
                )
            );
            invisible(self)
        },

        addBlock = function(id = NULL, block = NULL, t = block$Values$int$blockStart, comment = comment, ...) {
            if (missing(block) || is.null(block) || !is(block, "InsuranceContract")) {
                # Create a block with the same tariff and parameters as the main contract, but allow overriding params with the ... arguments
                block = InsuranceContract$new(id = id, ...)
            }
            # declare as child of 'self', store the time offset to the parent contract
            block$parent = self
            block$Parameters$ContractData$blockStart = t

            if (length(self$blocks) == 0) {
                main = self$clone()
                main$parent = self
                self$blocks[["main"]] = main
                self$Parameters$ContractData$id = "Gesamt"
            }

            if (missing(id) || is.null(id)) {
                id = paste0("block", length(self$blocks) + 1)
            }
            self$blocks[[id]] = block
            # recalculate the whole contract by consolidating values from each block
            self$consolidateBlocks(valuesFrom = t)

            self$addHistorySnapshot(time = t, comment = comment,
                                    type = "Dynamics", params = self$Parameters, values = self$Values);

            invisible(self)
        },

        addDynamics = function(t, NewSumInsured, SumInsuredDelta, id, ...) {
            # TODO: Implement overriding contract parameters by ...

            # TODO: Override only the required parameters
            params = private$initParams
            if (is.null(params)) params = list()
            if (!is.null(params$age)) params$age = params$age + t
            if (!is.null(params$policyPeriod)) params$policyPeriod = params$policyPeriod - t
            if (!is.null(params$premiumPeriod)) params$premiumPeriod = max(1, params$premiumPeriod - t)
            if (!is.null(params$deferralPeriod)) params$deferralPeriod = max(0, params$deferralPeriod - t)
            if (!is.null(params$contractClosing)) params$contractClosing = params$contractClosing + years(t)

            # TODO: Generalize this to also allow specifying dynamic premium rather than sum insured
            if (!missing(SumInsuredDelta)) {
                SIdelta = SumInsuredDelta
            } else if (!missing(NewSumInsured)) {
                SIdelta = NewSumInsured - self$Values$reserves[t + 1, "SumInsured"]
            } else {
                warning("Neither NewSumInsured nor SumInsuredDelta are given. Unable to determine sum insured of the dynamic increase.")
                return(invisible(self))
            }
            params$sumInsured = SIdelta

            if (missing(id)) {
                # numbering dynamics: Use # of blocks (except main) as a simplification
                id = paste0("dyn", max(1, length(self$blocks)))
            }

            params$t = t
            params$id = id
            # Override with arguments explicitly given
            arguments = list(...)
            params[names(arguments)] = arguments[names(arguments)]
            params$comment = sprintf("Dynamic increase at time %d to sum %d", t, NewSumInsured)
            do.call(self$addBlock, params)
        },

        calculateContract = function(calculate = "all", valuesFrom = 0, premiumCalculationTime = 0, preservePastPV = TRUE, recalculatePremiums = TRUE, recalculatePremiumSum = TRUE, history_comment = NULL, history_type = "Contract") {
            if (!is.null(self$blocks)) {
                for (b in self$blocks) {
                    .args = as.list(match.call()[-1])
                    # correctly shift the valuesFrom by each block's blockStart parameter
                    .args$valuesFrom = max(0, .args$valuesFrom - b$Parameters$ContractData$blockStart)
                    do.call(b$calculateContract, .args)
                }
            }
            self$Values$int = private$determineInternalValues()
            self$Values$transitionProbabilities = mergeValues(
              starting = self$Values$transitionProbabilities,
              ending = private$determineTransitionProbabilities(),
              t = valuesFrom)
            if (calculate == "probabilities") return(invisible(self));

            self$Values$cashFlowsBasic = mergeValues(
                starting = self$Values$cashFlowsBasic,
                ending = private$determineCashFlowsBasic(),
                t = valuesFrom);
            self$Values$cashFlows = mergeValues(
                starting = self$Values$cashFlows,
                ending = private$determineCashFlows(),
                t = valuesFrom);

            if (recalculatePremiumSum) {
                # Premium waiver: Premium sum is not affected by premium waivers, i.e. everything depending on the premium sum uses the original premium sum!
                self$Values$unitPremiumSum = private$determinePremiumSum();
            }
            self$Values$cashFlowsCosts = mergeValues3D(
                starting = self$Values$cashFlowsCosts,
                ending = private$determineCashFlowsCosts(),
                t = valuesFrom);
            if (calculate == "cashflows") return(invisible(self));


            # Shall we re-calculate PV or preserve the old ones???
            pv = private$calculatePresentValues()
            pvCost = private$calculatePresentValuesCosts()
            oldPV = self$Values$presentValues
            if (preservePastPV) {
                # Preserve past present values, i.e. the PV represents the PV
                # with the knowledge of the past, even though the future CF
                # might have changed meanwhile, so the PV at time 0 is no
                # longer the PV of the current cash flows... The PV at time t
                # always represents the information available at time t, but no
                # future chagnes.
                # This is useful to preserver the PV information neede to
                # calculate the premiums from the past.
                if (!is.null(self$Values$presentValues)) {
                    self$Values$presentValues = self$Values$presentValues[,1:NCOL(pv)]
                }
                self$Values$presentValues = mergeValues(starting = self$Values$presentValues, ending = pv, t = valuesFrom)
                self$Values$presentValuesCosts = mergeValues3D(starting = self$Values$presentValuesCosts, ending = pvCost, t = valuesFrom)
            } else {
                # Recalculate present value for times before start, i.e. make all PV consistent with the current cash flows
                self$Values$presentValues = pv
                self$Values$presentValuesCosts = pvCost
            }
            if (calculate == "presentvalues") return(invisible(self));

            # the premiumCalculation function returns the premiums AND the cofficients,
            # so we have to extract the coefficients and store them in a separate variable
            if (recalculatePremiums) {
                res = private$calculatePremiums(premiumCalculationTime = premiumCalculationTime);
                self$Values$premiumCoefficients = res[["coefficients"]];
                # TODO: Store premiums in a data.frame, including the time they are calculated???
                self$Values$premiums = res[["premiums"]]
                self$Values$int$premiumCalculationTime = premiumCalculationTime
            }
            if (calculate == "premiums") return(invisible(self));

            # Update the cash flows and present values with the values of the premium
            pvAllBenefits = private$calculatePresentValuesBenefits()
            if (preservePastPV) {
                self$Values$presentValues = mergeValues(starting = oldPV, ending = cbind(pv, pvAllBenefits), t = valuesFrom)
            } else {
                self$Values$presentValues = cbind(pv, pvAllBenefits)
            }

            self$Values$absCashFlows       = mergeValues(starting = self$Values$absCashFlows,       ending = private$calculateAbsCashFlows(), t = valuesFrom);
            self$Values$absPresentValues   = mergeValues(starting = self$Values$absPresentValues,   ending = private$calculateAbsPresentValues(), t = valuesFrom);
            if (calculate == "absvalues") return(invisible(self));

            self$Values$reserves           = mergeValues(starting = self$Values$reserves,           ending = private$calculateReserves(), t = valuesFrom);
            self$Values$reservesBalanceSheet = mergeValues(starting = self$Values$reservesBalanceSheet,ending = private$calculateReservesBalanceSheet(), t = valuesFrom);
            if (calculate == "reserves") return(invisible(self));
            self$Values$premiumComposition = mergeValues(starting = self$Values$premiumComposition, ending = private$premiumAnalysis(), t = valuesFrom);
            self$Values$premiumCompositionSums = mergeValues(starting = self$Values$premiumCompositionSums, ending = private$premiumCompositionSums(), t = valuesFrom);
            self$Values$premiumCompositionPV = mergeValues(starting = self$Values$premiumCompositionPV, ending = private$premiumCompositionPV(), t = valuesFrom);
            self$Values$basicData          = mergeValues(starting = self$Values$basicData,          ending = private$getBasicDataTimeseries(), t = valuesFrom);
            if (calculate == "premiumcomposition") return(invisible(self));

            private$profitParticipation(calculateFrom = valuesFrom);
            if (calculate == "profitparticipation") return(invisible(self));

            self$addHistorySnapshot(
                time    = valuesFrom,
                comment = ifelse(is.null(history_comment),
                                 ifelse(valuesFrom == 0, "Initial contract values", paste("Contract recalculation at time", premiumCalculationTime)),
                                 history_comment),
                type    = history_type,
                params  = self$Parameters,
                values  = self$Values
            );
            if (calculate == "history") return(invisible(self));

            invisible(self)
        },
        consolidateBlocks = function(valuesFrom = 0) {
            # First, Re-calculate all children that have children on their own
            for (b in self$blocks) {
                if (length(b$blocks) > 0) {
                    # correctly shift the valuesFrom by each block's blockStart parameter
                    b$consolidateBlocks(valuesFrom = max(0, valuesFrom - b$Parameters$ContractData$blockStart))
                }
            }

            # Helper functions to prepend/append rows to the arrays and sum them up
            padArray = function(arr = NULL, pad = 0, len = 0) {
                padEnd = max(0, len - pad - NROW(arr)) # if len is too short, return an array containing at least the arr
                nrcols = ifelse(is.null(arr), 0, NCOL(arr))
                rbind(
                    array(0, dim = c(pad, nrcols)) %>% `colnames<-`(colnames(arr)),
                    arr,
                    array(0, dim = c(padEnd, nrcols)) %>% `colnames<-`(colnames(arr))
                ) %>% `colnames<-`(colnames(arr))
            }
            sumPaddedArrays = function(arr1 = NULL, arr2 = NULL, pad1 = 0, pad2 = 0) {
                newlen = max(pad1 + NROW(arr1), pad2 + NROW(arr2))
                if (is.null(arr2)) {
                    padArray(arr1, pad = pad1, len = newlen)
                } else if (is.null(arr1)) {
                    padArray(arr2, pad = pad2, len = newlen)
                } else {
                    # First prepend trailing zero rows according to pad1/pad2:
                    arr1 = padArray(arr1, pad = pad1, len = newlen)
                    arr2 = padArray(arr2, pad = pad2, len = newlen)

                    # arr1 and arr2 now should have the same dimensions => sum them up
                    arr1 + arr2
                }
            }
            consolidateField = function(field) {
                vals = NULL
                if (length(self$blocks) == 0) {
                    vals = self$Values[[field]]
                }
                for (b in self$blocks) {
                    vals = sumPaddedArrays(arr1 = vals, arr2 = b$Values[[field]], pad2 = b$Parameters$ContractData$blockStart)
                }
                mergeValues(starting = self$Values[[field]],   ending = vals, t = valuesFrom);
            }


            # Some values do not make sense for consolidated values
            self$Values$cashFlowsBasic = NULL
            self$Values$cashFlows = NULL
            self$Values$cashFlowsCosts = NULL
            self$Values$presentValues = NULL
            self$Values$presentValuesCosts = NULL

            self$Values$unitPremiumSum = NULL
            self$Values$premiumCoefficients = NULL
            self$Values$premiums = NULL

            # self$Values$transitionProbabilities = consolidateField("transitionProbabilities")
            # self$Values$cashFlowsBasic       = consolidateField("cashFlowsBasic")
            # self$Values$cashFlows            = consolidateField("cashFlows")
            # self$Values$cashFlowsCosts       = consolidateField("cashFlowsCosts")
            self$Values$absCashFlows           = consolidateField("absCashFlows")
            self$Values$absPresentValues       = consolidateField("absPresentValues")
            self$Values$premiumComposition     = consolidateField("premiumComposition")
            self$Values$premiumCompositionSums = consolidateField("premiumCompositionSums")
            self$Values$premiumCompositionPV   = consolidateField("premiumCompositionPV")
            self$Values$reserves               = consolidateField("reserves")
            self$Values$reservesBalanceSheet   = consolidateField("reservesBalanceSheet")
            # TODO: Basic Data cannot simply be summed, e.g. the interest rate!
            self$Values$basicData              = consolidateField("basicData")
            # self$Values$basicData[,c("InterestRate", "PolicyDuration", "PremiumPeriod")] = NULL

            invisible(self)
        },

        # Premium Waiver: Stop all premium payments at time t
        # the SumInsured is determined from the available
        premiumWaiver = function(t) {
            newSumInsured = self$Values$reserves[[toString(t), "PremiumFreeSumInsured"]];
            self$Parameters$ContractState$premiumWaiver = TRUE;
            self$Parameters$ContractState$surrenderPenalty = FALSE; # Surrender penalty has already been applied, don't apply a second time
            self$Parameters$ContractState$alphaRefunded = TRUE;     # Alpha cost (if applicable) have already been refunded partially, don't refund again

            self$Parameters$ContractData$sumInsured = newSumInsured;

            self$calculateContract(
                valuesFrom = t,
                premiumCalculationTime = t,
                preservePastPV = TRUE, recalculatePremiums = TRUE, recalculatePremiumSum = FALSE,
                history_comment = sprintf("Premium waiver at time %d", t), history_type = "PremiumWaiver")

            invisible(self)
        },



        # Premium Waiver: Stop all premium payments at time t
        # the SumInsured is determined from the available
        premiumWaiverOLD = function(t) {
            newSumInsured = self$Values$reserves[[toString(t), "PremiumFreeSumInsured"]];
            self$Parameters$ContractState$premiumWaiver = TRUE;
            self$Parameters$ContractState$surrenderPenalty = FALSE; # Surrender penalty has already been applied, don't apply a second time
            self$Parameters$ContractState$alphaRefunded = TRUE;     # Alpha cost (if applicable) have already been refunded partially, don't refund again

            self$Parameters$ContractData$sumInsured = newSumInsured;

            self$Values$cashFlowsBasic = mergeValues(starting = self$Values$cashFlowsBasic, ending = private$determineCashFlowsBasic(), t = t);
            self$Values$cashFlows = mergeValues(starting = self$Values$cashFlows, ending = private$determineCashFlows(), t = t);
            # Premium sum is not affected by premium waivers, i.e. everything depending on the premium sum uses the original premium sum!
            # self$Values$premiumSum = private$determinePremiumSum();
            self$Values$cashFlowsCosts = mergeValues3D(starting = self$Values$cashFlowsCosts, ending = private$determineCashFlowsCosts(), t = t);

            pv = private$calculatePresentValues();
            pvc = private$calculatePresentValuesCosts();
            self$Values$presentValuesCosts = mergeValues3D(starting = self$Values$presentValuesCosts, ending = pvc, t = t);

            # TODO:
            # the premiumCalculation function returns the premiums AND the cofficients,
            # so we have to extract the coefficients and store them in a separate variable
            # res = private$calculatePremiums(t);
            # self$Values$premiumCoefficients = mergeValues(starting = self$Values$premiumCoefficients, ending=res[["coefficients"]], t = t);
            # self$Values$premiums = mergeValues(starting= = res[["premiums"]]

            # Update the cash flows and present values with the values of the premium
            pvAllBenefits = private$calculatePresentValuesBenefits()
            self$Values$presentValues = mergeValues(starting = self$Values$presentValues, ending = cbind(pv, pvAllBenefits), t = t);

            self$Values$absCashFlows       = mergeValues(starting = self$Values$absCashFlows,       ending = private$calculateAbsCashFlows(), t = t);
            self$Values$absPresentValues   = mergeValues(starting = self$Values$absPresentValues,   ending = private$calculateAbsPresentValues(), t = t);
            self$Values$reserves           = mergeValues(starting = self$Values$reserves,           ending = private$calculateReserves(), t = t);
            self$Values$basicData          = mergeValues(starting = self$Values$basicData,          ending = private$getBasicDataTimeseries(), t = t);
            self$Values$premiumComposition = mergeValues(starting = self$Values$premiumComposition, ending = private$premiumAnalysis(), t = t);

            self$addHistorySnapshot(time = t, comment = sprintf("Premium waiver at time %d", t),
                                    type = "PremiumWaiver", params = self$Parameters, values = self$Values);

            invisible(self)
        },

        # Calculate one profit scenario and return all values
        profitScenario = function(...) {
            private$calculateProfitParticipation(...)
        },

        # Calculate one profit scenario and store it in the contract (e.g. to be exported to Excel), this function can be chained!
        addProfitScenario = function(id, ...) {
            .args = as.list(match.call()[-1])
            self$Parameters$ProfitParticipation$scenarios[[id]] = list(...)
            if (length(self$blocks) > 0) {
                vals = NULL
                for (b in self$blocks) {
                    # TODO: shift the profit rates by b$Parameters$ContractData$blockStart
                    do.call(b$addProfitScenario, .args)
                    vals = sumPaddedArrays(arr1 = vals, arr2 = b$Values$profitScenarios[[id]], pad2 = b$Parameters$ContractData$blockStart)
                    # TODO: consolidate reserves after profit!
                }
                # Consolidate all child blocks
                self$Values$profitScenarios[[id]] = vals
            } else {
                # profitParticipation will assign the values to Values$profitScenarios[[id]] and Values$reservesAfterProfit[[id]]
                # private$profitParticipation(id = id, ...)
                pp = private$calculateProfitParticipation(...)
                self$Values$profitScenarios[[id]] = pp
                self$Values$reservesAfterProfit[[id]] = private$calculateReservesAfterProfit(profitScenario = pp, ...)
            }

            invisible(self)
        },


        #' @field dummy.public
        #' dummy field to allow a trailing comma after the previous field/method
        dummy.public = NULL
    ),

    ######################### PRIVATE METHODS ##################################
    private = list(
        initParams = NULL,

        consolidateContractData = function(...) {
            args = list(...);
            # TODO-blocks

            # Calculate YOB, age, contract closing etc. from each other
            # 1. Contract date (if not given) is NOW, unless age + YOB is given => Then year is derived as YOB+age
            if (is.null(self$Parameters$ContractData$contractClosing)) {
                if (!is.null(self$Parameters$ContractData$age) && !is.null(self$Parameters$ContractData$YOB)) {
                    # Use current day, but determine year from YOB and age
                    self$Parameters$ContractData$contractClosing = Sys.Date() %>%
                        'year<-'(self$Parameters$ContractData$YOB + self$Parameters$ContractData$age);
                }
            }

            # 2. Current age: If YOB is given, calculate from contract closing and YOB, otherwise assume 40
            if (is.null(self$Parameters$ContractData$age)) {
                if (is.null(self$Parameters$ContractData$YOB)) {
                    self$Parameters$ContractData$age = 40; # No information to derive age => Assume 40
                    warning("InsuranceContract: Missing age, no information to derive age from YOB and contractClosing => Assuming default age 40. Tariff: ", self$tarif$name)
                } else {
                    self$Parameters$ContractData$age = year(self$Parameters$ContractData$contractClosing) -
                        self$Parameters$ContractData$YOB;
                }
            }
            if (is.null(self$Parameters$ContractData$YOB)) {
                self$Parameters$ContractData$YOB = year(self$Parameters$ContractData$contractClosing) - self$Parameters$ContractData$age;
            }

            # Evaluate policy period, i.e. if a function is used, calculate its numeric value
            self$Parameters$ContractData$policyPeriod = valueOrFunction(
                self$Parameters$ContractData$policyPeriod,
                params = self$Parameters, values = self$Values);

            #### #
            # PREMIUM PAYMENT PERIOD (default: policyPeriod, can be given as function or numeric value)
            #### #
            if (is.null(self$Parameters$ContractData$premiumPeriod)) {
                self$Parameters$ContractData$premiumPeriod = self$Parameters$ContractData$policyPeriod
            }
            self$Parameters$ContractData$premiumPeriod = valueOrFunction(
                self$Parameters$ContractData$premiumPeriod,
                params = self$Parameters, values = self$Values);
            # At least 1 year premium period!
            self$Parameters$ContractData$premiumPeriod = max(self$Parameters$ContractData$premiumPeriod, 1);

            # Evaluate deferral period, i.e. if a function is used, calculate its numeric value from the other parameters
            self$Parameters$ContractData$deferralPeriod = valueOrFunction(
                self$Parameters$ContractData$deferralPeriod,
                params = self$Parameters, values = self$Values);

            #### #
            # COSTS PARAMETERS: can be a function => evaluate it to get the real costs
            #### #
            self$Parameters$Costs = private$evaluateCosts(self$Parameters$Costs)

            #### #
            # AGES for multiple joint lives:
            #### #
            # For joint lives, some parameters can be given multiple times: age, sex
            # Collect all given values into one vector!
            age = unlist(args[names(args) == "age"], use.names = FALSE)
            if (!is.null(age)) {
                self$Parameters$ContractData$age = age;
            }
            sex = unlist(args[names(args) == "sex"], use.names = FALSE)
            if (!is.null(sex)) {
                self$Parameters$ContractData$sex = sex;
            }
            if (is.null(self$Parameters$ContractData$ageDifferences)) {
                self$Parameters$ContractData$ageDifferences = diff(self$Parameters$ContractData$age);
            } else {
                self$Parameters$ContractData$ageDifferences = valueOrFunction(
                    self$Parameters$ContractData$ageDifferences,
                    params = self$Parameters, values = self$Values);
            }


            #### #
            # TECHNICAL AGE
            #### #
            # Calculate the technical age (e.g. female are made younger, contracts on joint lives, etc.)
            if (is.null(self$Parameters$ContractData$technicalAge)) {
                self$Parameters$ContractData$technicalAge = self$Parameters$ContractData$age[1]
            } else {
                self$Parameters$ContractData$technicalAge = valueOrFunction(
                    self$Parameters$ContractData$technicalAge,
                    params = self$Parameters, values = self$Values);
            }

            # Evaluate all possibly variable values (mortalityTable depending on sex, etc.)
            self$Parameters$ActuarialBases$mortalityTable = valueOrFunction(
                self$Parameters$ActuarialBases$mortalityTable,
                params = self$Parameters, values = self$Values)

            invisible(self)
        },

        evaluateCosts = function(costs) {
            self$tarif$getCostValues(costs, params = self$Parameters)
        },

        determineInternalValues = function(...) {
            self$tarif$getInternalValues(params = self$Parameters, ...);
        },

        determineTransitionProbabilities = function(...) {
            self$tarif$getTransitionProbabilities(params = self$Parameters, values = self$Values, ...);
        },
        determineCashFlowsBasic = function(...) {
            self$tarif$getBasicCashFlows(params = self$Parameters, values = self$Values, ...);
        },
        determineCashFlows = function(...) {
            self$tarif$getCashFlows(params = self$Parameters, values = self$Values, ...);
        },
        determinePremiumSum = function(...) {
            sum(self$Values$cashFlows$premiums_advance + self$Values$cashFlows$premiums_arrears);
        },
        determineCashFlowsCosts = function(...) {
            self$tarif$getCashFlowsCosts(params = self$Parameters, values = self$Values, ...);
        },
        calculatePresentValues = function(...) {
            self$tarif$presentValueCashFlows(params = self$Parameters, values = self$Values, ...);
        },
        calculatePresentValuesCosts = function(...) {
            self$tarif$presentValueCashFlowsCosts(params = self$Parameters, values = self$Values, ...);
        },
        calculatePremiums = function(...) {
            self$tarif$premiumCalculation(params = self$Parameters, values = self$Values, ...);
        },
        calculatePresentValuesBenefits = function(...) {
            self$tarif$presentValueBenefits(params = self$Parameters, values = self$Values, ...);
        },
        calculateAbsCashFlows = function(...) {
            self$tarif$getAbsCashFlows(params = self$Parameters, values = self$Values, ...);
        },
        calculateAbsPresentValues = function(...) {
            self$tarif$getAbsPresentValues(params = self$Parameters, values = self$Values, ...);
        },
        calculateReserves = function(...) {
            self$tarif$reserveCalculation(params = self$Parameters, values = self$Values, ...);
        },
        calculateReservesBalanceSheet = function(...) {
            self$tarif$reserveCalculationBalanceSheet(params = self$Parameters, values = self$Values, ...);
        },
        premiumAnalysis = function(...) {
            self$tarif$premiumDecomposition(params = self$Parameters, values = self$Values, ...);
        },
        premiumCompositionSums = function(...) {
            self$tarif$calculateFutureSums(self$Values$premiumComposition, ...);
        },
        premiumCompositionPV = function(...) {
            self$tarif$calculatePresentValues(self$Values$premiumComposition, params = self$Parameters, ...);
        },

        profitParticipation = function(...) {
            scens = self$Parameters$ProfitParticipation$scenarios
            lapply(seq_along(scens), function(x) {
                nm = names(scens)[x]
                scn = NULL
                if (!is.null(self$Values$profitScenarios) && length(self$Values$profitScenarios) >= x) {
                    scn = self$Values$profitScenarios[[x]]
                }
                pp = do.call(private$calculateProfitParticipation, c(list(profitScenario = scn, ...), scens[x]))
                res = do.call(private$calculateReservesAfterProfit, c(list(profitScenario = pp, ...), scens[x]))
                if (nm != "" && !is.null(nm)) {
                    self$Values$profitScenarios[[nm]] = pp
                    self$Values$reservesAfterProfit[[nm]] = res
                } else {
                    self$Values$profitScenarios[[x]] = pp
                    self$Values$reservesAfterProfit[[x]] = res
                }
            })

            # For convenience, return the profit participation table:
            # (self$Values$reservesAfterProfit was also changed, but is not returned!)
            self$Values$profitScenarios
        },

        calculateProfitParticipation = function(...) {
            self$tarif$calculateProfitParticipation(params = self$Parameters, values = self$Values, ...);
        },
        calculateReservesAfterProfit = function(profitScenario, ...) {
            self$tarif$reservesAfterProfit(profitScenario = profitScenario, params = self$Parameters, values = self$Values, ...);
        },


        getBasicDataTimeseries = function(...) {
            self$tarif$getBasicDataTimeseries(params = self$Parameters, values = self$Values, ...);
        },

        dummy.private = NULL
    )
)

