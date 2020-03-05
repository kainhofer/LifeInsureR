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
#' Immediately upon construction, all premiums, reserves and cash flows for the
#' whole contract period are calculated.
#'
#' @export
InsuranceContract = R6Class(
    "InsuranceContract",

    ######################### PUBLIC METHODS ##################################
    public = list(
        tarif = NULL,
        parent = NULL,

        ContractParameters = InsuranceContract.ParameterStructure, # Only values explicitly given for this contract, not including fallback values from the tariff
        Parameters = InsuranceContract.ParameterStructure,         # The whole parameter set, including values given by the tariff

        #### Caching values for this contract, initialized/calculated when the object is created
        Values = InsuranceContract.Values,

        #### List of all tariff blocks (independently calculated, but combined to one contract, e.g. dynamic/sum increases)
        # If blocks is empty, this object describes a contract block (calculated as a stand-alone tariff), otherwise it will
        # simply be the sum of its blocks (adjusted to span the same time periods)
        blocks = list(),

        #### Keeping the history of all contract changes during its lifetime
        history = list(),


        #### The code:

        initialize = function(tarif, parent = NULL, calculate = "all", ...) {
            private$initParams = c(list(tarif = tarif, parent = parent, calculate = calculate), list(...))
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
            }

            private$consolidateContractData(tarif = tarif, ...);
            self$calculateContract(calculate = calculate, start = self$Parameters$ContractData$blockStart);

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

        addBlock = function(blockID = NULL, block = NULL, t = 0, comment = comment, ...) {
            if (missing(block) || is.null(block) || !is(block, "InsuranceContract")) {
                # Create a block with the same tariff and parameters as the main contract, but allow overriding params with the ... arguments
                block = InsuranceContract$new(...)
            }
            block$parent = self
            block$Values$int$startsAt = t

            if (length(self$blocks) == 0) {
                main = self$clone()
                main$parent = self
                self$blocks[["main"]] = main
                self$Parameters$ContractData$id = "Gesamt"
            }

            if (missing(blockID) || is.null(blockID)) {
                blockID = paste0("block", length(self$blocks) + 1)
            }
            self$blocks[[blockID]] = block
            # recalculate the whole contract by consolidating values from each block
            self$consolidateBlocks(start = t)

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
            params$blockID = id
            params$id = id
            params$comment = sprintf("Dynamic increase at time %d to sum %d", t, NewSumInsured)
            do.call(self$addBlock, params)
        },

        calculateContract = function(calculate = "all", start = 0, preservePastPV = TRUE, recalculatePremiums = TRUE, recalculatePremiumSum = TRUE, history_comment = NULL, history_type = "Contract") {
            if (!is.null(self$blocks)) {
                .args = as.list(match.call()[-1])
                for (b in self$blocks) {
                    do.call(b$calculateContract, .args)
                }
            }
            self$Values$int = private$determineInternalValues(start = start)
            self$Values$transitionProbabilities = mergeValues(
              starting = self$Values$transitionProbabilities,
              ending = private$determineTransitionProbabilities(start = start),
              t = start)
            if (calculate == "probabilities") return(invisible(self));

            self$Values$cashFlowsBasic = mergeValues(
                starting = self$Values$cashFlowsBasic,
                ending = private$determineCashFlowsBasic(start = start),
                t = start);
            self$Values$cashFlows = mergeValues(
                starting = self$Values$cashFlows,
                ending = private$determineCashFlows(start = start),
                t = start);

            if (recalculatePremiumSum) {
                # Premium waiver: Premium sum is not affected by premium waivers, i.e. everything depending on the premium sum uses the original premium sum!
                self$Values$unitPremiumSum = private$determinePremiumSum(start = start);
            }
            self$Values$cashFlowsCosts = mergeValues3D(
                starting = self$Values$cashFlowsCosts,
                ending = private$determineCashFlowsCosts(start = start),
                t = start);
            if (calculate == "cashflows") return(invisible(self));


            # Shall we re-calculate PV or preserve the old ones???
            pv = private$calculatePresentValues(start = start)
            pvCost = private$calculatePresentValuesCosts(start = start)
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
                self$Values$presentValues = mergeValues(starting = self$Values$presentValues, ending = pv, t = start)
                self$Values$presentValuesCosts = mergeValues3D(starting = self$Values$presentValuesCosts, ending = pvCost, t = start)
            } else {
                # Recalculate present value for times before start, i.e. make all PV consistent with the current cash flows
                self$Values$presentValues = pv
                self$Values$presentValuesCosts = pvCost
            }
            if (calculate == "presentvalues") return(invisible(self));

            # the premiumCalculation function returns the premiums AND the cofficients,
            # so we have to extract the coefficients and store them in a separate variable
            res = private$calculatePremiums(start = start);
            self$Values$premiumCoefficients = res[["coefficients"]];
            # TODO: Store premiums in a data.frame???
            self$Values$premiums = res[["premiums"]]
            self$Values$int$premiumCalculationTime = start
            if (calculate == "premiums") return(invisible(self));

            # Update the cash flows and present values with the values of the premium
            pvAllBenefits = private$calculatePresentValuesBenefits(start = start)
            if (preservePastPV) {
                self$Values$presentValues = mergeValues(starting = oldPV, ending = cbind(pv, pvAllBenefits), t = start)
            } else {
                self$Values$presentValues = cbind(pv, pvAllBenefits)
            }

            self$Values$absCashFlows       = mergeValues(starting = self$Values$absCashFlows,       ending = private$calculateAbsCashFlows(start = start), t = start);
            self$Values$absPresentValues   = mergeValues(starting = self$Values$absPresentValues,   ending = private$calculateAbsPresentValues(start = start), t = start);
            if (calculate == "absvalues") return(invisible(self));

            self$Values$reserves           = mergeValues(starting = self$Values$reserves,           ending = private$calculateReserves(start = start), t = start);
            self$Values$reservesBalanceSheet = mergeValues(starting = self$Values$reservesBalanceSheet,ending = private$calculateReservesBalanceSheet(start = start), t = start);
            if (calculate == "reserves") return(invisible(self));
            self$Values$premiumComposition = mergeValues(starting = self$Values$premiumComposition, ending = private$premiumAnalysis(start = start), t = start);
            self$Values$premiumCompositionSums = mergeValues(starting = self$Values$premiumCompositionSums, ending = private$premiumCompositionSums(start = start), t = start);
            self$Values$premiumCompositionPV = mergeValues(starting = self$Values$premiumCompositionPV, ending = private$premiumCompositionPV(start = start), t = start);
            self$Values$basicData          = mergeValues(starting = self$Values$basicData,          ending = private$getBasicDataTimeseries(start = start), t = start);
            if (calculate == "premiumcomposition") return(invisible(self));

            private$profitParticipation(start = start); # TODO-start
            if (calculate == "profitparticipation") return(invisible(self));

            self$addHistorySnapshot(
                time    = start,
                comment = ifelse(is.null(history_comment),
                                 ifelse(start == 0, "Initial contract values", paste("Contract recalculation at time ", start)),
                                 history_comment),,
                type    = history_type,
                params  = self$Parameters,
                values  = self$Values
            );
            if (calculate == "history") return(invisible(self));

            invisible(self)
        },
        consolidateBlocks = function(start = 0) {
            # First, Re-calculate all children that have children on their own
            for (b in self$blocks) {
                if (length(b$blocks) > 0) {
                    b$consolidateBlocks(start = start)
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
                    vals = padArray(self$Values[[field]], pad = self$Values$int$startsAt)
                }
                for (b in self$blocks) {
                    vals = sumPaddedArrays(arr1 = vals, arr2 = b$Values[[field]], pad2 = b$Values$int$startsAt)
                }
                mergeValues(starting = self$Values[[field]],   ending = vals, t = start);
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
        premiumWaiverNew = function(t) {
            newSumInsured = self$Values$reserves[[toString(t), "PremiumFreeSumInsured"]];
            self$Parameters$ContractState$premiumWaiver = TRUE;
            self$Parameters$ContractState$surrenderPenalty = FALSE; # Surrender penalty has already been applied, don't apply a second time
            self$Parameters$ContractState$alphaRefunded = TRUE;     # Alpha cost (if applicable) have already been refunded partially, don't refund again

            self$Parameters$ContractData$sumInsured = newSumInsured;

            self$calculateContract(
                start = t,
                preservePastPV = TRUE, recalculatePremiums = TRUE, recalculatePremiumSum = FALSE,
                history_comment = sprintf("Premium waiver at time %d", t), history_type = "PremiumWaiver")

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

        determineInternalValues = function(start = 0) {
            self$tarif$getInternalValues(params = self$Parameters, start = start);
        },

        determineTransitionProbabilities = function(start = 0) {
            self$tarif$getTransitionProbabilities(params = self$Parameters, values = self$Values, start = start);
        },
        determineCashFlowsBasic = function(start = 0) {
            self$tarif$getBasicCashFlows(params = self$Parameters, values = self$Values, start = start);
        },
        determineCashFlows = function(start = 0) {
            self$tarif$getCashFlows(params = self$Parameters, values = self$Values, start = start);
        },
        determinePremiumSum = function(start = 0) {
            sum(self$Values$cashFlows$premiums_advance + self$Values$cashFlows$premiums_arrears);
        },
        determineCashFlowsCosts = function(start = 0) {
            self$tarif$getCashFlowsCosts(params = self$Parameters, values = self$Values, start = start);
        },
        calculatePresentValues = function(start = 0) {
            self$tarif$presentValueCashFlows(params = self$Parameters, values = self$Values, start = start);
        },
        calculatePresentValuesCosts = function(start = 0) {
            self$tarif$presentValueCashFlowsCosts(params = self$Parameters, values = self$Values, start = start);
        },
        calculatePremiums = function(start = 0) {
            self$tarif$premiumCalculation(params = self$Parameters, values = self$Values, start = start);
        },
        calculatePresentValuesBenefits = function(start = 0) {
            self$tarif$presentValueBenefits(params = self$Parameters, values = self$Values, start = start);
        },
        calculateAbsCashFlows = function(start = 0) {
            self$tarif$getAbsCashFlows(params = self$Parameters, values = self$Values, start = start);
        },
        calculateAbsPresentValues = function(start = 0) {
            self$tarif$getAbsPresentValues(params = self$Parameters, values = self$Values, start = start);
        },
        calculateReserves = function(start = 0) {
            self$tarif$reserveCalculation(params = self$Parameters, values = self$Values, start = start);
        },
        calculateReservesBalanceSheet = function(start = 0) {
            self$tarif$reserveCalculationBalanceSheet(params = self$Parameters, values = self$Values, start = start);
        },
        premiumAnalysis = function(start = 0) {
            self$tarif$premiumDecomposition(params = self$Parameters, values = self$Values, start = start);
        },
        premiumCompositionSums = function(start = 0) {
            self$tarif$calculateFutureSums(self$Values$premiumComposition, start = start);
        },
        premiumCompositionPV = function(start = 0) {
            self$tarif$calculatePresentValues(self$Values$premiumComposition, params = self$Parameters, start = start);
        },

        profitParticipation = function(...) {
            self$Values$profitParticipation = private$calculateProfitParticipation(...);
            self$Values$reservesInclProfit = private$calculateReservesWithProfit(...);

            # For convenience, return the profit participation table:
            self$Values$profitParticipation
        },

        calculateProfitParticipation = function(...) {
            self$tarif$calculateProfitParticipation(params = self$Parameters, values = self$Values, ...);
        },
        calculateReservesWithProfit = function(...) {
            self$tarif$reservesWithProfit(params = self$Parameters, values = self$Values, ...);
        },


        getBasicDataTimeseries = function(start = 0) {
            self$tarif$getBasicDataTimeseries(params = self$Parameters, values = self$Values, start = start);
        },

        dummy.private = NULL
    )
)
# InsuranceContract$debug("premiumWaiver")
