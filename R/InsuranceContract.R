#' @include HelperFunctions.R InsuranceParameters.R InsuranceTarif.R ProfitParticipation.R
#'
#' @import MortalityTables
#' @import R6
NULL


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

    public = list(
        tarif = NULL,

        ContractParameters = InsuranceContract.ParameterStructure, # Only values explicitly given for this contract, not including fallback values from the tariff
        Parameters = InsuranceContract.ParameterStructure,         # The whole parameter set, including values given by the tariff

        #### Caching values for this contract, initialized/calculated when the object is created
        Values = InsuranceContract.Values,

        #### Keeping the history of all contract changes during its lifetime
        history = list(),


        #### The code:

        initialize = function(tarif, ...) {
            self$tarif = tarif;

            self$ContractParameters = InsuranceContract.ParametersFill(
                ...,
                premiumWaiver = FALSE,
                surrenderPenalty = TRUE,
                alphaRefunded = FALSE
            );

            # Set default values for required contract-specific data
            # First, take the tariff defaults, then the  ProfitParticipation
            # defaults, so a tariff can override the profit participation scheme
            # values. Also, the tariff will fill the profit scheme default,
            # so we need to call the tariff first in any case
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

            self$consolidateContractData(tarif = tarif, ...);

            self$calculateContract();
        },

        addHistorySnapshot = function(time = 0, comment = "Initial contract values", type = "Contract", params = self$Parameters, values = self$Values) {
            self$history = rbind(
                self$history,
                list(
                    time=list(
                        "time"    = time,
                        "comment" = comment,
                        "type"    = type,
                        "params"  = params,
                        "values"  = values
                    )
                )
            );
        },

        consolidateContractData = function(...) {
            args = list(...);

            #####
            # PREMIUM PAYMENT PERIOD (default: policyPeriod, can be given as function or numeric value)
            #####
            if (is.null(self$Parameters$ContractData$premiumPeriod)) {
                self$Parameters$ContractData$premiumPeriod = self$Parameters$ContractData$policyPeriod
            } else {
                self$Parameters$ContractData$premiumPeriod = valueOrFunction(
                    self$Parameters$ContractData$premiumPeriod,
                    params = self$Parameters, values = self$Values);
            }

            #####
            # COSTS PARAMTERS: can be a function => evaluate it to get the real costs
            #####
            self$Parameters$Costs = self$evaluateCosts(self$Parameters$Costs)

            #####
            # AGES for multiple joint lives:
            #####
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

            #####
            # TECHNICAL AGE
            #####
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
        },

        evaluateCosts = function(costs) {
            self$tarif$getCostValues(costs, params = self$Parameters);
        },


        calculateContract = function() {
            self$Values$transitionProbabilities = self$determineTransitionProbabilities();

            self$Values$cashFlowsBasic = self$determineCashFlowsBasic();
            self$Values$cashFlows = self$determineCashFlows();
            self$Values$unitPremiumSum = self$determinePremiumSum();
            self$Values$cashFlowsCosts = self$determineCashFlowsCosts();

            self$Values$presentValues = self$calculatePresentValues();
            self$Values$presentValuesCosts = self$calculatePresentValuesCosts();

            # the premiumCalculation function returns the premiums AND the cofficients,
            # so we have to extract the coefficients and store them in a separate variable
            res = self$calculatePremiums();
            self$Values$premiumCoefficients = res[["coefficients"]];
            self$Values$premiums = res[["premiums"]]

            # Update the cash flows and present values with the values of the premium
            pvAllBenefits = self$calculatePresentValuesBenefits()
            self$Values$presentValues = cbind(self$Values$presentValues, pvAllBenefits)

            self$Values$absCashFlows = self$calculateAbsCashFlows();
            self$Values$absPresentValues = self$calculateAbsPresentValues();
            self$Values$reserves = self$calculateReserves();
            self$Values$reservesBalanceSheet = self$calculateReservesBalanceSheet();
            self$Values$basicData = self$getBasicDataTimeseries()
            self$Values$premiumComposition = self$premiumAnalysis();
            self$Values$premiumCompositionSums = self$premiumCompositionSums();
            self$Values$premiumCompositionPV = self$premiumCompositionPV();

            # self$Values$profitParticipation = self$profitParticipation();
            # self$Values$reservesInclProfit = self$calculateReservesWithProfit();

            self$addHistorySnapshot(
                time    = 0,
                comment = "Initial contract values",
                type    = "Contract",
                params  = self$Parameters,
                values  = self$Values
            );
        },

        determineTransitionProbabilities = function() {
            self$tarif$getTransitionProbabilities(params=self$Parameters);
        },
        determineCashFlowsBasic = function() {
            self$tarif$getBasicCashFlows(params=self$Parameters);
        },
        determineCashFlows = function() {
            self$tarif$getCashFlows(params=self$Parameters, values=self$Values);
        },
        determinePremiumSum = function() {
            sum(self$Values$cashFlows$premiums_advance + self$Values$cashFlows$premiums_arrears);
        },
        determineCashFlowsCosts = function() {
            self$tarif$getCashFlowsCosts(params = self$Parameters, values = self$Values);
        },
        calculatePresentValues = function() {
            self$tarif$presentValueCashFlows(params=self$Parameters, values=self$Values);
        },
        calculatePresentValuesCosts = function() {
            self$tarif$presentValueCashFlowsCosts(params=self$Parameters, values=self$Values);
        },
        calculatePremiums = function() {
            self$tarif$premiumCalculation(params=self$Parameters, values=self$Values);
        },
        calculatePresentValuesBenefits = function() {
            self$tarif$presentValueBenefits(params=self$Parameters, values=self$Values);
        },
        calculateAbsCashFlows = function() {
            self$tarif$getAbsCashFlows(params=self$Parameters, values=self$Values);
        },
        calculateAbsPresentValues = function() {
            self$tarif$getAbsPresentValues(params=self$Parameters, values=self$Values);
        },
        calculateReserves = function() {
            self$tarif$reserveCalculation(params=self$Parameters, values=self$Values);
        },
        calculateReservesBalanceSheet = function() {
            self$tarif$reserveCalculationBalanceSheet(params=self$Parameters, values=self$Values);
        },
        premiumAnalysis = function() {
            self$tarif$premiumDecomposition(params=self$Parameters, values=self$Values);
        },
        premiumCompositionSums = function() {
            self$tarif$calculateFutureSums(self$Values$premiumComposition);
        },
        premiumCompositionPV = function() {
            self$tarif$calculatePresentValues(self$Values$premiumComposition, params=self$Parameters);
        },

        profitParticipationRates = function() {
            self$tarif$profitParticipationRates(params=self$Parameters, values=self$Values);
        },

        profitParticipation = function(rates) {
            self$tarif$profitParticipation(rates=rates, params=self$Parameters, values=self$Values);
        },
        calculateReservesWithProfit = function() {
            self$tarif$reservesWithProfit(params=self$Parameters, values=self$Values);
        },


        getBasicDataTimeseries = function() {
            self$tarif$getBasicDataTimeseries(params=self$Parameters, values=self$Values);
        },

        # Premium Waiver: Stop all premium payments at time t
        # the SumInsured is determined from the available
        premiumWaiver = function (t) {
            newSumInsured = self$Values$reserves[[toString(t), "PremiumFreeSumInsured"]];
            self$Parameters$ContractState$premiumWaiver = TRUE;
            self$Parameters$ContractState$surrenderPenalty = FALSE; # Surrender penalty has already been applied, don't apply a second time
            self$Parameters$ContractState$alphaRefunded = TRUE;     # Alpha cost (if applicable) have already been refunded partially, don't refund again

            self$Parameters$ContractData$sumInsured = newSumInsured;

            self$Values$cashFlowsBasic = mergeValues(starting=self$Values$cashFlowsBasic, ending=self$determineCashFlowsBasic(), t=t);
            self$Values$cashFlows = mergeValues(starting=self$Values$cashFlows, ending=self$determineCashFlows(), t=t);
            # Premium sum is not affected by premium waivers, i.e. everything depending on the premium sum uses the original premium sum!
            # self$Values$premiumSum = self$determinePremiumSum();
            self$Values$cashFlowsCosts = mergeValues3D(starting=self$Values$cashFlowsCosts, ending=self$determineCashFlowsCosts(), t=t);

            pv = self$calculatePresentValues();
            pvc = self$calculatePresentValuesCosts();
            self$Values$presentValuesCosts = mergeValues3D(starting=self$Values$presentValuesCosts, ending=pvc, t=t);

            # TODO:
            # the premiumCalculation function returns the premiums AND the cofficients,
            # so we have to extract the coefficients and store them in a separate variable
            # res = self$calculatePremiums(t);
            # self$Values$premiumCoefficients = mergeValues(starting=self$Values$premiumCoefficients, ending=res[["coefficients"]], t=t);
            # self$Values$premiums = mergeValues(starting= = res[["premiums"]]

            # Update the cash flows and present values with the values of the premium
            pvAllBenefits = self$calculatePresentValuesBenefits()
            self$Values$presentValues = mergeValues(starting=self$Values$presentValues, ending=cbind(pv, pvAllBenefits), t=t);

            self$Values$absCashFlows       = mergeValues(starting=self$Values$absCashFlows,       ending=self$calculateAbsCashFlows(), t=t);
            self$Values$absPresentValues   = mergeValues(starting=self$Values$absPresentValues,   ending=self$calculateAbsPresentValues(), t=t);
            self$Values$reserves           = mergeValues(starting=self$Values$reserves,           ending=self$calculateReserves(), t=t);
            self$Values$basicData          = mergeValues(starting=self$Values$basicData,          ending=self$getBasicDataTimeseries(), t=t);
            self$Values$premiumComposition = mergeValues(starting=self$Values$premiumComposition, ending=self$premiumAnalysis(), t=t);

            self$addHistorySnapshot(time = t, comment = sprintf("Premium waiver at time %d", t),
                                    type = "PremiumWaiver", params = self$Parameters, values = self$Values);
        },

        dummy=NULL
    )
);
# InsuranceContract$debug("premiumWaiver")
