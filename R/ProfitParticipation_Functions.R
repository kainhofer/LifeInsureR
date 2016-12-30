#' @include HelperFunctions.R InsuranceParameters.R
#'
#' @import dplyr
NULL

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# Calculation bases for the various types of profit
#
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


#--------------------------------------------------------------------------
# Calculation basis for Interest profit and profit on interest
#--------------------------------------------------------------------------

#' @describeIn ProfitParticipation
#' Basis for profit: Previous Zillmer reserve (no administration cost reserve)
#' @export
profPart.base.PreviousZillmerReserve = function(params, values) {
    nm = names(values$reserves[,"Zillmer"])
    res = c(0, head(values$reserves[,"Zillmer"], -1))
    names(res) = nm
    res
};

#' @describeIn ProfitParticipation
#' Basis for profit: Zillmer reserve (no administration cost reserve) at time t-2
#' @export
profPart.base.ZillmerReserveT2 = function(params, values) {
    nm = names(values$reserves[,"Zillmer"])
    res = c(0, 0, head(values$reserves[,"Zillmer"], -2))
    names(res) = nm
    res
};

#' @describeIn ProfitParticipation
#' Basis for profit: Contractual reserve (including administration costs) averaged over t and t-1
#' @export
profPart.base.meanContractualReserve = function(params, values) {
    # Rolling mean of the value for the current and previous year.
    pmax(0, rollingmean(c(0, values$reserves[,"contractual"])))
};

#' @describeIn ProfitParticipation
#' Basis for risk/mortality profit: Zillmer Risk Premium of the past year
#' @export
profPart.base.ZillmerRiskPremium = function(params, values) {
    # The risk premium of t=0 is used to determine the risk profit at time
    # t=1, so shift the whole vector!
    c(0, head(values$premiumComposition[,"Zillmer.risk"], -1))
};

#' @describeIn ProfitParticipation
#' Basis for expense/sum profit: sum insured
#' @export
profPart.base.sumInsured = function(params, values) {
    params$ContractData$sumInsured
};




#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Profit rates for the various types of profit
# Can / shall be overridden in child classes that use other schemes!
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
getInterestProfitRate = function(rates, params, values) {
    rates$interestProfitRate
};
getRiskProfitRate = function(rates, params, values) {
    rates$mortalityProfitRate
};
getExpenseProfitRate = function(rates, params, values) {
    rates$expenseProfitRate
};
getSumProfitRate = function(rates, params, values) {
    rates$sumProfitRate
};
getTerminalBonusRate = function(rates, params, values) {
    rates$terminalBonusRate
};

#' @describeIn ProfitParticipation
#' Rate for interest on past profits: total interest rate
#' @export
profPart.rate.totalInterest = function(rates, params, values) {
    rates$totalInterest
};

#' @describeIn ProfitParticipation
#' Rate for interest on past profits: second total interest rate
#' @export
profPart.rate.totalInterest2 = function(rates, params, values) {
    rates$totalInterest2
};


getTerminalBonusReserves = function(rates, terminalBonus, terminalBonusAccount, params, values) {
    n = length(terminalBonusAccount)
    terminalBonusAccount * 1/(1.07) ^ ((n - 1):0)
};


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Calculations of the assigned profit amounts, based on the bases and
# rates defined with the functions above.
# Can / shall be overridden in child classes that use other bases!
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' @describeIn ProfitParticipation
#' Calculate profit by a simple rate applied on the basis (with an optional waiting vector of values 0 or 1)
#' @export
profPart.calculate.RateOnBase = function(base, rate, waiting, params, values) {
    base * rate * waiting
}



calculateSurvivalBenefit = function(profits, rates, params, values) {
    profits[,"totalProfit"] + profits[,"terminalBonusReserve"]
};
calculateDeathBenefitAccrued = function(profits, rates, params, values) {
    profits[,"totalProfit"]*(1 + rates$guaranteedInterest)
};
calculateDeathBenefitTerminal = function(profits, rates, params, values) {
    n = params$ContractData$policyPeriod;
    profits[, "terminalBonusReserve"] * (0:n)/n * ((0:n) >= max(10, n - 5))
};
calculateSurrenderBenefitAccrued = function(profits, rates, params, values) {
    profits[,"totalProfit"]*(1 + rates$guaranteedInterest / 2)
};
calculateSurrenderBenefitTerminal = function(profits, rates, params, values) {
    n = params$ContractData$policyPeriod;
    profits[, "terminalBonusReserve"] * (0:n)/n * ((0:n) >= max(10, n - 5))
};
calculatePremiumWaiverBenefitAccrued = function(profits, rates, params, values) {
    profits[,"totalProfit"]
};
calculatePremiumWaiverBenefitTerminal = function(profits, rates, params, values) {
    n = params$ContractData$policyPeriod;
    profits[, "terminalBonusReserve"] * (0:n)/n * ((0:n) >= max(10, n - 5))
};


