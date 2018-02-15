#' @include HelperFunctions.R InsuranceParameters.R
#'
#' @import dplyr
NULL

#' Helper functions for profit participation
#'
#' Various helper functions for the \code{ProfitParticipation} class that
#' provide the building blocks for the individual components of profit participation,
#' the rates and how the assigned profit is calculated.
#'
#' @name ProfitParticipationFunctions
NULL


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# Calculation bases for the various types of profit
#
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#--------------------------------------------------------------------------
# Calculation basis for Interest profit and profit on interest
#--------------------------------------------------------------------------

#' @describeIn ProfitParticipationFunctions
#' Basis for profit: Previous Zillmer reserve (no administration cost reserve)
#' @export
profPart.base.PreviousZillmerReserve = function(rates, params, values, ...) {
    nm = names(values$reserves[,"Zillmer"])
    res = c(0, head(values$reserves[,"Zillmer"], -1))
    names(res) = nm
    res
};

#' @describeIn ProfitParticipationFunctions
#' Basis for profit: Zillmer reserve (no administration cost reserve) at time t-2
#' @export
profPart.base.ZillmerReserveT2 = function(rates, params, values, ...) {
    nm = names(values$reserves[,"Zillmer"])
    res = c(0, 0, head(values$reserves[,"Zillmer"], -2))
    names(res) = nm
    res
};

#' @describeIn ProfitParticipationFunctions
#' Basis for profit: Contractual reserve (including administration costs) at time t
#' @export
profPart.base.contractualReserve = function(rates, params, values, ...) {
    pmax(0, values$reserves[,"contractual"])
};

#' @describeIn ProfitParticipationFunctions
#' Basis for profit: Contractual reserve (including administration costs) averaged over t and t-1
#' @export
profPart.base.meanContractualReserve = function(rates, params, values, ...) {
    # Rolling mean of the value for the current and previous year.
    pmax(0, rollingmean(c(0, values$reserves[,"contractual"])))
};

#' @describeIn ProfitParticipationFunctions
#' Basis for risk/mortality profit: Zillmer Risk Premium of the past year
#' @export
profPart.base.ZillmerRiskPremium = function(rates, params, values, ...) {
    # The risk premium of t=0 is used to determine the risk profit at time
    # t=1, so shift the whole vector!
    c(0, head(values$premiumComposition[,"Zillmer.risk"], -1))
};

#' @describeIn ProfitParticipationFunctions
#' Basis for expense/sum profit: sum insured
#' @export
profPart.base.sumInsured = function(rates, params, values, ...) {
    params$ContractData$sumInsured
};




#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Profit rates for the various types of profit
# Can / shall be overridden in child classes that use other schemes!
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#' @describeIn ProfitParticipationFunctions
#' Returns the array of interest profit rates (keyed by year)
#' @export
profPart.rate.interestProfit = function(rates, params, values, ...) {
    rates$interestProfitRate
};

#' @describeIn ProfitParticipationFunctions
#' Returns the array of risk profit rates (keyed by year)
#' @export
profPart.rate.riskProfit = function(rates, params, values, ...) {
    rates$mortalityProfitRate
};
#' @describeIn ProfitParticipationFunctions
#' Returns the array of expense profit rates (keyed by year)
#' @export
profPart.rate.expenseProfit = function(rates, params, values, ...) {
    rates$expenseProfitRate
};

#' @describeIn ProfitParticipationFunctions
#' Returns the array of sum profit rates (keyed by year)
#' @export
profPart.rate.sumProfit = function(rates, params, values, ...) {
    rates$sumProfitRate
};

#' @describeIn ProfitParticipationFunctions
#' Returns the array of terminal bonus rates (keyed by year)
#' @export
profPart.rate.terminalBonus = function(rates, params, values, ...) {
    rates$terminalBonusRate
};

#' @describeIn ProfitParticipationFunctions
#' Returns the array of terminal bonus rates (keyed by year) as the terminal bonus fund ratio
#' @export
profPart.rate.terminalBonusFundRatio = function(rates, params, values, ...) {
    rates$terminalBonusFundRatio
};


#' @describeIn ProfitParticipationFunctions
#' Rate for interest on past profits: total interest rate
#' @export
profPart.rate.interestProfitPlusGuarantee = function(rates, params, values, ...) {
    rates$totalInterest + rates$guaranteedInterest
};

#' @describeIn ProfitParticipationFunctions
#' Rate for interest on past profits: total interest rate
#' @export
profPart.rate.totalInterest = function(rates, params, values, ...) {
    rates$totalInterest
};

#' @describeIn ProfitParticipationFunctions
#' Rate for interest on past profits: second total interest rate
#' @export
profPart.rate.totalInterest2 = function(rates, params, values, ...) {
    rates$totalInterest2
};

#' @describeIn ProfitParticipationFunctions
#' Rate for interest on past profits: second interest profit rate (not including guaranteed interest), keyed by year
#' @export
profPart.rate.interestProfit2 = function(rates, params, values, ...) {
    rates$interestProfitRate2
};


# TODO
getTerminalBonusReserves = function(profits, rates, terminalBonus, terminalBonusAccount, params, values, ...) {
    n = length(terminalBonusAccount)
    terminalBonusAccount * 1/(1.07) ^ ((n - 1):0)
};


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Calculations of the assigned profit amounts, based on the bases and
# rates defined with the functions above.
# Can / shall be overridden in child classes that use other bases!
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' @describeIn ProfitParticipationFunctions
#' Calculate profit by a simple rate applied on the basis (with an optional waiting vector of values 0 or 1)
#' @export
profPart.calculate.RateOnBase = function(base, rate, waiting, rates, params, values, ...) {
    base * rate * waiting
};

#' @describeIn ProfitParticipationFunctions
#' Calculate profit by a rate + guaranteed interest applied on the basis (with an optional waiting vector of values 0 or 1)
#' @export
profPart.calculate.RatePlusGuaranteeOnBase = function(base, rate, waiting, rates, params, values, ...) {
    base * (rate + rates$guaranteedInterest) * waiting
};

#' @describeIn ProfitParticipationFunctions
#' Calculate profit by a simple rate applied on the basis (with only (1-SGFFactor) put into profit participation, and an optional waiting vector of values 0 or 1)
#' @export
profPart.calculate.RateOnBaseSGFFactor = function(base, rate, waiting, rates, params, values, ...) {
    base * rate * waiting * (1 - rates$terminalBonusFundRatio)
};



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Calculations of the benefits, based on the assigned profit amounts and
# rates defined with the functions above.
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' @describeIn ProfitParticipationFunctions
#' Calculate survival benefit as total profit amount plus the terminal bonus reserve
#' @export
profPart.benefit.ProfitPlusTerminalBonusReserve = function(profits, rates, params, values) {
    profits[,"totalProfit"] + profits[,"terminalBonusReserve"]
};

#' @describeIn ProfitParticipationFunctions
#' Calculate benefit as total profit accrued so far
#' @export
profPart.benefit.Profit = function(profits, rates, params, values) {
    profits[,"totalProfit"]
};

#' @describeIn ProfitParticipationFunctions
#' Calculate accrued death benefit as total profit with (guaranteed) interest for one year
#' @export
profPart.benefit.ProfitPlusGuaranteedInterest = function(profits, rates, params, values) {
    profits[,"totalProfit"] * (1 + rates$guaranteedInterest)
};

#' @describeIn ProfitParticipationFunctions
#' Calculate accrued death benefit as total profit with total interest (interest on profit rate) for one year
#' @export
profPart.benefit.ProfitPlusTotalInterest = function(profits, rates, params, values) {
    profits[,"totalProfit"] * (1 + profits[,"interestOnProfitRate"])
};

#' @describeIn ProfitParticipationFunctions
#' Calculate accrued benefit as total profit with total interest (interest on profit rate) for half a year
#' @export
profPart.benefit.ProfitPlusHalfTotalInterest = function(profits, rates, params, values) {
    profits[,"totalProfit"] * (1 + profits[,"interestOnProfitRate"]/2)
};

#' @describeIn ProfitParticipationFunctions
#' Calculate death benefit as total profit with (guaranteed) interest for one year
#' @export
profPart.benefit.ProfitPlusHalfGuaranteedInterest = function(profits, rates, params, values) {
    profits[,"totalProfit"] * (1 + rates$guaranteedInterest/2)
};

#' @describeIn ProfitParticipationFunctions
#' Calculate accrued benefit as total profit with interest for one year (min of guarantee and total interest)
#' @export
profPart.benefit.ProfitPlusInterestMinGuaranteeTotal = function(profits, rates, params, values) {
    profits[,"totalProfit"] * (1 + pmin(rates$guaranteedInterest, rates$totalInterest))
};

#' @describeIn ProfitParticipationFunctions
#' Calculate accrued benefit as total profit with interest for half a year (min of guarantee and total interest)
#' @export
profPart.benefit.ProfitPlusHalfInterestMinGuaranteeTotal = function(profits, rates, params, values) {
    profits[,"totalProfit"] * (1 + pmin(rates$guaranteedInterest, rates$totalInterest)/2)
};

#' @describeIn ProfitParticipationFunctions
#' Calculate benefit from terminal bonus as 1/n parts of the terminal bonus reserve during the last 5 years
#' @export
profPart.benefit.TerminalBonus5YearsProRata = function(profits, rates, params, values) {
    n = params$ContractData$policyPeriod;
    profits[, "terminalBonusReserve"] * (0:n)/n * ((0:n) >= max(10, n - 5))
};

"dummy"
