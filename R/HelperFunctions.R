#' @importFrom abind abind
#' @importFrom objectProperties setSingleEnum
#' @importFrom utils head tail
#' @importFrom methods new
NULL



#' Enum to describe when a benefit or premium payment is due (in advance or in arrears)
#' @details Currently, only two values are allowed;
#' \itemize{
#'     \item "in advance"
#'     \item "in arrears"
#' }
#'
#' @export
PaymentTimeEnum = objectProperties::setSingleEnum("PaymentTime", levels = c("in advance", "in arrears"));

#' Enum to describe possible sexes in an insurance contract or tariff.
#' @details
#' Currently, the only possible values are:
#' * "unisex"
#' * "male"
#' * "female"
#'
#' @export
SexEnum = objectProperties::setSingleEnum("Sex", levels = c("unisex", "male", "female"));

#' Enum to define how much of a contract needs to be calculated automatically.
#' @details
#' When an [InsuranceContract] object is created, all time series are immediately
#' calculated. However, sometimes, one only needs part of the values, so it
#' would be a waste of resources to calculate e.g. all future reserves and
#' profit participation, if only premiums are of interest.
#'
#' Possible values are:
#' * "all"
#' * "probabilities"
#' * "cashflows"
#' * "presentvalues"
#' * "premiums"
#' * "absvalues"
#' * "reserves"
#' * "premiumcomposition"
#' * "profitparticipation"
#' * "history"
#'
#' @export
CalculationEnum = objectProperties::setSingleEnum("Calculation",
    levels = c(
      "all",
      "probabilities",
      "cashflows",
      "presentvalues",
      "premiums",
      "absvalues",
      "reserves",
      "premiumcomposition",
      "profitparticipation",
      "history"
    )
)


#' Enum to define the different components of profit participation.
#' @details
#' Profit participation schemes typically consist of different components,
#' which are calculated independently. Typical components are interest profit
#' to distribute investment gains to the customer, risk profit and expense profit
#' to return security margins in the biometric risk and the expenses to the customer
#' and sum profit, which aplies to contracts with higher sums insured, where
#' charged expenses are calculated from the sum insured, while the actual
#' expenses are more or less constant. Thus, high contracts are charged more,
#' which causes profits that are returned as sum profit.
#'
#' As a special case, part of the profits can be stored in a terminal bonus
#' reserve and only distributed on maturity (or potentially on death), but
#' not on surrender. Some (older) profit participation schemes add an independently
#' calculated bonus on maturity (e.g. twice the total profit assignment of the
#' last year) at maturity to give customers an additional incentive not to
#' surrender a contract.
#'
#' Possible values are (multiple can be given):
#' * "interest"
#' * "risk"
#' * "expense"
#' * "sum"
#' * "terminal"
#' * "TBF"
#'
#' @export
ProfitComponentsEnum = objectProperties::setMultipleEnum("ProfitComponents",
    levels = c(
      "advance",
      "interest",
      "risk",
      "expense",
      "sum",
      "terminal",
      "TBF"
    )
)

#' Describes the death benefit of a linearly decreasing whole life insurance (after a possible deferall period)
#'
#' The death benefit will be the full sumInsured for the first year after the
#' deferral period and then decrease linearly to 0 at the end of the policyPeriod.
#' This can be used with the \code{deathBenefit} parameter for insurance
#' contracts, but should not be called directly.
#'
#' @param len The desired length of the Cash flow vector (can be shorter than
#'            the policyPeriod, if q_x=1 before the end of the contract, e.g.
#'            for life-long insurances)
#' @param params The full parameter set of the insurance contract (including
#'               all inherited values from the tariff and the profit participation)
#' @param values The values calculated from the insurance contract so far
#'
#' @export
deathBenefit.linearDecreasing = function(len, params, values) {
    protectionPeriod = params$ContractData$policyPeriod - params$ContractData$deferralPeriod;
    pad0((protectionPeriod:0) / protectionPeriod, l = len)
}


#' Describes the death benefit of a decreasing whole life insurance (after a possible deferall period)
#'
#' The death benefit will be the full sumInsured for the first year after the
#' deferral period and then decrease like an annuity to 0 at the end of the policyPeriod.
#' This can be used with the \code{deathBenefit} parameter for insurance
#' contracts, but should not be called directly.
#'
#' This function is a mere generator function, which takes the interest rate and
#' generates a function that describes a decreasing annuity.
#'
#' The generated function has the following parameters:
#' \describe{
#'     \item{len}{The desired length of the Cash flow vector (can be shorter than
#'            the policyPeriod, if q_x=1 before the end of the contract, e.g.
#'            for life-long insurances)}
#'     \item{params}{The full parameter set of the insurance contract (including
#'               all inherited values from the tariff and the profit participation)}
#'     \item{values}{The values calculated from the insurance contract so far}
#' }
#'
#'
#' @param interest The interest rate of the loan, which is underlying the insurance.
#'
#'
#' @export
deathBenefit.annuityDecreasing = function(interest) {
    function(len, params, values) {
        protectionPeriod = params$ContractData$policyPeriod - params$ContractData$deferralPeriod;
        vk = 1/(1 + interest);
        if (interest == 0) {
            sumInsured = (protectionPeriod:0) / protectionPeriod
        } else {
            sumInsured = (vk ^ (protectionPeriod:0) - 1) / (vk ^ protectionPeriod - 1)
        }
        pad0(sumInsured, l = len)
    }
}

mergeValues = function(starting, ending, t) {
  # if either starting or ending is missing, always use the other, irrespective of t:
  if (missing(ending) || is.null(ending)) {
    starting
  } else if (missing(starting) || is.null(starting)) {
    ending
  } else if (t == 0) {
    ending
  } else {
    rbind(starting[1:t,], ending[-1:-t,])
  }
}
mergeValues3D = function(starting, ending, t) {
  # if either starting or ending is missing, always use the other, irrespective of t:
  if (missing(ending) || is.null(ending)) {
    starting
  } else if (missing(starting) || is.null(starting)) {
    ending
  } else if (t == 0) {
    ending
  } else {
    abind::abind(starting[1:t,,], ending[-1:-t,,], along = 1)
  }
}
# Caution: px is not neccessarily 1-qx, because we might also have dread diseases so that px=1-qx-ix! However, the ix is not used for the survival present value
calculatePVSurvival = function(px = 1 - qx, qx = 1 - px, advance, arrears = c(0), ..., m = 1, mCorrection = list(alpha = 1, beta = 0), v = 1, start = 0) {
  # assuming advance and arrears have the same dimensions...
  init = advance[1]*0;
  l = max(length(qx), length(advance), length(arrears));
  p = pad0(px, l, value=0);
  advance = pad0(advance, l, value=init);
  arrears = pad0(arrears, l, value=init);

  # TODO: Make this work for matrices (i.e. currently advance and arrears are assumed to be one-dimensional vectors)
  # TODO: Replace loop by better way (using Reduce?)
  res = rep(0, l+1);
  for (i in l:(start + 1)) {
    # coefficients for the payments (including corrections for payments during the year (using the alpha(m) and beta(m)):
    advcoeff = mCorrection$alpha - mCorrection$beta*(1-p[i]*v);
    arrcoeff = mCorrection$alpha - (mCorrection$beta + 1/m)*(1-p[i]*v);
    # The actual recursion:
    res[i] = advance[i]*advcoeff + arrears[i]*arrcoeff + v*p[i]*res[i+1];
  }
  res[1:l]
}


calculatePVGuaranteed = function(advance, arrears = c(0), ..., m = 1, mCorrection = list(alpha = 1, beta = 0), v = 1, start = 0) {
  # assuming advance and arrears have the same dimensions...
  init = advance[1]*0;
  l = max(length(advance), length(arrears));
  advance = pad0(advance, l, value = init);
  arrears = pad0(arrears, l, value = init);

  # TODO: Make this work for matrices (i.e. currently advance and arrears are assumed to be one-dimensional vectors)
  # TODO: Replace loop by better way (using Reduce?)
  res = rep(0, l + 1);
  for (i in l:(start + 1)) {
    # coefficients for the payments (including corrections for payments during the year (using the alpha(m) and beta(m)):
    advcoeff = mCorrection$alpha - mCorrection$beta * (1 - v);
    arrcoeff = mCorrection$alpha - (mCorrection$beta + 1 / m) * (1 - v);
    # The actual recursion:
    res[i] = advance[i]*advcoeff + arrears[i]*arrcoeff + v*res[i + 1];
  }
  res[1:l]
}


# TODO: So far, we are assuming, the costs array has sufficient time steps and does not need to be padded!
calculatePVCosts = function(px = 1 - qx, qx = 1 - px, costs, ..., v = 1, start = 0) {
  l = max(length(qx), dim(costs)[1]);
  p = pad0(px, l, value = 0);
  costs = costs[1:l,,];

  # Take the array structure from the cash flow array and initialize it with 0
  res = costs*0;
  prev = res[1,,]*0;
  # Backward recursion starting from the last time:
  for (i in l:(start + 1)) {
    # cat("values at iteration ", i, ": ", v, q[i], costs[i,,], prev);
    res[i,,] = costs[i,,] + v*p[i]*prev;
    prev = res[i,,];
  }
  res
}

calculatePVDeath = function(px, qx, benefits, ..., v = 1, start = 0) {
  init = benefits[1]*0; # Preserve the possible array structure of the benefits -> vectorized calculations possible!
  l = max(length(qx), length(benefits));
  q = pad0(qx, l, value = 1);
  p = pad0(px, l, value = 0);
  benefits = pad0(benefits, l, value = init);

  # TODO: Make this work for matrices (i.e. currently benefits are assumed to be one-dimensional vectors)
  # TODO: Replace loop by better way (using Reduce?)
  res = rep(init, l + 1);
  for (i in l:(start + 1)) {
    # Caution: p_x is not neccessarily 1-q_x, because we might also have dread diseases, so that px=1-qx-ix!
    res[i] = v * q[i] * benefits[i] + v * p[i] * res[i + 1];
  }
  res[1:l]
}

calculatePVDisease = function(px = 1 - qx - ix, qx = 1 - ix - px, ix = 1 - px - qx, benefits, ..., v = 1, start = 0) {
  init = benefits[1] * 0;
  l = min(length(ix), length(qx), length(benefits));
  qx = pad0(qx, l, value = 1);
  ix = pad0(ix, l, value = 0);
  px = pad0(px, l, value = 0);
  benefits = pad0(benefits, l, value = init);

  # TODO: Make this work for matrices (i.e. currently benefits are assumed to be one-dimensional vectors)
  # TODO: Replace loop by better way (using Reduce?)
  res = rep(init, l + 1);
  for (i in l:(start + 1)) {
    res[i] = v * ix[i] * benefits[i] + v * px[i] * res[i + 1];
  }
  res[1:l]
}



getSavingsPremium = function(reserves, v=1, survival_advance=c(0), survival_arrears=c(0)) {
  pad0(reserves[-1], length(reserves))*v - reserves + survival_advance + survival_arrears*v
}

correctionPaymentFrequency = function(i, m = 1, order = 0) {
  # 0th-order approximation
  alpha = 1;
  beta = 0;
  # negative orders mean that NO correction is done, e.g. because other means of
  # correction are used like an explicit premium frequency loading on the premium.
  if (order >= 0 ) beta = beta + (m - 1) / (2 * m);
  # For higher orders, simply add one term after the other!
  if (order >= 1)     beta = beta + (m ^ 2 - 1) / (6 * m ^ 2) * i; # S-Versicherung: *(1-i/2)
  # order 1.5 has a special term that should NOT be used for higher-order approximations!
  if (order == 1.5)   beta = beta + (1 - m ^ 2) / (12 * m ^ 2) * i ^ 2;

  if (order >= 2) {
    beta = beta + (1 - m ^ 2) / (24 * m ^ 2) * i ^ 2;
    alpha = alpha + (m ^ 2 - 1) / (12 * m ^ 2) * i ^ 2;
  }
  # Exact value
  if (order == Inf) {
    d = i / (1 + i);
    im = m * ((1 + i) ^ (1/m) - 1);
    dm = im / (1 + im/m);

    alpha = d*i / (dm*im);
    beta = (i - im) / (dm * im);
  }
  list(alpha = alpha, beta = beta);
}

#' Pad a vector with 0 to a desired length
#'
#' @param v the vector to pad with 0
#' @param l the desired (resulting) length of the vector
#' @param value the value to pad with (if padding is needed). Default to 0, but
#'     can be overridden to pad with any other value.
#' @param start the first \code{start} values are always set to 0 (default is 0),
#'     the vector \code{v} starts only after these leading zeroes. The number of
#'     leading zeroes counts towards the desired length
#'
#' @return returns the vector \code{v} padded to length \code{l} with value \code{value} (default 0).
#'
#' @examples
#' pad0(1:5, 7)   # Pad to length 7 with zeroes
#' pad0(1:5, 3)   # no padding, but cut at length 3
#'
#' # 3 leading zeroes, then the vector start (10 elements of vector, no additional padding needed):
#' pad0(1:10, 13, start = 3)
#'
#' # padding with value other than zero:
#' pad0(1:5, 7, value = "pad")
#' @export
pad0 = function(v, l, value = 0, start = 0) {
  # 3 cases: desired length<=start => only 0
  #          desired length within start+v => cut v
  #          desired length longer than start+v => pad with 0/value
  if (l <= start) {
    rep(0, l)
  } else if (start <= l && l <= start + length(v)) {
    c(rep(0, start), v[0:(l - start)])
  } else {
    # Need padding
    c(rep(0, start), v, rep(value, l - length(v) - start))
  }
}

#' Set all entries of the given vector to 0 up until index 'start'
#' @param v the vector to modify
#' @param start how many leading elements to zero out
#'
#' @return the vector \code{v} with the first \code{start} elements replaced by 0.
#'
#' @examples
#' head0(1:10, 3)
#' @export
head0 = function(v, start = 0) {
  if (start == 0) {
    v
  } else {
    c(rep(0, start), tail(v, -start))
  }
}

#' Pad the vector \code{v} to length \code{l} by repeating the last entry of the
#' vector.
#'
#' This function callc [pad0()] with the last element of the vector as padding value
#'
#' @param v the vector to pad by repeating the last element
#' @param l the desired (resulting) length of the vector
#' @param start the first \code{start} values are always set to 0 (default is 0),
#'     the vector \code{v} starts only after these leading zeroes. The number of
#'     leading zeroes counts towards the desired length
#'
#' @examples
#' padLast(1:5, 7) # 5 is repeated twice
#' padLast(1:5, 3) # no padding needed
#'
#' @export
padLast = function(v, l, start = 0) {
    pad0(v, l, value = tail(v, n = 1), start = start)
}

#' Replace all \code{NA} entries of a vector with the previous non-NA value
#'
#' Sometimes one has a vector with some gaps (\code{NA}) values, which cause
#' problems for several numeric functions. This function \code{fillNAgaps} fills
#' these missing values by inserting the last preceeding non-NA-value. Leading
#' NA values (at the start of the vector will not be modified). If the
#' argument \code{firstBack = TRUE}, leading \code{NA}-values are replaced by
#' the first non-NA value.
#' Trailing NAs are always replaced by the last previous NA-value.
#'
#' This code was taken from the R Cookbook:
#' http://www.cookbook-r.com/Manipulating_data/Filling_in_NAs_with_last_non-NA_value/
#' LICENSE (from that page): The R code is freely available for use without any restrictions.
#' In other words: you may reuse the R code for any purpose (and under any license).
#'
#' @param x The vector where NA-values should be filled by repeating the last preceeding non-NA value
#' @param firstBack if \code{TRUE}, leading NAs are replaced by the first non-NA
#'     value in the vector, otherwise leading NAs are left untouched.
#'
#' @export
fillNAgaps <- function(x, firstBack=FALSE) {
    ## NA's in a vector or factor are replaced with last non-NA values
    ## If firstBack is TRUE, it will fill in leading NA's with the first
    ## non-NA value. If FALSE, it will not change leading NA's.

    # If it's a factor, store the level labels and convert to integer
    lvls <- NULL
    if (is.factor(x)) {
        lvls <- levels(x)
        x    <- as.integer(x)
    }

    goodIdx <- !is.na(x)

    # These are the non-NA values from x only
    # Add a leading NA or take the first good value, depending on firstBack
    if (firstBack)   goodVals <- c(x[goodIdx][1], x[goodIdx])
    else             goodVals <- c(NA,            x[goodIdx])

    # Fill the indices of the output vector with the indices pulled from
    # these offsets of goodVals. Add 1 to avoid indexing to zero.
    fillIdx <- cumsum(goodIdx) + 1

    x <- goodVals[fillIdx]

    # If it was originally a factor, convert it back
    if (!is.null(lvls)) {
        x <- factor(x, levels = seq_along(lvls), labels = lvls)
    }

    x
}


#' If \code{val} is a function, evaluate it, otherwise return \code{val}
#' @param val Function or value
#' @param ... Argument passed to \code{val} if it is a function
#' @examples
#' valueOrFunction(3) # returns 3
#' valueOrFunction(`+`, 1, 2) # also returns 3
#' A = `+`
#' valueOrFunction(A, 1, 2)
#' @export
valueOrFunction = function(val, ...) {
  if (is.function(val)) {
    val(...)
  } else {
    val
  }
}

#' If \code{hook} is a function, apply it to \code{val}, otherwise return \code{val} unchanged
#' @param hook (optional) function to apply to \code{val} and the other parameters
#' @param val The value to which the hook is applied (ifgiven)
#' @param ... optional parameters passed to the hook function (if it is a function)
#' @examples
#' applyHook(NULL, 3) # returns 3 unchanged
#' applyHook(function(x) 2*x, 3) # applies the function, returns 6
#' applyHook(`+`, 3, 1) # returns 4
#' @export
applyHook = function(hook, val, ...) {
  if (is.function(hook)) {
    hook(val, ...)
  } else if (is.null(hook)) {
    val
  } else {
    warning("Hook function", hook, "is neither a function nor NULL. Please provide a function or leave it empty!")
  }
}



#' Overwrite all existing fields in the first argument with
#' values given in valuelist. Members of valuelist that are not yet in
#' fields are ignored. This allows a huge valuelist to be used to fill
#' fields in multiple lists with given structure.
#'
#' @param fields existing list
#' @param valuelist list of fields to replace in \code{fields}. Only keys that exist in \code{fields} are overwritten, no new fields are added to \code{fields}
#'
#' @export
fillFields = function(fields, valuelist) {
  fieldsToInsert = intersect(names(fields), names(valuelist));
  fields[fieldsToInsert] = valuelist[fieldsToInsert]
  fields
}

#' Replace all missing values in fields (either missing or NA) with
#' their corresponding values from fallback. Members in fallback that are missing
#' in fields are inserted
#' @param fields existing list
#' @param valuelist list of fields to replace in \code{fields}. Only keys that are missing in \code{fields} are added, no existing fields in \code{fields} are overwritten
#' @export
fallbackFields = function(fields, valuelist) {
  keepFields = !sapply(fields, is.null);
  # We need to set all fields of valuelist, except those that are NOT NA in fields:
  useFields = setdiff(names(valuelist), names(fields[keepFields]))
  fields[useFields] = valuelist[useFields]
  fields
}

#' Calculate the rolling mean of length 2
#' @param x vector of values, for which the rolling mean is calculated
#' @examples
#' rollingmean(1:10)
#' @export
rollingmean = function(x) (tail(x, -1) + head(x, -1))/2


# Sum two or more vectors and correctly handle (i.e. ignore) NULL values given
plusNULL = function(v1, v2, ...) {
  if (missing(v2) && length(list(...)) == 0) {
    if (missing(v1) || is.null(v1)) {
      return(0)
    } else {
      return(v1)
    }
  }
  if (missing(v1) || is.null(v1)) {
    return(plusNULL(v2, ...));
  }
  if (missing(v2) || is.null(v2)) {
    return(plusNULL(v1, ...));
  } else {
    return(plusNULL(v1 + v2, ...))
  }
}


######################################################################=#
# Functions for handling sub-contract blocks                        ####

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


