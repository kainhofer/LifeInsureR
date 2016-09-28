#' @import abind
#'

PaymentTimeEnum = setSingleEnum("PaymentTime", levels = c("in advance", "in arrears"))
#PaymentCountEnum = setSingleEnum(PaymentCount, levels = c(1,2,3))


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
#' @param len The desired length of the Cash flow vector (can be shorter than
#'            the policyPeriod, if q_x=1 before the end of the contract, e.g.
#'            for life-long insurances)
#' @param params The full parameter set of the insurance contract (including
#'               all inherited values from the tariff and the profit participation)
#' @param values The values calculated from the insurance contract so far
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
  rbind(starting[1:t,], ending[-1:-t,])
}
mergeValues3D = function(starting, ending, t) {
  abind(starting[1:t,,], ending[-1:-t,,], along = 1)
}
# Caution: px is not neccessarily 1-qx, because we might also have dread diseases so that px=1-qx-ix! However, the ix is not used for the survival present value
calculatePVSurvival = function(px=1-qx, qx=1-px, advance, arrears=c(0), ..., m=1, mCorrection = list(alpha=1, beta=0), v=1) {
  # assuming advance and arrears have the same dimensions...
  init = advance[1]*0;
  l = max(length(qx), length(advance), length(arrears));
  p = pad0(px, l, value=0);
  advance = pad0(advance, l, value=init);
  arrears = pad0(arrears, l, value=init);

  # TODO: Make this work for matrices (i.e. currently advance and arrears are assumed to be one-dimensional vectors)
  # TODO: Replace loop by better way (using Reduce?)
  res = rep(0, l+1);
  for (i in l:1) {
    # coefficients for the payments (including corrections for payments during the year (using the alpha(m) and beta(m)):
    advcoeff = mCorrection$alpha - mCorrection$beta*(1-p[i]*v);
    arrcoeff = mCorrection$alpha - (mCorrection$beta + 1/m)*(1-p[i]*v);
    # The actual recursion:
    res[i] = advance[i]*advcoeff + arrears[i]*arrcoeff + v*p[i]*res[i+1];
  }
  res[1:l]
}


calculatePVGuaranteed = function(advance, arrears=c(0), ..., m=1, mCorrection = list(alpha=1, beta=0), v=1) {
  # assuming advance and arrears have the same dimensions...
  init = advance[1]*0;
  l = max(length(advance), length(arrears));
  advance = pad0(advance, l, value=init);
  arrears = pad0(arrears, l, value=init);

  # TODO: Make this work for matrices (i.e. currently advance and arrears are assumed to be one-dimensional vectors)
  # TODO: Replace loop by better way (using Reduce?)
  res = rep(0, l+1);
  for (i in l:1) {
    # coefficients for the payments (including corrections for payments during the year (using the alpha(m) and beta(m)):
    advcoeff = mCorrection$alpha - mCorrection$beta*(1-v);
    arrcoeff = mCorrection$alpha - (mCorrection$beta + 1/m)*(1-v);
    # The actual recursion:
    res[i] = advance[i]*advcoeff + arrears[i]*arrcoeff + v*res[i+1];
  }
  res[1:l]
}


# TODO: So far, we are assuming, the costs array has sufficient time steps and does not need to be padded!
calculatePVCosts = function(px=1-qx, qx=1-px, costs, ..., v=1) {
  l = max(length(qx), dim(costs)[1]);
  p = pad0(px, l, value=0);
  costs = costs[1:l,,];

  # Take the array structure from the cash flow array and initialize it with 0
  res = costs*0;
  prev = res[1,,]*0;
  # Backward recursion starting from the last time:
  for (i in l:1) {
    # cat("values at iteration ", i, ": ", v, q[i], costs[i,,], prev);
    res[i,,] = costs[i,,] + v*p[i]*prev;
    prev=res[i,,];
  }
  res
}

calculatePVDeath = function(px, qx, benefits, ..., v=1) {
  init = benefits[1]*0; # Preserve the possible array structure of the benefits -> vectorized calculations possible!
  l = max(length(qx), length(benefits));
  q = pad0(qx, l, value=1);
  p = pad0(px, l, value=0);
  benefits = pad0(benefits, l, value=init);

  # TODO: Make this work for matrices (i.e. currently benefits are assumed to be one-dimensional vectors)
  # TODO: Replace loop by better way (using Reduce?)
  res = rep(init, l+1);
  for (i in l:1) {
    # Caution: p_x is not neccessarily 1-q_x, because we might also have dread diseases, so that px=1-qx-ix!
    res[i] = v*q[i]*benefits[i] + v*p[i]*res[i+1];
  }
  res[1:l]
}

calculatePVDisease = function(px = 1 - qx - ix, qx = 1 - ix - px, ix = 1 - px - qx, benefits, ..., v = 1) {
  init = benefits[1]*0;
  l = min(length(ix), length(qx), length(benefits));
  qx = pad0(qx, l, value = 1);
  ix = pad0(ix, l, value = 0);
  px = pad0(px, l, value = 0);
  benefits = pad0(benefits, l, value = init);

  # TODO: Make this work for matrices (i.e. currently benefits are assumed to be one-dimensional vectors)
  # TODO: Replace loop by better way (using Reduce?)
  res = rep(init, l + 1);
  for (i in l:1) {
    res[i] = v * ix[i] * benefits[i] + v * px[i] * res[i + 1];
  }
  res[1:l]
}



getSavingsPremium = function(reserves, v=1, survival_advance=c(0), survival_arrears=c(0)) {
  pad0(reserves[-1], length(reserves))*v - reserves + survival_advance + survival_arrears*v
}

correctionPaymentFrequency = function(m = 1, i = self$i, order = 0) {
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

#' @export
pad0 = function(v, l, value=0) {
    if (l >= length(v)) {
        c(v, rep(value, l - length(v)))
    } else {
        v[0:l]
    }
}
#' @export
padLast = function(v, l) {
    pad0(v, l, tail(v, n = 1))
}

valueOrFunction = function(val, ...) {
  if (is.function(val)) {
    val(...)
  } else {
    val
  }
}



# fillFields(fields, valuelist)
#
# Overwrite all existing fields in the first argument with
# values given in valuelist. Members of valuelist that are not yet in
# fields are ignored. This allows a huge valuelist to be used to fill
# fields in multiple lists with given structure.
fillFields = function (fields, valuelist) {
  fieldsToInsert = intersect(names(fields), names(valuelist));
  fields[fieldsToInsert] = valuelist[fieldsToInsert]
  fields
}

# fallbackFields(fields, fallback)
#
# Replace all missing values in fields (either missing or NA) with
# their corresponding values from fallback. Members in fallback that are missing
# in fields are inserted
fallbackFields = function (fields, valuelist) {
  keepFields = !sapply(fields, is.null);
  # We need to set all fields of valuelist, except those that are NOT NA in fields:
  useFields = setdiff(names(valuelist), names(fields[keepFields]))
  fields[useFields] = valuelist[useFields]
  fields
}

# extractProfitRates = function(rates, )

rollingmean = function(x) (tail(x, -1) + head(x, -1))/2
