calculatePVSurvival = function(q, advance, arrears, ..., m=1, mCorrection = list(alpha=1, beta=0), v=1) {
  # assuming advance and arrears have the same dimensions...
  init = advance[1]*0;
  l = max(length(q), length(advance), length(arrears));
  q = pad0(q, l, value=1);
  advance = pad0(advance, l, value=init);
  arrears = pad0(arrears, l, value=init);

  # TODO: Make this work for matrices (i.e. currently advance and arrears are assumed to be one-dimensional vectors)
  # TODO: Replace loop by better way (using Reduce?)
  res = rep(0, l+1);
  for (i in l:1) {
    # coefficients for the payemtns(including corrections for payments during the year (using the alpha(m) and beta(m)):
    p = (1-q[i]);
    advcoeff = mCorrection$alpha - mCorrection$beta*(1-p*v);
    arrcoeff = mCorrection$alpha - (mCorrection$beta + 1/m)*(1-p*v);
    # The actual recursion:
    res[i] = advance[i]*advcoeff + arrears[i]*arrcoeff + v*(1-q[i])*res[i+1];
  }
  res[1:l]
}



# TODO: So far, we are assuming, the costs array has sufficient time steps and does not need to be padded!
calculatePVCosts = function(q, costs, ..., v=1) {
  l = max(length(q), dim(costs)[1]);
  q = pad0(q, l, value=1);
  costs = costs[1:l,,];

  # Take the array structure from the cash flow array and initialize it with 0
  res = costs*0;
  prev = res[1,,]*0;
  # Backward recursion starting from the last time:
  for (i in l:1) {
    # cat("values at iteration ", i, ": ", v, q[i], costs[i,,], prev);
    res[i,,] = costs[i,,] + v*(1-q[i])*prev;
    prev=res[i,,];
  }
  res
}

calculatePVDeath = function(q, benefits, ..., v=1) {
  init = benefits[1]*0;
  l = max(length(q), length(benefits));
  q = pad0(q, l, value=1);
  benefits = pad0(benefits, l, value=init);

  # TODO: Make this work for matrices (i.e. currently benefits are assumed to be one-dimensional vectors)
  # TODO: Replace loop by better way (using Reduce?)
  res = rep(init, l+1);
  for (i in l:1) {
    res[i] = v*q[i]*benefits[i] + v*(1-q[i])*res[i+1];
  }
  res[1:l]
}


correctionPaymentsPerYear = function(m = 1, i = self$i, order = 0) {
  # 0th-order approximation
  alpha=1;
  beta=(m-1)/(2*m);

  # For higher orders, simply add one term after the other!
  if (order >= 1)     beta = beta + (m^2-1)/(6*m^2)*i;
  # order 1.5 has a special term that should NOT be used for higher-order approximations!
  if (order == 1.5)   beta = beta + (1-m^2)/(12*m^2)*i^2;

  if (order >= 2) {
    beta = beta + (1-m^2)/(24*m^2)*i^2;
    alpha = alpha + (m^2-1)/(12*m^2)*i^2;
  }
  # Exact value
  if (order == Inf) {
    d = i/(1+i);
    im = m * ((1+i)^(1/m) - 1);
    dm = im / (1+im/m);

    alpha = d*i / (dm*im);
    beta = (i-im) / (dm*im);
  }
  list(alpha=alpha, beta=beta);
}

pad0 = function(v, l, value=0) {
  if (l>=length(v)) {
    c(v, rep(value, l-length(v)))
  } else {
    v[0:l]
  }
}

