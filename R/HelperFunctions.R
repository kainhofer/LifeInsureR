calculatePVSurvival = function(q, advance, arrears, v=1) {
  # assuming advance and arrears have the same dimensions...
  init = advance[1]*0;
  l = max(length(q), length(advance), length(arrears));
  q = pad0(q, l, value=1);
  advance = pad0(advance, l, value=init);
  arrears = pad0(arrears, l, value=init);

  # TODO: Make this work for matrices (i.e. currnently advance and arrears are assumed to be one-dimensional vectors)
  # TODO: Replace loop by better way (using Reduce?)
  res = rep(0, l+1);
  for (i in l:1) {
    res[i] = advance[i] + v*(1-q[i])*(arrears[i] + res[i+1]);
  }
  res
}

calculatePVDeath = function(q, benefits, v=1) {
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
  res
}

pad0 = function(v, l, value=0) {
  if (l>=length(v)) {
    c(v, rep(value, l-length(v)))
  } else {
    v[0:l]
  }
}

