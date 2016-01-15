# Basic Formulas --------------------------------------------------------------

reliability = function(lams){
  # reliability of total (unweighted linear composite)
  sum(lams)^2 / ( sum(lams)^2 + sum(1-lams^2) )
}

ic_sub_ttl = function(lams){
  # subject-total correlation
  # lambda * sqrt(reliability of others)
  sapply(1:length(lams), function(ii) 
    lams[ii]*sqrt(reliability(lams[-ii]))
  )
}

ic_isc = function(lams){
  # inter-subject correlation
  # lam_i * lam_j for all pairwise combinations
  pairs = combn(lams, 2)
  pairs[1,] * pairs[2,]
}

bg_sub_ttl = function(lams1, lams2, rho){
  # between-group subject-total correlation
  ttl_rel = reliability(lams2)
  lams1 * sqrt(ttl_rel) * rho
}

bg_isc = function(lams1, lams2, rho){
  # between-group inter-subject correlation
  outer(lams1, lams2)*rho
}

bg_ttl_ttl = function(lams1, lams2, rho){
  # between-group total-total correlation
  rho * sqrt(reliability(lams1) * reliability(lams2))
}

# Bias and friends ------------------------------------------------------------
ic_bias = function(lambdas) data.frame(
  N =        length(lambdas), 
  mean_lam = mean(lambdas),
  isc =      mean(ic_isc(lambdas)), 
  sub_ttl =  mean(ic_sub_ttl(lambdas)), 
  rel =      reliability(lambdas)
)

ttl_ttl_bias = function(lams1, lams2, rho){
  data.frame(
    N1 = length(lams1), N2 = length(lams2),
    mean_lam1 = mean(lams1),
    mean_lam2 = mean(lams2),
    rho = rho,
    est_lam1 = mean(ic_sub_ttl(lams1)),
    est_lam2 = mean(ic_sub_ttl(lams2)),
    est_rho = bg_ttl_ttl(lams1, lams2, rho),
    bg_sub_ttlB = bg_sub_ttl(lams1, lams2, rho),
    bg_sub_ttlA = bg_sub_ttl(lams2, lams1, rho)
  )
}
