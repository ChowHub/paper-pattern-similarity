library(importr)

tests = import('../../R/model_tests.R')

time <- function(n, ...){
  a <- proc.time()
  replicate(n, ...)
  (proc.time() - a) / n
}

dat <- data.frame(tests$gen_data(Nobs=400, nsubs1=10, nsubs2=20, lam1=.5, lam2=.7, rho=1))

time(10,
  tests$perm_test(tests$btwn_isc, R = 1000, dat, upper=TRUE)
)

time(10,
  tests$perm_test(tests$btwn_sub_ttl, R= 1000, dat, upper=TRUE)
)

time(10,
  tests$cfa_param_test(dat)
)


