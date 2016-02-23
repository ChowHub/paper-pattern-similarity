library(importr)
library(testthat)
tests = import('../../R/model_tests.R')


context('Testing stats on generated data')

dat <- data.frame(tests$gen_data(Nobs=400, nsubs1=10, nsubs2=20, lam1=.5, lam2=.7, rho=1))

test_that("between isc", {
  tests$perm_test(tests$btwn_isc, R = 1000, dat, upper=TRUE)
})

test_that("", {
  tests$perm_test(tests$btwn_sub_ttl, R= 1000, dat, upper=TRUE)
})

test_that("cfa", {
  tests$cfa_param_test(dat)
})

