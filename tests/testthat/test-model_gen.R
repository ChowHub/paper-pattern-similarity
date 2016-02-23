library(importr)
library(testthat)
library(lavaan)

gen <- import('../../R/model_gen.R')   # generating functions
obliq_pars <- import('../obliq_pars.R')      # parameters to test
# for testing fitting linear model -> obliq
fitLM <- import('../../R/fit_lm.R')
trans <- import('../../R/transformations.R')

# Run tests -------------------------------------------------------------------
# generate lams
# generate rho

pars <- obliq_pars$pars
a <- proc.time()
for (row_num in 1:nrow(pars)) {
  # generate parameters
  ulams <- unlist(pars[row_num, c('A', 'B', 'C')])
  M_shared <- diag(ulams)
  M_shared[lower.tri(M_shared)] <- unlist(pars[row_num, paste0('rho_', c('AB', 'AC', 'BC'))])
  M_shared[upper.tri(M_shared)] <- t(M_shared)[upper.tri(M_shared)]
  
  # simulate data,fit model. Returns initial parameters and those model learned
  simdat <- gen$simObliqCoefs(ulams=ulams, M_shared=M_shared)

  test_that("can recover oblique parameters", {
    pars <- simdat$pars
    expect_equal(pars$coefs, pars$args, tolerance=.01)
  })
  
  test_that("lm fit to corrmat -> oblique params", {
    fit <- fitLM$fitLM(simdat$data, LETTERS[1:3])
    obliq <- trans$lm_to_obliq(fitLM$coef.lm.mat(fit))
    expect_equal(obliq, M_shared)
  })
}
proc.time() -a


#test_that("can recover bifactor parameters", {
#  template <- gen$gen_data('bifactor', c(A=.3, B=.5, C=.7), rep(5, 3), matrix(-.25, ncol=3, nrow=3), 
#                           ret_template=TRUE)
#  start <- lavaanify(gen$gen_data('bifactor', c(A=1, B=1, C=1), rep(5, 3), matrix(-1, ncol=3, nrow=3),
#                           ret_template=FALSE))
#  start[start$lhs == start$rhs & start$op == "~~", "ustart"] = .5
#
#  d <- gen$gen_data('bifactor', c(A=.3, B=.5, C=.7), rep(5, 3), matrix(-.25, ncol=3, nrow=3), 
#                    ret_template=FALSE, n_obs=5000, empirical=TRUE)
#  
#  mod <- gen$rm_mustache(template$mod)
#  #mod <- gsub('shared_AxB_B', 'shared_AxB_A', mod)
#  #mod <- gsub("shared_AxB_A == abs\\(shared_AxB_A\\)", "", mod)
#  fit2 <- cfa(mod, data=d, std.ov=TRUE, std.lv=TRUE, orthogonal=TRUE, start=start)
#  args <- unlist(template$args)
#  expect_equal(coef(fit)[names(args)], args, tolerance=.001)
#  
#})