# There and back again tests
library(importr)
library(testthat)
eq <- import('../../R/transformations.R')
gen <- import('../../R/model_gen.R')
pars <- import('../obliq_pars.R')

context('Transformation tests: bifactor there and back')

M_bif <- matrix(nrow=3, ncol=3)
diag(M_bif) <- c(.2, .3, .4)
M_bif[lower.tri(M_bif)] <- c(.5, .5, .5)^2
M_bif[upper.tri(M_bif)] <- t(M_bif)[upper.tri(M_bif)]

test_that("bif -> obliq", {
  obliq <- eq$bif_to_obliq(M_bif)
  rebif <- eq$obliq_to_bif(obliq)
  
  expect_equal(M_bif, rebif)
})

test_that("bif -> lm", {
  betas <- eq$bif_to_lm(M_bif)
  rebif <- eq$lm_to_bif(betas)
  
  expect_equal(M_bif, rebif)
})

context("Transformation tests: from obliq and lm pars")

obliqMat <- function(row) {
  M <- diag(3)
  M[lower.tri(M, TRUE)] <- unlist(row[c('A', 'rho_AB', 'rho_AC', 'B', 'rho_BC', 'C')])
  M
}

p <- pars$pars
for (row_num in 1:nrow(p)){
  test_that("obliq -> lm -> obliq", {
    M <- obliqMat(p[row_num,])
    M[upper.tri(M)] <- t(M)[upper.tri(M)]
    
    beta <- eq$obliq_to_lm(M)
    obliq <- eq$lm_to_obliq(beta)
    
    expect_equal(M, obliq)
  })
}


context("valid correlation matrices that do not work")
# positive-semi definite with all negative entries
