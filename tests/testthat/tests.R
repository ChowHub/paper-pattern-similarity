library(testthat)
eq = (function(){source('model_formulas.R', local=tmp<-new.env());tmp})()

test_that("IC subject-total",function(){
  # calculates sub_ttl correlation for each lambda
  expect_equal(length(eq$isc_sub_ttl(c(.5,.6))), 2)
  
  # with 2 lamdas it is their pairwise correlation
  expect_equal(eq$ic_sub_ttl(c(.5, .6)), c(.3, .3))
  
  # case where first two entries should have same sub_ttl corr, 
  # but third should be different
  lams = c(.5,.5,.6)
  expect_equal(eq$ic_sub_ttl(lams), 
               c(lams[1:2]*eq$reliability(c(.5,.6)), lams[3]*eq$reliability(c(.5,.5)))
  )
})

test_that("IC isc", function(){
  expect_equal(eq$ic_isc(c(.5, .6)), c(.3))
  
  # returns pairwise as 1x2, 1x3, 1x4, ..., 2x3, 2x4, ...
  expect_equal(eq$ic_isc(c(.5, .6, .7)), c(.3, .35, .42))
})

test_that("BG isc", function(){
  # returns a matrix
  expect_equal(class(eq$bg_isc(.2, .3, .9)), "matrix")
               
  lam1 = c(.2, .3)
  lam2 = c(.4, .5)
  M = matrix(c(lam1*lam2[1], lam1*lam2[2]), ncol=2)
  # when rho is 1
  expect_equal(eq$bg_isc(lam1, lam2, 1), 
               M)
  # when rho is .5
  rho = .5
  expect_equal(eq$bg_isc(c(.2, .3), c(.4, .5), rho),
               M * rho)
})
test_that("BG subject-total", function(){
  # between-groups with rho set to 1 is the same as w/in group for new subject
  bg_lams = c(.3, .4, .5)
  expect_equal(eq$bg_sub_ttl(bg_lams, bg_lams[2:3], 1)[1], eq$ic_sub_ttl(bg_lams)[1])
  
  # even with the same IC, BG subject-total correlation depends on sample size used for composite
  fives = rep(.5, 10)
  expect_less_than(
    mean(eq$bg_sub_ttl(fives, fives[1:5], 1)),  # fewer used in composite
    mean(eq$bg_sub_ttl(fives[1:5], fives, 1))
  )
})
test_that("BG total-total", function(){
  # depends on sample size
  expect_less_than(eq$bg_ttl_ttl(rep(.5, 10), rep(.5, 10), 1), 
                   eq$bg_ttl_ttl(rep(.5, 5 ), rep(.5, 10), 1)
  )
  # depends on rho
  fives = rep(.5, 10)
  expect_less_than(eq$bg_ttl_ttl(fives, fives, .8),
                   eq$bg_ttl_ttl(fives, fives, .9))
})

