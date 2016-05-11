library(plyr)
library(lavaan)
library(importr)

M <- loadNamespace('lavtools')
#M <- import('lav_graph.R')
#out <- gen_data(list(A=c(.3, .5, .7), B=c(.6, .6, .7)), NULL, matrix(.5, nrow=2, ncol=2))

# Core data model functions ---------------------------------------------------

create_lams <- function(ltr, n, value="", pref="lam", same=FALSE) {
  # n is either the total number of measures, or an array of names
  measures <- 
    if (is.numeric(n)) paste0(ltr, 1:n)
    else n
  labels <- paste(pref, if (same) ltr else measures, sep='_')
  M$make_edge(ltr, '=~', measures, labels, value)  
}

gen_data <- function(ulams, n_meas, M_shared, rand_int=NULL){
  # if ulams are given in a list, 
  if (inherits(ulams, 'list') & is.null(n_meas))
    n_meas <- sapply(ulams, length)
  # name each n_meas entry after its corresponding ulams entry
  if (is.null(names(n_meas))) 
    names(n_meas) <- names(ulams)
  # set latent variable names in shared matrix
  if (is.null(dimnames(M_shared))) 
    dimnames(M_shared) <- list(names(ulams), names(ulams))
  
  # create measurement model --------------------------------------------------
  meas_block <- ldply(names(ulams), function(lv) 
    create_lams(lv, n_meas[lv], ulams[[lv]], same=!inherits(ulams, 'list'))
  )
  
  # create random intercept ---------------------------------------------------
  int_block <- 
  if (!is.null(rand_int)) {
    rbind(
      M$make_edge('int', '=~', meas_block$rhs, "rand_int", sqrt(rand_int)), # will label "rand_int"
      M$make_edge('int', '~~', names(ulams), "", 0)#,
      #M$make_edge('int', '~~', 'int', "rand_int", rand_int)
    )
  } else data.frame()
  
  # create either bifactor or oblique factor structures -----------------------
  shared_block <- adply(combn(colnames(M_shared), 2), 2, function(x) 
    M$make_edge(x[1], '~~', x[2], 
                paste0('rho_', x[1], 'x', x[2]), 
                M_shared[x[1], x[2]]
    ), .id = NULL)

  # final model to output
  mod_block <- rbind.fill(meas_block, int_block, shared_block)
  for (col in c('lhs', 'rhs', 'label')) 
    mod_block[,col] <- as.character(mod_block[,col])

  M$expand_mod(mod_block)  
}

rotLamPos <- function(coefs, lam){
  # parameters:
  #   coefs - an array of named coefficients. e.g. c("lam_A" = -.1)
  #   lam   - name of lamda to flip sign on. e.g. 'A'
  # transform parameters to make lambdas positive
  # necessary because the tau-equivalent model is sign-invariant
  if (sign(coefs[paste0('lam_', lam)]) == -1) {
    indx <- grep(lam, names(coefs))
    coefs[indx] <- -1 * coefs[indx]
  }
  coefs
}

# Misc data models --------------------------------------------------------

gen_rand_int <- function(..., dummy_lv='D'){
  # generate data model and then remove dummy latent
  # the dummy latent is used to create random_intercept only manifest vars
  out <- gen_data(...)
  M$remove_var(out$mod_block, dummy_lv, TRUE)
}

gen_resid <- function(mod_block, lhs, label, value){
  block <- rbind(
    mod_block,
    M$make_resid(lhs, label, value)
  )
  M$expand_mod(block)
}

# Sims ----------------------------------------------------------------------

simData <- function(mod, n_obs, ...){
  lavaan::simulateData(mod, standardized=TRUE, sample.nobs = n_obs, ...)
}

simObliqCoefs <- function(ulams, M_shared){
  # TODO, need to rewrite
  mod <- gen_data(ulams, rep(5,3), M_shared)

    d <- simData(mod$mod_inst, 500, empirical=TRUE)
  
  fit <- lavaan::cfa(mod$mod, data=d, std.ov=TRUE, std.lv=TRUE)
  args <- unlist(mod$args)
  coefs <- coef(fit)[names(args)]
  for (lam in names(ulams)) {
    coefs <- rotLamPos(coefs, lam)
  }
  
  args[paste0('lam_', LETTERS[1:3])] <- abs(args[paste0('lam_', LETTERS[1:3])])
  list(pars=data.frame(args, coefs), data=d)
}

# Standardized coefficients from lavaan fit object
coef_stand <- function(fit, col=c('std.lv', 'std.all')[1]){
  pars <- parameterEstimates(fit, standardize=TRUE) 
  value <- pars[,col]
  
  full_name <- with(pars, paste0(lhs, op, rhs))
  names(value) <- ifelse(pars$label != "", pars$label, full_name)
  value
}

unique_coef <- function(arr, ...) {
  sapply(split(arr, names(arr)), function(entry) {
    stopifnot( all.equal(entry, rep(entry[1], length(entry)), ...) )
    entry[[1]]
  }) 
}