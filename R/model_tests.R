library(isctools)
library(MASS)
library(lavaan)
isctools <- loadNamespace("isctools")
lavaan <- loadNamespace("lavaan")

fit_cfa = function(dat, f_labs=c('f1', 'f2')){
  groups = group_items(colnames(dat), f_labs)
  mod = isctools::build.model(f1 = groups[[1]], f2 = groups[[2]], type=c('oblique', '1f'))
  list(f2 = cfa(mod$oblique, dat, std.ov=TRUE, std.lv=TRUE),
       f1 = cfa(mod$f1, dat, std.ov=TRUE, std.lv=TRUE)
  )
}

# Parameter ------------------------------------------------------------
btwn_isc = function(dat, f_labs=c('f1', 'f2')){
  g1 = dat[,grepl(f_labs[1], colnames(dat))]
  g2 = dat[,grepl(f_labs[2], colnames(dat))]
  
  within = (mean(item.ic(g1)) + mean(item.ic(g2))) / 2
  between = mean(item.ic(g1, g2))
  within - between
}

btwn_sub_ttl = function(dat, f_labs=c('f1', 'f2')){
  g1 = dat[,grepl(f_labs[1], colnames(dat))]
  g2 = dat[,grepl(f_labs[2], colnames(dat))]
  
  ttl_g1 = rowSums(g1)
  ttl_g2 = rowSums(g2)
  
  sub_ttl = function(sub, ttl) cor(sub, ttl-sub)
  in_g1 = apply(g1, 2, sub_ttl, ttl=ttl_g1)
  in_g2 = apply(g2, 2, sub_ttl, ttl=ttl_g2)
  within = mean(c(in_g1, in_g2))
  #within = (mean(item.rel(g1)) + mean(item.rel(g2)) ) / 2
  between = mean(c(cor(g1, ttl_g2), cor(g2, ttl_g1)))
  #between = (mean(item.rel(g1, M2=g2)) + mean(item.rel(g2, M2=g1)) ) / 2
  within - between
}

btwn_cfa = function(dat, f_labs=c('f1', 'f2')){
  fit = fit.isc(dat, f.names=f_labs)
  group.cor(fit$fit$f2)
}

btwn_mantel = function(dat, f_labs=c('f1', 'f2')){
  # empirical correlation matrix
  cdat = cor(dat)
  # template correlation matrix
  indx = grepl(f_labs[1], colnames(dat))
  template = abs(outer(indx, indx, '+') - 1) # ones for within group, 0 otherwise
  # pearson's correlation between flattened lower triangles of both
  l_tri = lower.tri(cdat)
  cor(cdat[l_tri], template[l_tri])
}

# NHST ------------------------------------------------------------------------
perm_test = function(f, R, dat, upper=TRUE, retNull=FALSE){
  null_dist = replicate(R, {
    colnames(dat) = sample(colnames(dat),replace = FALSE)
    f(dat)
  })
  
  if (retNull) return(null_dist)
  
  if (upper) mean(null_dist > f(dat))
  else mean(abs(null_dist) > abs(f(dat)))
}

cfa_chisqr_test = function(dat, f_labs=c('f1', 'f2')){
  fit.isc(data.frame(dat), f.names=f_labs)$diff[2,'Pr(>Chisq)']
}

cfa_param_test = function(dat, f_labs=c('f1', 'f2')){
  fit = fit_cfa(dat, f_labs)
  tryCatch(
    lavaan::anova(fit$f1, fit$f2)[2, "Pr(>Chisq)"],
    error = function(e) NA
  )
}

# Data generation -------------------------------------------------------------
gen_data = function(Nobs, nsubs1, nsubs2, ...){
  pars = unlist(c(as.list(environment()), list(...)))
  
  # generate data frame
  start = nsubs1 + 1
  end = start + nsubs2 - 1
  C = gen.corrmat(1:nsubs1, start:end, ...)
  dat = data.frame(mvrnorm(Nobs, rep(0, nsubs1+nsubs2), C))
  
  # name columns, store pars as attribute
  names(dat) = paste0(c(rep('f1', nsubs1), rep('f2', nsubs2)), '_', 1:(nsubs1+nsubs2))
  attr(dat, 'pars') <- pars
  dat
}

# Model fitting ---------------------------------------------------------------

fit_nhst <- function(dat){
  # tests should return a single p-value value
  data.frame(
    btwn_isc = tests$perm_test(tests$btwn_isc, Nperms, dat, upper=TRUE),
    btwn_sub_ttl = tests$perm_test(tests$btwn_sub_ttl, Nperms, dat, upper=TRUE),
    cfa_param_test = tests$cfa_param_test(dat)
  )
}

# Simulation ------------------------------------------------------------------
sim_power = function(dat, fit, pars=NULL){
  
  
}


#cfa_boot_test = function(dat, R, f_labs=c('f1', 'f2')){
#  fit = fit_cfa(dat, f_labs)
#  bootstrapLavaan(fit, R, FUN = function(mod) group.cor(mod)$est)
#}

#C = isctools::gen.corrmat(1:10, 11:20, .3, .7, .8)
#dat = mvrnorm(400, rep(0, 20), C)
#colnames(dat) = paste0(rep(c('f1', 'f2'), each=10), '_', 1:20)

#group_items <- function(i_names, f.names){
#  list(f1 = grep(f.names[1], i_names, value=TRUE),
#       f2 = grep(f.names[2], i_names, value=TRUE))
#}

# Mantel is equivalent to within vs between contrast --------------------------------
#set.seed(100)
#perm_test(btwn_isc, 10000, dat)
#set.seed(100)
#perm_test(btwn_mantel, 10000, dat)


