tests = (function(){source('model_tests.R', local = tmp <- new.env()); tmp})()

sim_power = function(Nobs, nsubs1, nsubs2, lam1, lam2, rho, Nperms, alpha_lvl=NULL){
  dat = data.frame(tests$gen_data(Nobs, nsubs1, nsubs2, lam1, lam2, rho))
  data.frame(
    #btwn_isc = tests$perm_test(tests$btwn_isc, Nperms, dat, upper=TRUE),
    btwn_sub_ttl = tests$perm_test(tests$btwn_sub_ttl, Nperms, dat, upper=TRUE)
    #cfa_param_test = tests$cfa_param_test(dat)
  )
}

library(plyr)
library(doMC)
doMC::registerDoMC(cores=parallel::detectCores())

Nrep = 250
pars = expand.grid(Nobs=400, nsubs1=10, nsubs2=c(5, 8, 22), 
                   lam1=c(.25,.35,.5), lam2=c(.25, .35,.5), rho=c(1), Nperms=1000)
trialPars = pars[rep(rownames(pars), each=Nrep),]
dim(trialPars)

out = ldply(1:nrow(trialPars), function(ii){
  cbind(do.call(sim_power, trialPars[ii,]), trialPars[ii,])
}, .parallel=TRUE)

library(reshape)
library(ggplot2)
mout = melt(out, measure.vars = c("btwn_isc", "btwn_sub_ttl", "cfa_param_test"))
indx_gt = with(mout, lam1 > lam2)
mout[indx_gt ,c('lam1', 'lam2')] = mout[indx_gt, c('lam2', 'lam1')]

false_pos = ddply(mout, .(Nobs, nsubs1, nsubs2, lam1, lam2, rho, Nperms, variable), summarize, 
      fr = mean(value < .05, na.rm=TRUE))

ggplot(false_pos, aes(lam1, fr, color=variable, group=variable)) + 
  geom_point() +
  geom_line(size=.85) +
  facet_grid(lam2 ~ .) + coord_cartesian(ylim=c(-.05, 1.05)) +
  theme_bw(base_size=20) + ylab("False Positive Rate") + 
  ggtitle("Effect of Internal Consistency on Between Group Tests")


# Sanity checks ------------------------------------
#ptm = proc.time()
#out = ldply(1:5, function(ii) { 
#  sim_power(400, 10, 10, lam1=.5, lam2=.6, rho=1, Nperms=1000)
#}, .parallel=TRUE, .inform = TRUE)
#proc.time() - ptm

#dat = gen_data(400, 10, 10, lam1=.5, lam2=.6, rho=1)
#head(dat)
#tests$perm_test(btwn_isc, 1000, dat, upper=FALSE, retNull=TRUE)
#plot(density(null))
#btwn_isc(dat)
#mean(null > btwn_isc(dat))
