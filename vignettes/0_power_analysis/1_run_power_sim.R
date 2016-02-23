# Run power simulation. Save results as csv.

library(importr)
tests = import('../../R/model_tests.R')

library(methods)
library(magrittr)
library(plyr)
library(reshape)
library(doMC)
doMC::registerDoMC(cores=parallel::detectCores()-1)

if (!interactive()) {
  # not in interactive mode, use command line options
  # args are
  # 1,2: outfile, trial pars file
  # 3  : number of repetitions
  args = commandArgs(trailingOnly=TRUE)
  OUTFILE = args[1]
  trialPars = read.csv(args[2])
  trialPars = trialPars[rep(rownames(trialPars), each=as.integer(args[3])),]
} else {
  OUTFILE = 'out/pvals-0.csv'
  # Create trial parameters
  Nrep = 1
  pars = expand.grid(Nobs=400, nsubs1=10, nsubs2=c(5, 8, 22), 
                     lam1=c(.25,.35,.5), lam2=c(.25, .35,.5), rho=c(1))
  pars = subset(pars, lam1 >= lam2)
  trialPars = pars[rep(rownames(pars), each=Nrep),]
}

# Simulation Funcs ------------------------------------------------------------

# data -> pvals
get_pvals = . %>% {
  c(btwn_isc = tests$perm_test(tests$btwn_isc, R=1000, ., upper=TRUE),
    #btwn_sub_ttl = tests$perm_test(tests$btwn_sub_ttl, R=1000, ., upper=TRUE),
    cfa = tests$cfa_param_test(.),
    btwn_lm = tests$perm_test(tests$btwn_lm, R=1000, ., upper=TRUE)
  )
}

# par data.frame row -> pars w/ p-values
sim = . %>% do.call(tests$gen_data, .) %>%
             { c(attr(., 'pars'), get_pvals(.)) }
  
# Run simulation --------------------------------------------------------------
a <- proc.time()
out = mdply(trialPars, function(...) sim(data.frame(...)),
            .parallel=TRUE)
proc.time() - a

mout = melt(out, id.vars = names(trialPars))

write.csv(mout, OUTFILE)
