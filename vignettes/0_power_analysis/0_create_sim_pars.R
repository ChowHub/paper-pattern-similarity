# Create a csv of parameters for power simulations. Optionally split into batches of parameters.

# Parse command line, or set defaults
if(!interactive()) {
  args = commandArgs(TRUE)
  OUTROOT = args[1]
  nbatches = as.integer(args[2])
} else {
  OUTROOT = 'out/pars-'
  nbatches = 0
}

# Create parameters
pars = expand.grid(Nobs=400, nsubs1=10, nsubs2=c(5, 8, 22), 
                   lam1=c(.45,.65,.85), lam2=c(.45, .65,.85), rho=c(1))
pars = subset(pars, lam1 >= lam2)
trialPars = pars

# Split into batches
if (!nbatches) write.csv(trialPars, file=paste0(OUTROOT, nbatches, '.csv'), row.names=FALSE)
if (nbatches) {
  batches = split(trialPars, rep(1:batches, length.out=nbatches))
  for (ii in 1:length(batches)) {
    write.csv(batches[[ii]], file=paste0(OUTROOT, ii, '.csv'), row.names=FALSE)
  }
}
