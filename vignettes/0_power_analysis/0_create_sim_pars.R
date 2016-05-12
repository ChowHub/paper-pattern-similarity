# Create a csv of parameters for power simulations. Optionally split into batches of parameters.
OUTNAME <- if (!interactive()) commandArgs(TRUE)[1] else 'test_pars.csv'

# Create parameters
pars = expand.grid(Nobs=400, nsubs1=10, nsubs2=c(5, 8, 22), 
                   lam1=c(.45,.65,.85), lam2=c(.45, .65,.85), rho=c(1))
pars = subset(pars, lam1 >= lam2)

write.csv(pars, file = OUTNAME, row.names=FALSE)
