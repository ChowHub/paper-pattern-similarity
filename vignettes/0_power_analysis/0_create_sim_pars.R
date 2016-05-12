# Create a csv of parameters for power simulations. Optionally split into batches of parameters.
OUTNAME <- if (!interactive()) commandArgs(TRUE)[1] else 'test_pars.csv'

# Create parameters
loadings <- c(.25, .3, .45, .65, .85)
pars = expand.grid(Nobs=c(100, 400), nsubs1=10, nsubs2=c(5, 10, 22), 
                   lam1=loadings, lam2=loadings, rho=c(1))
pars = subset(pars, lam1 >= lam2)

write.csv(pars, file = OUTNAME, row.names=FALSE)
