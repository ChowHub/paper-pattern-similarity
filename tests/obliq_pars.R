library(plyr)

cross_pars <- expand.grid(
  A=seq(.2, .8, by=.3), 
  B=seq(.2, .8, by=.3), 
  C=seq(.2, .8, by=.3),
  rho_AB=seq(-1,1, by=.5),
  rho_AC=seq(-1,1, by=.5)
)

most_pars <- subset(cross_pars, (A >= B & B >= C) & (rho_AB >= rho_AC))

# Generate third latent variable correlation, 
# so latents will be postive semi-definite
pars <- adply(most_pars, 1, function(row) {
  C <- diag(3)
  C[2,1] <- row[,'rho_AB']
  C[3,1] <- row[,'rho_AC']
  utri <- upper.tri(C)
  to_keep <- c()
  for (entry in seq(-1,1, .25)) {
    C[3,2] <- entry
    C[utri] <- t(C)[utri]
    if (all(eigen(C)$values > 0)) {
      to_keep <- c(to_keep, entry)
    }
  }
  data.frame(rho_BC = to_keep)
})
