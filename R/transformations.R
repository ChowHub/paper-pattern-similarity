# Linear Model ----------------------------------------------------------------
lm_to_bif <- function(beta){
  # requires betas to be in lower triangle at least
  # constraints: check parameter ranges
  bif <- beta
  
  diag(bif) <- sqrt(2*diag(beta) - rowSums(beta))

  return(bif)
}

lm_to_obliq <- function(beta){
  # lambdas
  obliq <- diag(ncol(beta))
  diag(obliq) <- sqrt(diag(beta))
  # rhos
  xlam <- diag(obliq) %*% t(diag(obliq))
  indx <- lower.tri(beta)
  obliq[indx] <- beta[indx]/xlam[indx]
  
  obliq[upper.tri(obliq)] <- t(obliq)[upper.tri(obliq)]   # fill-in symmetry
  return(obliq)
}

# Oblique ---------------------------------------------------------------------
obliq_to_ <- function(obliq, type){
  # note that correlation space spanned by the bifactor 
  # is a subset of the oblique (unless the w/in group loadings can be complex)

  xlam <- diag(obliq) %*% t(diag(obliq))      # matrix w/ cross lambdas
  # shared_ab = lam_a*lam_b*rho_ab
  out <- xlam * obliq
  diag(out) <- 0
  
  if (type == 'bifactor'){
    # uniq_a^2 = lam_a^2 - sum of co-variance through shared factors
    diag(out) <- diag(obliq)^2 - (rowSums(abs(out)))
    # could stop here if using a linear model with design matrix like bifactor
    diag(out) <- sqrt(diag(out))
  }
  if (type == 'lm'){
    diag(out) <- diag(xlam)
  }
  out
}

obliq_to_lm <- function(obliq) obliq_to_(obliq, 'lm')

obliq_to_bif <- function(obliq) obliq_to_(obliq, 'bifactor')

# Bifactor --------------------------------------------------------------------
bif_to_lm <- function(bif){
  # bifactor parameters in lower triangle at least
  # no constraints
  beta <- bif
  
  diag(bif) <- diag(bif)^2     # loadings are on diag, so need to square
  diag(beta) <- rowSums(bif)   # within group variance is sum of co-variances through factors
  
  return(beta)
}


bif_to_obliq <- function(bif){
  obliq <- bif
  
  # solve for lambdas ----
  # lam_a = sqrt( uniq_a^2 - sum abs co-variances through shared factors )
  # it is absolute because the off-diagonal components may be negative, since they are
  # composed of two subloadings that are constrained to have the same magnitude
  
  diag(bif) <- diag(bif)^2    # loadings are on diag, so need to square
  diag(obliq) <- sqrt(rowSums(bif))
  
  # solve for rhos ----
  xlam <- diag(obliq) %*% t(diag(obliq))                  # matrix w/ cross lambdas
  indx <- lower.tri(obliq)
  obliq[indx] <- bif[indx] / xlam[indx]                   # rho_ab = bif_ab / lam_a*lam_b
  
  obliq[upper.tri(obliq)] <- t(obliq)[upper.tri(obliq)]   # fill-in symmetry
  return(obliq)
}
