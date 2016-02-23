makeTemplates <- function(dat, groups, retDesign=FALSE){
  cols <- colnames(dat)
  n <- length(groups)
  M <- array(dim=c(n,n, ncol(dat), ncol(dat)), dimnames=list(groups, groups, cols, cols))
  indx <- sapply(groups, grep, names(dat), simplify=FALSE)
  
  # unique(unlist(indx))
  nMeas <- ncol(dat)
  desMat <- matrix(ncol = n*(n-1)/2 + n, nrow = nMeas*(nMeas-1)/2)
  desNames <- c()
  
  # fill lower diagonal
  
  cycle <- 1
  for (ii in 1:n) {
    for (jj in ii:n) {
      # template matrix
      template <- matrix(FALSE, ncol=ncol(dat), nrow=ncol(dat))
      template[indx[[ii]], indx[[jj]]] <- TRUE
      template[indx[[jj]], indx[[ii]]] <- TRUE   # symmetry
      M[ii, jj,,] <- template
      M[jj, ii,,] <- template  # symmetry
      
      # design mat
      desMat[,cycle] <- as.integer(template[lower.tri(template)])
      desNames[cycle] <- paste0(groups[ii], '_x_', groups[jj])
      cycle <- cycle + 1
    }
  }
  colnames(desMat) <- desNames
  
  if (retDesign) desMat
  else M
}

coef.lm.mat <- function(fit){
  coefs <- fit$coefficients
  groups <- fit$groups
  
  nameMat <- t(outer(groups, groups, function(a,b) paste0(a, '_x_', b)))
  indx <- nameMat[lower.tri(nameMat, TRUE)]
  
  M <- matrix(NA, ncol=length(groups), nrow=length(groups), dimnames=list(groups, groups))
  M[lower.tri(M, TRUE)] <- coefs[indx]
  M
}

fitLM <- function(dat, groups){
  desMat <- makeTemplates(dat, groups, retDesign=TRUE)
  cors <- cor(dat)
  y <- cors[lower.tri(cors)]
  fit <- lm.fit(desMat, y)
  fit$groups <- groups
  structure(fit, class=c('lm.mat'))
}

