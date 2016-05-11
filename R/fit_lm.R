group_cols <- function(groups, cols){
  if (inherits(groups, 'list')) groups            # list of grp: c(colnames ...)
  else sapply(groups, grep, cols, simplify=FALSE) # get from c(grp=grep_string ...)
}

makeTemplates <- function(dat, groups, retDesign=FALSE, error_var=NULL){
  cols <- colnames(dat)
  n <- length(groups)
  if (keep_diag <- !is.null(error_var))
    error_var <- group_cols(error_var, cols)

  indx <- group_cols(groups, cols)

  grp_names <- names(indx)
  
  # [n_groups x n_groups x k_meas x k_meas] matrix for matching
  # templates on the correlation matrix (last two dims) to group pairs
  # e.g. M['A', 'A',,] indexes the correlations within the A group
  M <- array(dim=c(n,n, ncol(dat), ncol(dat)), dimnames=list(grp_names, grp_names, cols, cols))
  
  # unique(unlist(indx))
  nMeas <- ncol(dat)
  desMat <- matrix(ncol = n*(n-1)/2 + n + length(unique(error_var)), 
                   nrow = nMeas*(nMeas-1)/2 + keep_diag*nMeas)
  desNames <- c()
  
  # fill lower diagonal
  # Make templates to index correlations for each group pair
  cycle <- 1
  for (ii in 1:n) {
    for (jj in ii:n) {
      # template matrix
      template <- matrix(FALSE, ncol=ncol(dat), nrow=ncol(dat), dimnames = list(cols, cols))
      template[indx[[ii]], indx[[jj]]] <- TRUE
      template[indx[[jj]], indx[[ii]]] <- TRUE   # symmetry
      M[ii, jj,,] <- template
      M[jj, ii,,] <- template  # symmetry
      
      # design mat
      desMat[,cycle] <- as.integer(template[lower.tri(template, diag=keep_diag)])
      desNames[cycle] <- paste0(grp_names[ii], 'x', grp_names[jj])
      cycle <- cycle + 1
    }
  }
  # If error_var not NULL, create a column in the design matrix
  # for each unique entry
  err_M <- array(dim=c(length(error_var), c(dim(M)[3:4])),
                  dimnames = list(names(error_var), cols, cols))
  for (key in names(error_var)) {
    template <- matrix(FALSE, ncol=ncol(dat), nrow=ncol(dat), dimnames = list(cols, cols))
    for (jj in error_var[[key]]) template[jj, jj] <- TRUE      # single diag entry
    # add error template
    err_M[key,,] <- template
    # flatten template into design matrix column
    desMat[,cycle] <- as.integer(template[lower.tri(template, diag=keep_diag)])
    desNames[cycle] <- key
    cycle <- cycle + 1
  }
  colnames(desMat) <- desNames
  
  if (retDesign) desMat
  else list(M=M, err_M=err_M)
}

coef.lm.mat <- function(fit){
  coefs <- fit$coefficients
  group_names <- fit$group_names
  
  nameMat <- t(outer(group_names, group_names, function(a,b) paste0(a, 'x', b)))
  indx <- nameMat[lower.tri(nameMat, TRUE)]
  
  M <- matrix(NA, ncol=length(group_names), nrow=length(group_names), dimnames=list(group_names, group_names))
  M[lower.tri(M, TRUE)] <- coefs[indx]
  M
}

fitLM <- function(dat, groups, is_cor = FALSE, error_var=NULL, drop_cross=c()){
  fullDesMat <- makeTemplates(dat, groups, error_var, retDesign=TRUE)
  desMat <- fullDesMat[,!colnames(fullDesMat) %in% drop_cross]
  
  cors <- if(is_cor) dat else cor(dat)
  y <- cors[lower.tri(cors, diag=!is.null(error_var))]
  fit <- lm.fit(desMat, y)
  fit$group_names <- names(groups)
  structure(fit, class=c('lm.mat'))
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
