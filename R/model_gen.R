library(GetoptLong)
library(lavaan)

print(search())
#qq.options(code.pattern = "\\{\\{CODE\\}\\}")
qq.options(code.pattern = "@\\{CODE\\}")

MUSTACHE <- "\\{\\{(.*?)\\}\\}"

rm_mustache <- function(s){
  gsub(MUSTACHE, "\\1", s)
}

uniq_mustache <- function(s){
  matches <- unlist(regmatches(s, gregexpr(MUSTACHE, s) ))
  rm_mustache(unique(matches))
}

create_lams <- function(ltr, n, pref='lam_', same=FALSE, mustache=TRUE) {
  o_stache <- if (mustache) "{{" else ""
  c_stache <- if (mustache) "}}" else ""
  if (!same) paste0(o_stache, pref, ltr, 1:n, c_stache, '*', ltr, 1:n)
  else paste0(o_stache, pref, ltr, c_stache, '*', ltr, 1:n)
}

f_shared <- function(ltr1, n1, ltr2, n2) {
  # adds a shared factor between two groups for the bifactor model
  # this factor has two parameters, one for each group, which are constrainted
  # to have the same magnitude. The first parameter is constrained greater than
  # 0 to avoid issues with sign switching between the two.
  fname <- paste0("shared_", ltr1, 'x', ltr2)
  f_pref <- paste(fname,"", sep='_')
  measures <- c(create_lams(ltr1, n1, pref=f_pref, same=TRUE), 
                create_lams(ltr2, n2, pref=f_pref, same=TRUE))
  mod_meas <- paste0(fname, " =~ ", paste(measures, collapse=" + "))
  
  constraints <- qq("
  {{@{fname}_@{ltr1}}} == abs({{@{fname}_@{ltr2}}})

  {{@{fname}_@{ltr1}}} > 0
  ")
  
  paste(mod_meas, constraints, sep="\n")
}

gen_data <- function(type, ulams, n_meas, M_shared, ret_template=FALSE, n_obs=NULL, ...){
  # TODO: if ulams is a list, add method to use different lambdas
  
  # set names for shared matrix
  if (is.null(names(n_meas))) names(n_meas) <- names(ulams)
  if (is.null(dimnames(M_shared))) dimnames(M_shared) <- list(names(ulams), names(ulams))
  
  # create measurement model --------------------------------------------------
  meas_list <- sapply(
    1:length(ulams), 
    function(ii, ...) create_lams(names(ulams)[ii], n_meas[ii], ...),
    same=TRUE)
  colnames(meas_list) <- names(ulams)
  mod_uniq <- do.call(isctools::build.model, data.frame(meas_list))$oblique
  # get all cross names
  
  # create either bifactor or oblique factor structures -----------------------
  if (type == 'bifactor') {
    mod_shared <- apply(combn(names(ulams), 2), 2, 
      function(m) f_shared(m[1], n_meas[m[1]], m[2], n_meas[m[2]])
    )
    temp <- "shared_@{col[1]}x@{col[2]}_@{col}"
  }
  if (type == 'oblique') {
    mod_shared <- apply(combn(names(ulams), 2), 2, 
                      function(s) qq("@{s[1]} ~~ {{rho_@{s[1]}x@{s[2]}}}*@{s[2]}")
    )
    temp <- "rho_@{col[1]}x@{col[2]}"
  }
  
  # final model to output
  mod <- paste(mod_uniq, paste(mod_shared, collapse="\n"), sep="\n")
  
  # fill in template with parameters
  args <- list()
  comb_lam <- combn(names(ulams), 2)
  for (ii in 1:ncol(comb_lam)){
    # add arguments for shared loadings or factor correlations
    col <- comb_lam[,ii]
    shared_lams <- qq(temp, collapse=FALSE)
    loading <- M_shared[col[1], col[2]]
    args[shared_lams] <- if (type == 'bifactor') loading * c(sign(loading), 1) else loading
  }
  # add measure loadings
  args[paste0("lam_", names(ulams))] <- ulams
  
  # either return simulated data or model string
  if (!ret_template | !is.null(n_obs)) {
    mod <- qq(mod, envir=args, code.pattern = "\\{\\{CODE\\}\\}")
    if (!is.null(n_obs)){
      lavaan::simulateData(mod, standardized=TRUE, sample.nobs = n_obs, ...)
    }
    else mod
  }
  else list(mod=mod, args=args)
}

rotLamPos <- function(coefs, lam){
  if (sign(coefs[paste0('lam_', lam)]) == -1) {
    indx <- grep(lam, names(coefs))
    coefs[indx] <- -1 * coefs[indx]
    
  }
  coefs
}

simObliqCoefs <- function(ulams, M_shared){
  template <- gen_data('oblique', ulams, rep(5, 3), M_shared, 
                           ret_template=TRUE)
  
  d <- gen_data('oblique', ulams, rep(5, 3), M_shared, 
                    ret_template=FALSE, n_obs=500, empirical=TRUE)
  
  fit <- lavaan::cfa(rm_mustache(template$mod), data=d, std.ov=TRUE, std.lv=TRUE)
  args <- unlist(template$args)
  coefs <- coef(fit)[names(args)]
  for (lam in names(ulams)) {
    coefs <- rotLamPos(coefs, lam)
  }
  
  args[paste0('lam_', LETTERS[1:3])] <- abs(args[paste0('lam_', LETTERS[1:3])])
  list(pars=data.frame(args, coefs), data=d)
}