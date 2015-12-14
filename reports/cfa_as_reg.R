library(MASS)
C = matrix(ncol=9, nrow=9)
mod = "
A =~ lam_A*a1 + lam_A*a2 + lam_A*a3
B =~ lam_B*b1 + lam_B*b2 + lam_B*b3
C =~ lam_C*c1 + lam_C*c2 + lam_C*c3

A ~~ .5*B
B ~~ 0*C
A ~~ 0*C

lam_A == lam_B
lam_B == lam_C
"

vars = paste0(rep(c("a", "b", "c"), each=3), 1:3)

mod.int = paste("D =~ ", paste0("alph*", vars, collapse=" + "))
resid = paste0(vars, "~~ res*", vars, collapse="\n")
mod = paste(mod, mod.int, resid, sep="\n")


mod.1f = paste(mod.int, resid, sep="\n")

dat = simulateData(gsub("alph", .1, 
                        gsub("lam_[ABC]", ".6", mod)),
                   model.type="cfa", standardized=TRUE, orthogonal=TRUE,
                   sample.nobs=5000, std.lv = TRUE, std.ov=TRUE)
summary(cfa(mod, dat, std.lv=TRUE, std.ov=TRUE,orthogonal=TRUE))

ssd = function(fit){
  sum(resid(fit)$cov^2)
}

(ssd(fit_base) - ssd(fit) ) / ssd(fit_base)
anova(fit, fit_base)

fit = cfa(mod, dat, std.lv=TRUE, std.ov=TRUE, orthogonal=TRUE, estimator="ULS")
fit_base = cfa(mod.1f, dat, std.lv=TRUE, std.ov=TRUE, estimator="ULS")
fitted(fit)

cor(dat) - fitted(fit)$cov

anova(fit_base, fit)

cdat = cor(dat)
flat_cors = c(cdat[lower.tri(cdat)])
C = matrix(ncol=9, nrow=9)
C[1:3, 1:3] = lam_A = 1
C[4:6, 4:6] = lam_B = 1
C[7:9, 7:9] = lam_C = 1
C[4:6, 1:3] = .5
C[7:9, 1:3] = 0
C[7:9, 4:6] = 0

template = C[lower.tri(C)]
summary(lm(flat_cors ~ template))
template

