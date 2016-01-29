#' Demonstrating bias in measuring internal consistency with classic ISC methods
#' =============================================================================
# Load necessary libraries / functions
library(importr)
library(ggplot2)
library(grid)
eq = import('../R/model_formulas.R')

# Parameters
Nsubs = 4:30
pars = expand.grid(lams1 = .6, lams2 = .3, rho=1, Nsubs = Nsubs)

head(pars)
result = do.call(rbind,
  apply(pars, 1, function(row) 
    eq$ttl_ttl_bias(rep(row['lams1'], row['Nsubs']), rep(row['lams2'], row['Nsubs']), 1)
  )
)


# Plot
head(result)
library(reshape)
mres = melt(result,measure.vars=c('est_lam1', 'est_lam2', 'bg_sub_ttlA', 'bg_sub_ttlB'))
mres$group = ifelse(grepl("lam1|ttlB", mres$variable), "A", "B")
mres$comparison = ifelse(grepl("est", mres$variable), "within", "between")

head(mres)
p = ggplot(mres, aes(color=group, linetype=comparison, group=variable)) + 
  geom_line(aes(N1, value), size=1.3) +
  coord_cartesian(xlim=c(3, 25), ylim=c(0, 1)) +
  scale_linetype_manual(name="comparison",
                          breaks=c("within", "between"),
                          values=c("dashed", "solid")) +
  scale_y_continuous(breaks=seq(.1, 1, .2)) +
  theme_bw(base_size=25) + 
  theme(legend.title = element_text(size=25),
        legend.key.size = unit(.4, "inches")) +
  ggtitle("Subject-total correlation") +
  ylab("Item-total correlation") + xlab("Number of Subjects per Group")
p

ggsave("../paper/figs/sub_total_bias.png", p, width=10, height = 10 / 1.75)


result
