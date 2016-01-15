#' Demonstrating bias in measuring internal consistency with classic ISC methods
#' =============================================================================
# Load necessary libraries / functions
library(ggplot2)
library(grid)
eq = (function(){source('../scripts/model_formulas.R', local=tmp <- new.env()); tmp})()

# Parameters
group_lams = c(.3, .5, .7)
Nsubs = 2:100
pars = expand.grid(group_lams = group_lams, Nsubs = Nsubs)

# Calculate bias, rbind to dataframe
result = do.call(rbind,
  apply(pars, 1, function(row) 
    eq$ic_bias(rep(row['group_lams'], row['Nsubs'])))
)

# Plot
head(result)
result$sqrt_isc = sqrt(result$isc)
result$mean_lam = factor(result$mean_lam)

p = ggplot(result, aes(group=mean_lam, color=mean_lam)) + 
  geom_line(aes(N, sqrt_isc, linetype="isc"), size=1.3) +
  geom_line(aes(N, sub_ttl, linetype="subject-total"), size=1.3) + 
  coord_cartesian(xlim=c(3, 25)) +
  scale_color_discrete(name=bquote(lambda), breaks=c(.7, .5, .3)) +
  scale_linetype_manual(name="measure", 
                        breaks=c("isc", "subject-total"), 
                        values=c("solid", "dashed"),
                        labels=c(expression(sqrt(isc)), "subject-total")) +
  scale_y_continuous(breaks=seq(.1, 1, .2)) +
  theme_bw(base_size=25) + 
  theme(legend.title = element_text(size=25), legend.text.align=0,
        legend.key.size = unit(.4, "inches")) +
  xlab("Number of Subjects") + ylab("Mean corrrelation")
p

ggsave("../figs/ic_bias.png", p, width=10, height = 10 / 1.75)

