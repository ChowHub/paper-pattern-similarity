rel_range = function(n, lam){
  reliability(rep(lam, n))
}

rels = c(sapply(5:20, rel_range, .25),
         sapply(5:20, rel_range, .3),
         sapply(5:20, rel_range, .5)
)

rels = data.frame(lam = rep(c(.25, .3, .5), each=16), n = 5:20)

rels$rel = sapply(1:nrow(rels), function(ii){
  do.call(rel_range, rels[ii,])
})

head(rels)

ggplot(rels, aes(n, rel, color=factor(lam))) + geom_point() +
  geom_line(aes(group=lam))