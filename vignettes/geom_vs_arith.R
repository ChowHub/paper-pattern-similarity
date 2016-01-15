geom = function(x, y) (fishz(x) + fishz(y)) / 2
arith = function(x, y) fishz(sqrt(x*y))
fishz = function(x) 2*log((1+x)/(1-x))

lams = expand.grid(x = seq(0, 1, .1), y = c(.3, .5, .7))
lams

out = do.call(rbind,
  apply(lams, 1, function(row) {
    data.frame(x = row['x'], y = row['y'],
               geom  = geom(row['x'], row['y']),
               arith = arith(row['x'], row['y']))
  })
)
library(ggplot2)
ggplot(out, aes(x, geom-arith)) + geom_line(aes(x, geom-arith, group=factor(y), color=factor(y)))
