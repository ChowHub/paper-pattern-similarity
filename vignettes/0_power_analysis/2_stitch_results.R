args = commandArgs(TRUE)
library(plyr)

files = grep(basename(args[1]), dir(dirname(args[1])), value=TRUE)

out = ldply(files, full.names = TRUE), function(x){
  read.csv(x)
})

write.csv(out, filename=args[2])
