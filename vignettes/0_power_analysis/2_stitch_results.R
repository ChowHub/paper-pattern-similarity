# Stitch results together (if run in batches)
# command line args
#   1: path/to/prefix
#   2: output file

args = commandArgs(TRUE)
library(plyr)

files = args[1:(length(args)-1)]

out = ldply(files, function(x){
  read.csv(x)
})

write.csv(out, file=args[length(args)])
