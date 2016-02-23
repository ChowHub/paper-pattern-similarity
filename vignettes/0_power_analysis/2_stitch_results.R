# Stitch results together (if run in batches)
# command line args
#   1: path/to/prefix
#   2: output file

args = commandArgs(TRUE)
library(plyr)

files = grep(basename(args[1]), dir(dirname(args[1])), value=TRUE)

out = ldply(files, function(x){
  read.csv(x)
})

write.csv(out, filename=args[2])
