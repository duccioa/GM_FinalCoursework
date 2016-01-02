library(doParallel)
registerDoParallel(cores = 6)
foreach(i = 8) %dopar% read.NYCData(i,-1)
foreach(i = 8) %dopar% prepare.DATA(i)