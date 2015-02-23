library(dscr)

dsc_eg=new.dsc("one-sample-location")
source("scenarios.R")
source("methods.R")
source("score.R")

reset_dsc(dsc_eg,force=TRUE)
res=run_dsc(dsc_eg)

aggregate(abs_error~method+scenario,res,mean)
aggregate(squared_error~method+scenario,res,mean)



