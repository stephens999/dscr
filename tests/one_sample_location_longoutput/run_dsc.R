library(dscr)

source("scenarios.R")
source("methods.R")
source("score.R")
res=run_dsc(scenarios,methods,score)

aggregate(abs_error~method+scenario,res,mean)
aggregate(squared_error~method+scenario,res,mean)



