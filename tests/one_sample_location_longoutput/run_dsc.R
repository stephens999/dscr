library(dscr)

source("scenarios.R")
source("methods.R")
source("score.R")
res=run_dsc(scenarios,methods,score)
save(res,file="res.RData")

aggregate(abs_error~method+scenario,res,mean)
aggregate(squared_error~method+scenario,res,mean)



