library(dscr)

#init_dsc()
source("scenarios.R")
source("methods.R")
source("score.R")
dsc=list(scenarios=scenarios,methods=methods,scorefn=score)
reset_dsc(dsc)
res=run_dsc(dsc)

aggregate(abs_error~method+scenario,res,mean)
aggregate(squared_error~method+scenario,res,mean)



