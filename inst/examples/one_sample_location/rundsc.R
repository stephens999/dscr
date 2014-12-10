library(dscr)
res=run_dsc(scenarios,methods,score)


head(res)

aggregate(abs_error~method+scenario,res,mean)
aggregate(squared_error~method+scenario,res,mean)

