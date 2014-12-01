res=run_dsc(parammaker,datamaker,methods,score,scenario_seedlist)

head(res)

aggregate(abs_error~method+scenario,res,mean)
aggregate(squared_error~method+scenario,res,mean)

trimmedmean.wrapper = function(input){
  return(list(meanest=mean(input$x,trim=0.2)))
}

methods$trimmedmean = list(name="trimmedmean",fn = "trimmedmean.wrapper")

res=run_dsc(parammaker,datamaker,methods,score,scenario_seedlist)
aggregate(abs_error~method+scenario,res,mean)
aggregate(squared_error~method+scenario,res,mean)
