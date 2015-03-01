library(dscr)
sourceDir("methods")
sourceDir("datamakers")

###### Initialize #######

dsc_eg=new.dsc("one-sample-location")

###### Add Scenarios #####

addScenario(dsc_eg,"normal",datamaker,list(disttype="normal",nsamp=1000),1:100)
addScenario(dsc_eg,"uniform",datamaker,list(disttype="uniform",nsamp=1000),1:100)
addScenario(dsc_eg,"Cauchy",datamaker,list(disttype="Cauchy",nsamp=1000),seed=1:100)

###### Add Methods #####

addMethod(dsc_eg,"mean",mean.wrapper)
addMethod(dsc_eg,"median",median.wrapper)
addMethod(dsc_eg,"winsor",winsor.wrapper)

####### Define Score and Add it #######

score = function(data, output){
  return(list(squared_error = (data$meta$truemean-output$meanest)^2, 
              abs_error = abs(data$meta$truemean-output$meanest)))
}

addScore(dsc_eg,score)

######## Run the DSC #################

reset_dsc(dsc_eg,force=TRUE)
res=run_dsc(dsc_eg)
save(dsc_eg,file="dsc_eg.RData")

aggregate(abs_error~method+scenario,res,mean)
aggregate(squared_error~method+scenario,res,mean)



