library("BatchExperiments")
 
#' @description convert a method function (with input (input, args) to a algo function, with input dynamic)
#' 
mfn2afn = function(fn,...){
  return(function(dynamic,...){fn(dynamic$input,args=list(...))})
}

#' @description convert a scenario (datamaker) function with input (args) to a problem function, with input (static)
sfn2pfn = function(fn,...){
  return(function(static,...){fn(seed=.Random.seed,args=list(...))}) 
}

is.empty=function(l){return(length(l)==0)}

#' @param dsc a list with components scenarios and methods
#' @param id a string for the id   
dsc2BE=function(dsc,id,repl=100){
  reg <- makeExperimentRegistry(id=id)
  prob.designs=list()
  algo.designs=list()
  for(i in 1:length(dsc$scenarios)){
    addProblem(reg, id=dsc$scenarios[[i]]$name, dynamic=sfn2pfn(dsc$scenarios[[i]]$fn),seed=1)
    if(!is.null(dsc$scenarios[[i]]$args)){
      prob.designs[[i]]=makeDesign(dsc$scenarios[[i]]$name,exhaustive=dsc$scenarios[[i]]$args)
    }
  }
  for(i in 1:length(dsc$methods)){
    addAlgorithm(reg, id=dsc$methods[[i]]$name, fun=mfn2afn(dsc$methods[[i]]$fn))
    if(!is.null(dsc$methods[[i]]$args)){
        algo.designs[[i]]=makeDesign(id,exhaustive=dsc$methods[[i]]$args) 
    }
  }
  
  if(is.empty(prob.designs)){
    if(is.empty(algo.designs)){
      addExperiments(reg,repl=repl) 
    } else {
      addExperiments(reg,algo.designs=algo.designs,repl=repl)
    }
  } else {
    if(is.empty(algo.designs)){
      addExperiments(reg,prob.designs=prob.designs,repl=repl) 
    } else {
      addExperiments(reg,prob.designs=prob.designs,algo.designs=algo.designs,repl=repl)
    } 
  }
    reg
}

system("rm -r one_sample_location-files")
dsc = list(scenarios=scenarios,methods=methods)
reg=dsc2BE(dsc,"one_sample_location")
summarizeExperiments(reg)

id1 <- findExperiments(reg, algo.pattern="mean")[1]
testJob(reg,id1)
chunked <- chunk(getJobIds(reg), n.chunks = 10, shuffle = TRUE)
timetaken=system.time(submitJobs(reg, chunked))

get_meanest <- function(job, res) {
  list(meanest=res$meanest)
}
temp=reduceResultsExperiments(reg, ids=findDone(reg), fun=get_meanest)

aggregate(meanest~algo+prob,data=temp,mean)

