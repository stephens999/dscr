
##' @description convert a method function (with input (input, args) to a algo function, with input dynamic)
##' Not needed, so commented out!
#mfn2afn = function(fn,...){
#  return(function(dynamic,...){fn(dynamic$input,args=list(...))})
#}

##' @title convert methodfn and scorefn to a problem function
##' @description convert a method function (with input (input, args) and a score function (with input (data,output))  to a algo function, with input dynamic and output the score)
msfn_to_afn = function(methodfn,scorefn,...){
  return(function(dynamic,...){scorefn(dynamic,methodfn(dynamic$input,args=list(...)))})  
}

##' @title convert datamaker to problem fn
##'
##' @description convert a scenario (datamaker) function with input (args) to a problem function, with input (static)
sfn_to_pfn = function(fn,...){
  return(function(static,...){fn(args=list(...))}) 
}

is_empty=function(l){return(length(l)==0)}

#' 
#' 
#' @title Create a BatchExperiments registry from a dsc
#'
#' @description Create a BatchExperiments registry from a dsc
#'
#' @param dsc a list with components scenarios and methods and scorefn
#' @param id a string for the id   
#' 
#' @return a registry reg for BatchExperiments
#' @export
dsc_to_be=function(dsc,id,repl=100){
  reg <- makeExperimentRegistry(id=id)
  prob.designs=list()
  algo.designs=list()
  for(i in 1:length(dsc$scenarios)){
    addProblem(reg, id=dsc$scenarios[[i]]$name, dynamic=sfn_to_pfn(dsc$scenarios[[i]]$fn),seed=1)
    if(!is.null(dsc$scenarios[[i]]$args)){
      prob.designs[[i]]=makeDesign(dsc$scenarios[[i]]$name,exhaustive=dsc$scenarios[[i]]$args)
    }
  }
  for(i in 1:length(dsc$methods)){
    addAlgorithm(reg, id=dsc$methods[[i]]$name, fun=msfn_to_afn(dsc$methods[[i]]$fn,dsc$scores[[1]]$fn))
    if(!is.null(dsc$methods[[i]]$args)){
      algo.designs[[i]]=makeDesign(id,exhaustive=dsc$methods[[i]]$args) 
    }
  }
  if(is_empty(prob.designs)){
    if(is_empty(algo.designs)){
      addExperiments(reg,repl=repl) 
    } else {
      addExperiments(reg,algo.designs=algo.designs,repl=repl)
    }
  } else {
    if(is_empty(algo.designs)){
      addExperiments(reg,prob.designs=prob.designs,repl=repl) 
    } else {
      addExperiments(reg,prob.designs=prob.designs,algo.designs=algo.designs,repl=repl)
    } 
  }
  reg
}


