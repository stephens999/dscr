#' @title Dynamic Statistical Comparisons in R
#'
#' @description Package containing functions to help build dynamic statistical comparisons in R
#'
#' \tabular{ll}{ Package: \tab dscr\cr Type: \tab Package\cr
#' Version: \tab 0.1\cr License: \tab GPL (>=2)\cr }
#'
#' @name dscr-package
#' @aliases dscr
#' @docType package
#' @author Matthew Stephens <\email{mstephens@@uchicago.edu}>
#' @keywords dscr
#' @import psych plyr reshape2 knitr
NULL

#' @title return the path to a data file, parameter file, output file or results file
#'
#' @description return the path to a data file, parameter file, output file or results file
#' 
#' @param seed
#' @param scenario
#' 
#' @param datadir/paramdir/outputdir/resultsdir the (relative) path to the directory containing the relevant files
#' 
#' @return string containing path to file
#' 
#' @export
datafilename = function(dsc,seed,scenario,datadir="data"){
  return(file.path(dsc$file.dir,datadir,scenario$name,paste0("data.",seed,".RData")))
}

datafilename2 = function(seed, scenarioname,datadir="data"){
  return(file.path(datadir,scenarioname,paste0("data.",seed,".RData")))
}



#' @export
data_subdir = function(indexlist,datadir="data"){
  return(file.path(datadir,indexlist$scenarioname))
}



#' @export
outputfilename = function(dsc,seed, scenario, method, outputtype="default_output", outputdir="output"){
  return(file.path(dsc$file.dir,outputdir,outputtype,scenario$name,method$name,paste0("output.",seed,".RData")))
}

#' @export
outputfilename2 = function(seed, scenarioname,methodname, outputtype="default_output", outputdir="output"){
  return(file.path(outputdir,outputtype,scenarioname,methodname,paste0("output.",seed,".RData")))
}


#' @export
resultsfilename = function(dsc,seed, scenario, method, score=NULL, resultsdir="results"){
  if(is.null(score)){scorename="defaultscore"} else {scorename=score$name}
  return(file.path(dsc$file.dir,resultsdir,scorename,scenario$name,method$name,paste0("results.",seed,".RData")))
}

#' @export
results_subdir = function(methodname,indexlist, resultsdir="results"){
  return(file.path(resultsdir,indexlist$scenarioname,methodname))
}

#' @title Score a method on a single trial and save results
#'
#' @description Score results of a single method for a single trial and produce (and save) corresponding results
#' 
#' @param seed the seed to score
#' @param scenario the scenario to score
#' @param method the method to score
#' @param scorefn a function that scores output based on comparisons with input, parameters and metadata
#'
#' @return results, a list of appropriate format to be determined by the comparison being run (maybe required to be a dataframe?)
#' 
#' @export
score_method_singletrial = function(dsc,seed,scenario,method,score){
  if(!file.exists(resultsfilename(dsc,seed,scenario,method,score))){
    timedata = NULL #to provide backward compatibility for dscr before timedata added
    load(file=datafilename(dsc,seed,scenario))
    load(file=outputfilename(dsc,seed,scenario,method)) #also loads timedata
    results=c(score$fn(data,output),as.list(timedata))
    save(results,file=resultsfilename(dsc,seed,scenario,method,score))
  }
}

#' @title Score a method on all trials for a single scenario
#'
#' @description Score a method on all trials for a single scenario
#' 
#' @param scenario the scenario to score
#' @param method the method to score
#' @param scorefn a function that scores output based on comparisons with input, parameters and metadata
#'
#' @return results, a list of appropriate format to be determined by the comparison being run (maybe required to be a dataframe?)
#' 
#' @export
score_method_scenario = function(dsc,scenario,method,score){
  lapply(scenario$seed,score_method_singletrial,scenario=scenario,method=method,score=score,dsc=dsc)  
}

#' @title Score a method on all scenarios
#'
#' @description Score a method on all scenarios
#' 
#' @param scenarios the scenarios to score
#' @param method the method to score
#' @param scorefn a function that scores output based on comparisons with input, parameters and metadata
#'
#' @return results, a list of appropriate format to be determined by the comparison being run (maybe required to be a dataframe?)
#' 
#' @export
score_method = function(dsc,scenarios,method,score){
  lapply(scenarios,score_method_scenario,method=method,score=score,dsc=dsc)  
}

#' @title Score specified methods on specified scenarios
#'
#' @description Score specified methods on specified scenarios
#' 
#' @param scenarios the scenarios to score
#' @param methods the methods to score
#' @param scorefn a function that scores output based on comparisons with input, parameters and metadata
#'
#' @return results, a list of appropriate format to be determined by the comparison being run (maybe required to be a dataframe?)
#' 
#' @export
score_methods = function(dsc,scenarios,methods,score){
  lapply(methods, score_method, scenarios=scenarios, score=score,dsc=dsc)
}


#' @title Get the results of a single method for a single trial
#'
#' @description Get the results of a single method for a single trial
#' 
#' @param seed
#' @param scenario
#' @param method
#' 
#' @return results, a data frame of results, the details will depend on the comparison being run
#' 
#' @export
get_results_singletrial = function(dsc,seed,scenario,method,score){
  load(file=resultsfilename(dsc,seed,scenario,method,score))
  #convert NULL to NA to stop data.frame crashing
  results <- lapply(results, function(x)ifelse(is.null(x), NA, x))
  temp = c(seed=seed, scenario=scenario$name, method=method$name, results)
  class(temp)='data.frame'
  row.names(temp)=1
  #system.time({X<-(list(x='SomeText', y=200,z=1:10000)); class(X) <- 'data.frame'; row.names(X)<-1})
  return(temp)
}

#' @title provide the results of a single method for a single trial
#'
#' @description provide the results of a single method for a single trial; intended primarily for troubleshooting and debugging
#' 
#' @param seed
#' @param scenario
#' @param method
#' 
#' @return results output by score function, the details will depend on the comparison being run
#' 
#' @export
inspect_results_singletrial = function(seed, scenario,method,score){
  load(file=resultsfilename(seed,scenario,method,score))  
  return(results)
}

#' @title provide the output of a single method for a single trial
#'
#' @description provide the output of a single method for a single trial; intended primarily for troubleshooting and debugging
#' 
#' @param seed
#' @param scenario
#' @param method
#' 
#' @return output from method, the details will depend on the comparison being run
#' 
#' @export
inspect_output_singletrial = function(seed, scenario,method){
  load(file=outputfilename(seed,scenario,method))  
  return(output)
}


#' @title provide the data of a single scenario for a single trial
#'
#' @description provide the data of a single scenario for a single trial; intended primarily for troubleshooting and debugging
#' 
#' @param seed
#' @param scenario
#' 
#' @return output from method, the details will depend on the comparison being run
#' 
#' @export
inspect_data_singletrial = function(seed, scenario){
  load(file=datafilename(seed,scenario))  
  return(data)
}

#' @title Get the results of a single method for a single scenario
#'
#' @description  Get the results of a single method for a single scenario
#' 
#' @param scenario a scenario
#' @param method a method
#' @return a data frame of results, with one row for each trial. The details of the columns will depend on the comparison being run
#' 
#'
#' @export 
get_results_scenario = function(dsc,scenario, method,score){  
  print(paste0("Getting results for scenario ",scenario$name," , method ",method$name))
  ldply(scenario$seed, get_results_singletrial, scenario=scenario, method=method,score=score,dsc=dsc)
}


#' @export 
get_results = function(dsc,scenarios,method,score){
  ldply(scenarios, get_results_scenario, method=method,score=score,dsc=dsc)
}

#' @title Aggregate the results of multiple methods for multiple scenarios
#'
#' @description Aggregate the results of multiple methods for multiple scenarios
#' 
#' @param scenarios a list of scenarios. 
#' @param methods a list of methods
#' @return a data frame of results, with one row for each trial/method combination. The details of the columns will depend on the comparison being run
#' 
#' 
#' @export 
aggregate_results = function(dsc,scenarios,methods,score){
  ldply(methods,get_results,scenarios=scenarios,score=score,dsc=dsc)
}

make_dirs = function(namelist){
  for(i in 1:length(namelist)){dir.create(namelist[i],recursive=TRUE,showWarnings=FALSE)}
}

#' @export 
make_directories = function(dsc){
  scenarionames = names(dsc$scenarios)
  methodnames = names(dsc$methods)
  scorenames = names(dsc$scores)
  smdirs = as.vector(outer(scenarionames,methodnames,file.path))
  ssmdirs = as.vector(outer(scorenames,smdirs,file.path))
  
  make_dirs(outer(file.path(dsc$file.dir,"data"),scenarionames,file.path))
  make_dirs(outer(file.path(dsc$file.dir,"output","default_output"),smdirs,file.path))
  make_dirs(outer(file.path(dsc$file.dir,"results"),ssmdirs,file.path))
}

#' @title Make the data (inputs and meta) for a DSC for a particular seed and scenario
#'
#' @description Make the data (inputs and meta) for a DSC for a particular seed and scenario. THe subscript rs refers to the "repetition" of a "scenario".
#' I'm experiementing with naming conventions to try to make these easier to see, using . as a subscript to indicate
#' that that subscript is applied over. By this logic make_data_scenario should be make_data_.s
#' 
#' @param seed (integer) the seed for the pseudo-rng 
#' @param scenario a list including elements fn and args, the function name for the datamaker to be used and additional arguments
#' @param overwrite boolean indicating whether to overwrite existing files; default is not to
#' 
#' @return data are saved in files in the data subdirectory
#' @export 
make_data_rs = function(seed,scenario,overwrite=FALSE){
  if(!file.exists(datafilename(seed,scenario)) | overwrite){
    set.seed(seed)
    data = do.call(scenario$fn,list(args=scenario$args))
    save(data,file=datafilename(seed,scenario))
  } 
}
#' @title Make the data (inputs and meta) for a DSC for a particular scenario
#'
#' @description Make the data (inputs and meta) for a DSC for a particular scenario
#' 
#' @param scenario a list including elements fn and args, the function name for the datamaker to be used and additional arguments
#' 
#' @return data are saved in files in the data subdirectory
#' @export 
make_data_scenario = function(scenario){
  print(paste0("Making data for scenario ",scenario$name))
  lapply(scenario$seed,make_data_rs,scenario=scenario)
}
  
  
#' @title Make all the data for a DSC
#'
#' @description Make all the data for DSC using all combinations of seed and scenario
#' 
#' @param scenarios a list of scenarios
#' 
#' @return none; data are saved in files in the data subdirectory
#' @export 
make_data = function(scenarios){
  lapply(scenarios,make_data_scenario)
}


#' @title Apply a method for a particular seed and scenario
#'
#' @description Apply a method for a particular seed and scenario. THe subscript _rsm refers to a particular repetition
#' of a particular scenario for particular method.
#' 
#' @param seed (integer) the seed for the pseudo-rng which identifies/indexes trials (the seed is set to seed+1 before the method is run)
#' @param scenario a list including elements fn and args, the function name for the datamaker to be used and additional arguments
#' @param method a list including elements fn, name and args
#' 
#' @return none; output are saved in the output subdirectory
#' @export 
apply_method_rsm = function(seed, scenario, method){
  if(!file.exists(outputfilename(seed,scenario,method))){
    load(datafilename(seed,scenario))
    set.seed(seed+1)
    timedata = system.time(output <- do.call(method$fn,list(input=data$input,args=method$args)))
    save(output,timedata,file=outputfilename(seed,scenario,method))
  }
}

#' @title Apply a method to all trials for a particular scenario
#'
#' @description Apply a method to all trials for a particular scenario
#' 
#' @param scenario a list including elements seed and name
#' @param method a list including fn and name
#' 
#' @return none; output are saved in the output subdirectory
#' @export 
apply_method_.sm = function(scenario,method){
  print(paste0("Applying method ", method$name," to scenario ",scenario$name))
  lapply(scenario$seed,apply_method_rsm,scenario=scenario,method=method)
}


#' @title Apply method to all scenarios
#'
#' @description Apply method to all scenarios
#' 
#' @param scenarios a list of scenarios
#' @param method a list containing the method name, fn, etc
#' 
#' @return none; data are saved in files in the output subdirectory
#' @export
apply_method_..m = function(scenarios,method){
  lapply(scenarios,apply_method_.sm,method=method)
}

#' @title Apply all methods to a scenario
#'
#' @description Apply all methods to a scenario
#' 
#' @param scenario a scenario
#' @param methods a list of methods
#' 
#' @return none; data are saved in files in the output subdirectory
#' @export
apply_method_.s. = function(scenario,methods){
  lapply(methods,apply_method_.sm,scenario=scenario)
}

#' @title Apply all methods to all scenarios
#'
#' @description Apply all methods to all scenarios
#' 
#' @param scenarios a list of scenarios
#' @param methods a list of methods
#' 
#' @return none; data are saved in files in the output subdirectory
#' @export 
apply_methods = function(scenarios,methods){
  lapply(scenarios,apply_method_.s.,methods=methods)
}

#' @title Sources all R files in a directory
#'
#' @description Sources all R files in a directory (comes from examples in ??source)
#' 
#' @param path the directory you want to source
#' @param trace whether to print the names of files being sourced
#' 
#' @return none;
#' @export 
sourceDir <- function(path, trace = TRUE, ...) {
  for (nm in list.files(path, pattern = "[.][RrSsQq]$")) {
    if(trace) cat(nm,":")
    source(file.path(path, nm), ...)
    if(trace) cat("\n")
  }
}

#' @title Start a new (empty) dsc
#'
#' @description returns an environment that is a dsc
#'
#' @param name a string containing the name for your dsc
#' 
#' @return dsc, an environmnet
#' @export
new.dsc = function(name,file.dir){
  dsc=new.env()
  dsc$methods=list()
  dsc$scenarios=list()
  dsc$scores=list()
  dsc$parsers=list()
  dsc$name=name 
  dsc$db=NULL
  dsc$res = NULL
  dsc$file.dir=file.dir
  return(dsc)
}



#' @title Add a scenario to a dsc
#'
#' @description Adds a scenario to a dsc
#'
#' @param dsc the dsc to add the sceanario to
#' @param name a character string 
#' @param fn the datamaker, a function that is the datamaker for the scenario
#' @param args a list of arguments to the datamaker
#' @param seed a vector of integers showing seeds to use for the scenario
#' 
#' @return nothing, but modifies the dsc environment
#' @export
addScenario = function(dsc,name, fn, args=NULL, seed){
  if(name %in% names(dsc$scenarios)){stop("Error: that scenario name already exists")}
  if(!is.character(name)){stop("Error: name must be a string")}
  if(!is.function(fn)){stop("Error: fn must be a function")}
  if(!is.list(args) & !is.null(args)){stop("Error: args must be a list or NULL")}  
  if(!is.integer(seed)){stop("seed must be an integer vector")}
  dsc$scenarios[[name]]=list(name=name,fn=fn,args=args,seed=seed)
}




#' @title Add a method to a dsc
#'
#' @description Adds a method to a dsc
#'
#' @param dsc the dsc to add the method to
#' @param name a character string name for the method
#' @param fn, a wrapper function that implements the method
#' @param args a list of additional arguments to fn
#' 
#' @return nothing, but modifies the dsc environment
#' @export
addMethod = function(dsc,name, fn, args=NULL){
  if(name %in% names(dsc$methods)){stop("Error: that method name already exists")}
  if(!is.character(name)){stop("Error: name must be a string")}
  if(!is.function(fn)){stop("Error: fn must be a function")}
  if(!is.list(args) & !is.null(args)){stop("Error: args must be a list or NULL")}
  
  dsc$methods[[name]]=list(name=name,fn=fn,args=args)
}



#' @title Add a score function to a dsc
#'
#' @description Adds a score to a dsc
#'
#' @param dsc the dsc to add the score to
#' @param name string giving a name by which the score function is to be known by
#' @param fn, a score function
#' @param outputtype, the type of output the score function takes, as generated by a parser 
#' 
#' @return nothing, but modifies the dsc environment
#' @export
addScore = function(dsc,name,fn,outputtype="default_output"){
  if(name %in% names(dsc$scores)){stop("Error: that score name already exists")}
  if(!is.character(name)){stop("Error: name must be a string")}
  if(!is.function(fn)){stop("Error: fn must be a function")}
    
  dsc$scores[[name]]=list(name=name,fn=fn,outputtype=outputtype)
}

#' @title Add a parser to a dsc
#'
#' @description Adds a parser to a dsc; a parser converts one type of output to another type
#'
#' @param dsc the dsc to add the parser
#' @param name string giving a name by which the parser function is to be known by
#' @param fn, a parser function
#' @param outputtype, string naming the type of output the parser function creates. 
#' @param methodnames, a list of methodnames that the parser can be applied to
#' 
#' @return nothing, but modifies the dsc environment
#' @export
addParser = function(dsc,name,fn,outputtype,methodnames){
  if(name %in% names(dsc$parsers)){stop("Error: that parser name already exists")}
  if(!is.character(name)){stop("Error: name must be a string")}
  if(!is.function(fn)){stop("Error: fn must be a function")}
  if(!all(methodnames %in% names(dsc$methods))){stop("Error: not all methodnames exist")}
  dsc$parsers[[name]]=list(name=name,fn=fn,outputtype=outputtype,methodnames=methodnames)
}


getScenarioNames = function(dsc){return(names(dsc$scenarios))}
getMethodNames = function(dsc){return(names(dsc$methods))}

#' @title List scenarios
#'
#' @description List scenarios
#' @param dsc the dsc to have its scenarios listed
#' 
#' @return nothing
#' @export
listScenarios = function(dsc){print(getScenarioNames(dsc))}

#' @title List methods
#'
#' @description List methods
#' @param dsc the dsc to have its methods listed
#' 
#' @return nothing
#' @export
listMethods = function(dsc){print(getMethodNames(dsc))}

#' @export
runScenario=function(dsc,seed,scenarioname){
  scenario = dsc$scenarios[[scenarioname]]
  if(!file.exists(datafilename(dsc,seed,scenario))){
    set.seed(seed)
    data = do.call(scenario$fn,list(args=scenario$args))
    save(data,file=datafilename(dsc,seed,scenario))
  }   
}

#' @export
runScenarios=function(dsc,ssub=NULL){
  df = expandScenarios(dsc)
  if(!is.null(ssub)){df = dplyr::filter(df,scenarioname %in% ssub)}
  print(paste0("running Scenarios"))
  mapply(runScenario,seed=df$seed,scenarioname=df$scenarioname,MoreArgs=list(dsc=dsc))
  #reg1 <- makeRegistry(id="my_reg1", seed=123, file.dir="my_job_dir1")
  #batchMap(reg1, runScenario,seed=df$seed,scenarioname=df$scenarioname,more.args=list(dsc=dsc))
  #ids <- getJobIds(reg1)
  #submitJobs(reg1, ids)  
}


#' @export
runMethod=function(dsc,seed,scenarioname,methodname){
  print(paste0("running method ",methodname,", on scenario ", scenarioname, ", seed ",seed))
  scenario = dsc$scenarios[[scenarioname]]
  method = dsc$methods[[methodname]]
  if(!file.exists(outputfilename(dsc,seed,scenario,method))){
    load(datafilename(dsc,seed,scenario))
    set.seed(seed+1)
    timedata = system.time(output <- do.call(method$fn,list(input=data$input,args=method$args)))
    save(output,timedata,file=outputfilename(dsc,seed,scenario,method))
  }
}



#' @export
runMethods=function(dsc,ssub=NULL,msub=NULL){
  df = expandAll(dsc)
  if(!is.null(ssub)){df = dplyr::filter(df,scenarioname %in% ssub)}
  if(!is.null(msub)){df = dplyr::filter(df,methodname %in% msub)}
  print(paste0("running Methods"))
  mapply(runMethod,seed=df$seed,scenarioname=df$scenarioname,methodname=df$methodname,MoreArgs=list(dsc=dsc))
  #reg2 <- makeRegistry(id="my_reg2", seed=123, file.dir="my_job_dir2")
  #batchMap(reg2, runMethod,seed=df$seed,scenarioname=df$scenarioname,methodname=df$methodname,more.args=list(dsc=dsc))
  #ids <- getJobIds(reg2)
  #submitJobs(reg2, ids)  
}



#' @title Create a dataframe of scenarioname and seed combinations
#'
#' @description Create a dataframe of scenarioname and seed combinations
#' @param scenario the scenario to be expanded
#' 
#' @return data frame of scenarioname and seed combinations
#' @export
expandScenario = function(scenario){data.frame(scenarioname=scenario$name,seed=scenario$seed,stringsAsFactors=FALSE)}

#' @title Create a dataframe of all scenarioname and seed combinations
#'
#' @description Create a dataframe of all scenarioname and seed combinations
#' @param dsc the dsc to be expanded
#' 
#' @return data frame of all scenarioname and seed combinations
#' @export
expandScenarios = function(dsc){ldply(dsc$scenarios,expandScenario,.id="scenarioname")}

#' @title Create a list of scenarioname, seed and method combinations
#'
#' @description  Create a list of scenarioname, seed and method combinations
#' @param dsc the dsc to expand
#' 
#' @return data frame of all combinations
#' @export
expandAll = function(dsc){merge(expandScenarios(dsc),data.frame(methodname=getMethodNames(dsc),stringsAsFactors=FALSE))}
                                
#' @title Add outputfilenames to the database
#'
#' @description Add outputfilenames
#' @param dsc the dsc to use
#' 
#' @return nothing, but modified dsc
#' @export
addFilenames=function(dsc,outputfiletype){
  dsc$db=mutate(dsc$db,outputfile=outputfilename2(seed,scenarioname,methodname),
                datafile = datafilename2(seed,scenarioname))
}

#runParser = function(fn,infile,outfile){
#  load(infile)
#  output=fn(output)
#  save(output,file=outfile)
#}
  
#' @title Run a parser in a dsc
#'
#' @description Run the named parser to convert one type of output to another type
#'
#' @param dsc the dsc to use
#' @param parsername the name of the parser function to be run
#' 
#' @return nothing, but outputs files to output/ directories
#' @export
runParser = function(dsc,parsername){
  if(!(parsername %in% names(dsc$parsers))){stop("Error: that parsername does not exist")}
  parser=dsc$parsers[[parsername]]
  OutputFiles(dsc,ssub,msub)
#  for each  method in parser$methodnames apply parser$fn to create parser$outputtype
}



#' @title Removes all data, output and results for the dsc
#'
#' @description Removes all files in results/ data/ and output/ subdirectories. Mostly useful for testing purposes.
#'
#' @param scenarios a list of scenarios in the dsc
#' @param methods a list of methods in the dsc
#' @param force boolean, indicates whether to proceed without prompting user
#' 
#' @return nothing; simply deletes files
#' @export
reset_dsc = function(dsc,force=FALSE){
  for(i in 1:length(dsc$scenarios)){
    reset_scenario(dsc,dsc$scenarios[[i]]$name,force)
  }
  for(i in 1:length(dsc$methods)){
    reset_method(dsc,dsc$methods[[i]]$name,force)
  }
}

#' @title Removes all output and results for a method
#'
#' @description Removes all output and results for a method; primary intended purpose is to force re-running of that method.
#' Works only for unix look-alikes?
#'
#' @param method string indicating name of methods to remove output
#' @param force boolean, indicates whether to proceed without prompting user (prompt is to be implemented)
#' 
#' @return nothing; simply delets files
#' @export
reset_method = function(dsc,methodname,force=FALSE){
  if(!is.character(methodname))
    stop("Error in call to reset_method: must give methodname as string")
  
  if(!force){
    if(interactive()){
      force = (readline("Are you sure? [y to confirm]:")=="y")
    } else {
      stop("Error: Must specify force = TRUE in reset_method in non-interactive mode.")
    } 
  }
  
  if(force){
    file.remove(Sys.glob(file.path(dsc$file.dir,"output","*","*",methodname,"*")))
    file.remove(Sys.glob(file.path(dsc$file.dir,"results","*","*",methodname,"*")))    
  }
}

#' @title Removes all output and results for a scenario
#'
#' @description Removes all output and results for a scenario; primary intended purpose is to force re-running of that scenario.
#' Works only for unix look-alikes?
#'
#' @param scenarioname string indicating name of scenario to remove
#' @param force boolean, indicates whether to proceed without prompting user (prompt is to be implemented)
#' 
#' @return nothing; simply delets files
#' @export
reset_scenario = function(dsc,scenarioname,force=FALSE){
  if(!is.character(scenarioname))
    stop("Error in call to reset_scenario: must give scenarioname as string")
    
  if(!force){
    if(interactive()){
    force = (readline("Are you sure? [y to confirm]:")=="y")
    } else {
      stop("Error: Must specify force = TRUE in reset_scenario in non-interactive mode.")
    } 
  }
  
  if(force){
    file.remove(Sys.glob(file.path(dsc$file.dir,"data",scenarioname,"*")))
    file.remove(Sys.glob(file.path(dsc$file.dir,"output","*",scenarioname,"*","*")))
    file.remove(Sys.glob(file.path(dsc$file.dir,"results","*",scenarioname,"*","*")))    
  }
}

db.create=function(dsc){
  dsc$db=expandAll(dsc)
  addFilenames(dsc)
}

#' @title Run all methods on all scenarios for a DSC
#'
#' @description Run all methods on all scenarios for a DSC
#'
#' @param scenarios a list of scenarios used to produce data=list(input,meta)
#' @param methods a list of methods to turn data$input into output
#' @param scorefn a function that takes output and scores it against data
#' @param scenariosubset a vector of the names of the scenarios to actually make and run
#' @param methodsubset a vector of the names of the methods to run (default is to run all of them)
#' 
#' @return data frame of results from all methods run on all scenarios
#' @export
run_dsc=function(dsc,scenariosubset=NULL, methodsubset=NULL){
  scenarios=dsc$scenarios
  methods=dsc$methods
  score=dsc$scores[[1]]
  
  if(!is.null(scenariosubset)){
    scenarionames=lapply(scenarios,function(x){return(x$name)})
    ssub = scenarionames %in% scenariosubset
  } else {ssub = rep(TRUE, length(scenarios))}
    
  if(!is.null(methodsubset)){
    methodnames=lapply(methods,function(x){return(x$name)})
    msub = methodnames %in% methodsubset
  } else { msub= rep(TRUE, length(methods))}
  #dsc$db=NULL
  #db.create(dsc)
  
  make_directories(dsc)
  
  runScenarios(dsc,scenariosubset)
  runMethods(dsc,scenariosubset,methodsubset)

  #  make_data(scenarios[ssub])
#  apply_methods(scenarios[ssub],methods[msub])
  
  score_methods(dsc,scenarios[ssub],methods[msub],score)
  
      
  res=aggregate_results(dsc,scenarios[ssub],methods[msub],score)
  dsc$res = res
  return(res)
}


#' @title plot results for DSC
#'
#' @description interactive plot for results of DSC
#'
#' @param dsc
#' @return a shiny plot
#' @export
shiny_plot=function(dsc){
  scenario_names = as.character(unique(dsc$res$scenario))
  method_names = as.character(unique(dsc$res$method))
  numeric_criteria = names(dsc$res)[unlist(lapply(dsc$res,is.numeric))]

  ui=shinyUI(pageWithSidebar(
    headerPanel('DSC Results'),
    sidebarPanel(
      checkboxGroupInput("scen.subset", "Choose Scenarios", 
                                               choices  = scenario_names,
                                               selected = scenario_names),
      
      checkboxGroupInput("method.subset", "Choose Methods", 
                            choices  = method_names,
                            selected = method_names),
         
      selectInput("criteria", "Choose Criteria", 
                     choices  = numeric_criteria,
                     selected = numeric_criteria[1])

    ),
    mainPanel(
      plotOutput('plot1')
    )
  ))

  server = shinyServer(
    function(input, output, session) {
      output$plot1 <- renderPlot({
        res.filter = dplyr::filter(dsc$res,scenario %in% input$scen.subset & method %in% input$method.subset)
        print(input)
        res.filter$value = res.filter[[input$criteria]]
        ggplot(res.filter,aes(method,value,color=method)) + geom_boxplot() + facet_grid(.~scenario)
      })
    }
  )
  
  shinyApp(ui=ui,server=server)
}
#' 
#' 
#' @title Create or update a Makefile for the package
#'
#' @description Create or update a Makefile for the package.
#'
#' @param deps depends of the package to check in Makefile
#' @param run_methods method names to run in Makefile
#' @param vignettes vignette names to built in Makefile
#'
#' @return Makefile for the package
#'
#' @export update_makefile
#'
#' @examples
#' \donttest{
#' deps = c('devtools', 'roxygen2', 'knitr', 'plyr', 'psych', 'reshape2')
#' run_methods = c('one_sample_location')
#' vignettes = c('one_sample_location.rmd')
#' update_makefile(deps, run_methods, vignettes, output = '~/dscr/Makefile')}
update_makefile = function(deps = NULL,
                           run_methods = NULL,
                           vignettes = NULL,
                           output = NULL) {
  
  # deps
  deps_str = if (is.null(deps)) 'echo "No depends needed"' else 
    paste(paste0("\tRscript -e \'if (!require(\"", deps, 
                 "\")) install.packages(\"", deps, 
                 "\", repos=\"http://cran.rstudio.com\")\';\\"), 
          collapse = "\n")
  
  # run-methods
  if (is.null(run_methods)) {
    run_methods_str = 'echo "No methods found"'
  } else if (length(run_methods) == 1L) {
    run_methods_str = paste(paste0('\tcd inst/examples/', run_methods, 
                                   ' || { echo "Running method: ', run_methods, 
                                   ' failed\"; exit 1; } ;\\'),
                            paste0('\tR -q -e \'library(\"dscr\"); source(\"datamaker.R\"); source(\"method.R\"); source(\"score.R\"); source(\"scenario.R\"); source(\"rundsc.R\")\';\\'), 
                            sep = "\n")
  } else {
    run_methods_str = rep(NA, length(run_methods))
    run_methods_str[1L] = paste(paste0('\tcd inst/examples/', run_methods[1L], 
                                       ' || { echo "Running method: ', run_methods[1L], 
                                       ' failed\"; exit 1; } ;\\'),
                                paste0('\tR -q -e \'library(\"dscr\"); source(\"datamaker.R\"); source(\"method.R\"); source(\"score.R\"); source(\"scenario.R\"); source(\"rundsc.R\")\';\\'), 
                                sep = "\n")
    
    run_methods_str[-1L] = paste(paste0('\tcd ../', run_methods[-1L], 
                                        ' || { echo "Running method: ', run_methods[-1L], 
                                        ' failed\"; exit 1; } ;\\'),
                                 paste0('\tR -q -e \'library(\"dscr\"); source(\"datamaker.R\"); source(\"method.R\"); source(\"score.R\"); source(\"scenario.R\"); source(\"rundsc.R\")\';\\'), 
                                 sep = '\n')
    run_methods_str = paste(run_methods_str, collapse = '\n')
  }
  
  # run-methods-clean
  if (is.null(run_methods)) {
    run_methods_clean_str = 'echo "No methods found"'
  } else if (length(run_methods) == 1L) {
    run_methods_clean_str = paste(paste0('\tcd inst/examples/', run_methods, 
                                         ' || { echo "Cleaning method: ', run_methods, 
                                         ' failed\"; exit 1; } ;\\'),
                                  paste0('\tR -q -e \'unlink(setdiff(list.files(), c(\"datamaker.R\", \"method.R\", \"score.R\", \"scenario.R\", \"rundsc.R\")), recursive=TRUE)\';\\'), 
                                  sep = "\n")
  } else {
    run_methods_clean_str = rep(NA, length(run_methods))
    run_methods_clean_str[1L] = paste(paste0('\tcd inst/examples/', run_methods[1L], 
                                             ' || { echo "Cleaning method: ', run_methods[1L], 
                                             ' failed\"; exit 1; } ;\\'),
                                      paste0('\tR -q -e \'unlink(setdiff(list.files(), c(\"datamaker.R\", \"method.R\", \"score.R\", \"scenario.R\", \"rundsc.R\")), recursive=TRUE)\';\\'), 
                                      sep = "\n")
    
    run_methods_clean_str[-1L] = paste(paste0('\tcd ../', run_methods[-1L], 
                                              ' || { echo "Cleaning method: ', run_methods[-1L], 
                                              ' failed\"; exit 1; } ;\\'),
                                       paste0('\tR -q -e \'unlink(setdiff(list.files(), c(\"datamaker.R\", \"method.R\", \"score.R\", \"scenario.R\", \"rundsc.R\")), recursive=TRUE)\';\\'), 
                                       sep = '\n')
    run_methods_clean_str = paste(run_methods_clean_str, collapse = '\n')
  }
  
  # vignettes
  vignettes_str = if (is.null(vignettes)) 'echo "No vignettes"' else
    paste(paste0("\tR -q -e \'library(\"knitr\"); knit2html(\"", vignettes,
                 "\"); browseURL(\"", 
                 paste0(sub("([^.]+)\\.[[:alnum:]]+$", "\\1", vignettes), '.html'),
                 "\")\';\\"), 
          collapse = '\n')
  
  # vignettes-clean
  vignettes_clean_str = if (is.null(vignettes)) 'echo "No vignettes"' else
    paste0("\tR -q -e \'unlink(setdiff(list.files(), c(\"",
           paste(vignettes, collapse = ','),
           "\")), recursive=TRUE)\';\\")  
  
  knit(system.file('examples/Makefile.Rmd', package = 'dscr'),
       output = output, quiet = TRUE)
  
}


