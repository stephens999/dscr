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
datafilename = function(seed,scenario,datadir="data"){
  return(file.path(datadir,scenario$name,paste0("data.",seed,".RData")))
}

#' @export
data_subdir = function(indexlist,datadir="data"){
  return(file.path(datadir,indexlist$scenarioname))
}



#' @export
outputfilename = function(seed, scenario, method, outputdir="output"){
  return(file.path(outputdir,scenario$name,method$name,paste0("output.",seed,".RData")))
}



#' @export
output_subdir = function(methodname,indexlist, outputdir="output"){
  return(file.path(outputdir,indexlist$scenarioname,methodname))
}

#' @export
resultsfilename = function(seed, scenario, method, resultsdir="results"){
  return(file.path(resultsdir,scenario$name,method$name,paste0("results.",seed,".RData")))
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
score_method_singletrial = function(seed,scenario,method,scorefn){
  if(!file.exists(resultsfilename(seed,scenario,method))){
    timedata = NULL #to provide backward compatibility for dscr before timedata added
    load(file=datafilename(seed,scenario))
    load(file=outputfilename(seed,scenario,method)) #also loads timedata
    results=c(scorefn(data,output),as.list(timedata))
    save(results,file=resultsfilename(seed,scenario,method))
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
score_method_scenario = function(scenario,method,scorefn){
  lapply(scenario$seed,score_method_singletrial,scenario=scenario,method=method,scorefn=scorefn)  
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
score_method = function(scenarios,method,scorefn){
  lapply(scenarios,score_method_scenario,method=method,scorefn=scorefn)  
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
score_methods = function(scenarios,methods,scorefn){
  lapply(methods, score_method, scenarios=scenarios, scorefn=scorefn)
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
get_results_singletrial = function(seed,scenario,method){
  load(file=resultsfilename(seed,scenario,method))
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
inspect_results_singletrial = function(seed, scenario,method){
  load(file=resultsfilename(seed,scenario,method))  
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
get_results_scenario = function(scenario, method){  
  print(paste0("Getting results for scenario ",scenario$name," , method ",method$name))
  ldply(scenario$seed, get_results_singletrial, scenario=scenario, method=method)
}


#' @export 
get_results = function(scenarios,method){
  ldply(scenarios, get_results_scenario, method=method)
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
aggregate_results = function(scenarios,methods){
  ldply(methods,get_results,scenarios=scenarios)
}


#' @export 
make_directories_method= function(method,scenario){ 
  dir.create(file.path("output",scenario$name,method$name),showWarnings=FALSE)
  dir.create(file.path("results",scenario$name,method$name),showWarnings=FALSE)
}


#' @export 
make_directories_scenario=function(scenario,methods){ 
  dir.create(file.path("data",scenario$name),showWarnings=FALSE)
  dir.create(file.path("output",scenario$name),showWarnings=FALSE)
  dir.create(file.path("results",scenario$name),showWarnings=FALSE)
  l_ply(methods,make_directories_method,scenario=scenario)
}

#' @export 
make_directories = function(scenarios,methods){
  dir.create("data",showWarnings=FALSE)
  dir.create("output",showWarnings=FALSE)
  dir.create("results",showWarnings=FALSE)
  l_ply(scenarios,make_directories_scenario,methods=methods)
}

#' @title Make the data (inputs and meta) for a DSC for a particular seed and scenario
#'
#' @description Make the data (inputs and meta) for a DSC for a particular seed and scenario. THe subscript rs refers to the "repetition" of a "scenario".
#' I'm experiementing with naming conventions to try to make these easier to see, using . as a subscript to indicate
#' that that subscript is applied over. By this logic make_data_scenario should be make_data_.s
#' 
#' @param seed (vector or list of integers) the seeds for the pseudo-rng which identifies/indexes trials
#' @param scenario a list including elements fn and args, the function name for the datamaker to be used and additional arguments
#' @param overwrite boolean indicating whether to overwrite existing files; default is not to
#' 
#' @return data are saved in files in the data subdirectory
#' @export 
make_data_rs = function(seed,scenario,overwrite=FALSE){
  if(!file.exists(datafilename(seed,scenario)) | overwrite){
    data = do.call(scenario$fn,list(seed=seed,args=scenario$args))
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
#' @param seed (vector or list of integers) the seeds for the pseudo-rng which identifies/indexes trials
#' @param scenario a list including elements fn and args, the function name for the datamaker to be used and additional arguments
#' @param method a list including elements fn, name and args
#' 
#' @return none; output are saved in the output subdirectory
#' @export 
apply_method_rsm = function(seed, scenario, method){
  if(!file.exists(outputfilename(seed,scenario,method))){
    load(datafilename(seed,scenario))
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

#' @title Initialise empty dsc
#'
#' @description returns a list that is a dsc
#'
#' @param name a string containing the name for your dsc
#' 
#' @return nothing; simply deletes files
#' @export
init_dsc = function(name){
  list(methods=list(), scenarios=list(), scorefn=NULL, name=name) 
}

#' @title Add a scenario to a dsc
#'
#' @description Adds a scenario to a dsc
#'
#' @param dsc the dsc to add the sceanario to
#' @param name a character string 
#' @param datamaker the datamaker, a function that is the datamaker for the scenario
#' @param args a list of arguments to the datamaker
#' @param seed a vector of integers showing seeds to use for the scenario
#' 
#' @return nothing; simply deletes files
#' @export
addScenario = function(dsc,name, datamaker, args, seed){
  if(name %*% in names(dsc)){stop("Error: that scenario name already exists")}
  dsc[[name]]=list(name=name,fn=datamaker,args=args,seed=seed)
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
    reset_scenario(dsc$scenarios[[i]]$name,force)
  }
  for(i in 1:length(dsc$methods)){
    reset_method(dsc$methods[[i]]$name,force)
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
reset_method = function(methodname,force=FALSE){
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
    file.remove(Sys.glob(file.path("output","*",methodname,"*")))
    file.remove(Sys.glob(file.path("results","*",methodname,"*")))    
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
reset_scenario = function(scenarioname,force=FALSE){
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
    file.remove(Sys.glob(file.path("data",scenarioname,"*")))
    file.remove(Sys.glob(file.path("output",scenarioname,"*","*")))
    file.remove(Sys.glob(file.path("results",scenarioname,"*","*")))    
  }
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
  scorefn=dsc$scorefn
  if(!is.null(scenariosubset)){
    scenarionames=lapply(scenarios,function(x){return(x$name)})
    ssub = scenarionames %in% scenariosubset
  } else {ssub = rep(TRUE, length(scenarios))}
    
  if(!is.null(methodsubset)){
    methodnames=lapply(methods,function(x){return(x$name)})
    msub = methodnames %in% methodsubset
  } else { msub= rep(TRUE, length(methods))}
  
  make_directories(scenarios[ssub],methods[msub])
  
  make_data(scenarios[ssub])
  apply_methods(scenarios[ssub],methods[msub])
  
  score_methods(scenarios[ssub],methods[msub],scorefn)
  
      
  res=aggregate_results(scenarios[ssub],methods[msub])
  
  return(res)
}

#' @title List all scenarios for a DSC
#'
#' @description List all scenarios for a DSC
#'
#' @param scenarios a list of scenarios used to produce data=list(input,meta)
#' @return a list of names of the scenarios
#' @export
list_scenarios = function(dsc){
  get_name=function(x){return(x$name)}
  print(apply(dsc$scenarios,get_name))
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


