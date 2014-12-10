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
output_subdir = function(methodname,indexlist,flavor=NULL, outputdir="output"){
  methodname=long_methodname(methodname,flavor)
  return(file.path(outputdir,indexlist$scenarioname,methodname))
}

#' @export
resultsfilename = function(seed, scenario, method, resultsdir="results"){
  return(file.path(resultsdir,scenario$name,method$name,paste0("results.",seed,".RData")))
}

#' @export
results_subdir = function(methodname,indexlist,flavor=NULL, resultsdir="results"){
  methodname=long_methodname(methodname,flavor)    
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
  load(file=datafilename(seed,scenario))
  load(file=outputfilename(seed,scenario,method))
  results=scorefn(data,output)
  save(results,file=resultsfilename(seed,scenario,method))
  return(results)
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
  return(data.frame(seed=seed, scenario=scenario$name, method=method$name, results))
}

#' @title Get the results of a single method for a single scenario
#'
#' @description  Get the results of a single method for a single scenario
#' 
#' @param method a list with elements
#' \itemize{
#' \item{methodname}{string by which method should be identified}
#' \item{methodfn}{name of function that is used to call the method}
#' }
#' @param seed (list of integers) the seeds for the pseudo-rng which identifies/indexes trials
#' @return a data frame of results, with one row for each trial. The details of the columns will depend on the comparison being run
#' 
#'
#' @export 
get_results_scenario = function(scenario, method){  
  ldply(scenario$seed, get_results_singletrial, scenario=scenario, method=method)
}


#' @export 
get_results = function(scenarios,method){
  ldply(scenarios, get_results_scenario, method=method)
}

#' @title Aggregate the results of multiple methods for multiple trials
#'
#' @description Aggregate the results of multiple methods for multiple trials
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
  system(paste0("mkdir ",file.path("output",scenario$name,method$name)))
  system(paste0("mkdir ",file.path("results",scenario$name,method$name)))
}


#' @export 
make_directories_scenario=function(scenario,methods){ 
  system(paste0("mkdir ",file.path("data",scenario$name)))
  system(paste0("mkdir ",file.path("output",scenario$name)))
  system(paste0("mkdir ",file.path("results",scenario$name)))
  l_ply(methods,make_directories_method,scenario=scenario)
}

#' @export 
make_directories = function(scenarios,methods){
  system("mkdir data")
  system("mkdir output")
  system("mkdir results")
  l_ply(scenarios,make_directories_scenario,methods=methods)
}

#' @title Make the data (inputs and meta) for a DSC for a particular seed and scenario
#'
#' @description Make the data (inputs and meta) for a DSC for a particular seed and scenario
#' 
#' @param seed (vector or list of integers) the seeds for the pseudo-rng which identifies/indexes trials
#' @param scenario a list including elements fn and args, the function name for the datamaker to be used and additional arguments
#' 
#' @return data are saved in files in the data subdirectory
#' @export 
make_data_singletrial = function(seed,scenario){
  data = do.call(scenario$fn,list(seed=seed,args=scenario$args))
  save(data,file=datafilename(seed,scenario))
}
#' @title Make the data (inputs and meta) for a DSC for a particular scenario
#'
#' @description Make the data (inputs and meta) for a DSC for a particular scenario
#' 
#' @param scenario a list including elements fn and args, the function name for the datamaker to be used and additional arguments
#' @param seed (list of integers) the seeds for the pseudo-rng which identifies/indexes trials
#' 
#' @return data are saved in files in the data subdirectory
#' @export 
make_data_scenario = function(scenario){
  lapply(scenario$seed,make_data_singletrial,scenario=scenario)
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
#' @description Apply a method for a particular seed and scenario
#' 
#' @param seed (vector or list of integers) the seeds for the pseudo-rng which identifies/indexes trials
#' @param scenario a list including elements fn and args, the function name for the datamaker to be used and additional arguments
#' 
#' @return none; output are saved in the output subdirectory
#' @export 
apply_method_singletrial = function(seed, scenario, method){
  load(datafilename(seed,scenario))
  output = do.call(method$fn,list(input=data$input,args=method$args))
  save(output,file=outputfilename(seed,scenario,method))
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
apply_method_scenario = function(scenario,method){
  lapply(scenario$seed,apply_method_singletrial,scenario=scenario,method=method)
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
apply_method = function(scenarios,method){
  lapply(scenarios,apply_method_scenario,method=method)
}


#' @title Run all methods on all scenarios for a DSC
#'
#' @description Run all methods on all scenarios for a DSC
#'
#' @param parammaker a function for making parameters from seeds and scenario combinations
#' @param datamaker a function for making data=list(meta,input) for a dsc
#' @param methods a list of methods to be used in the dsc
#' @param scorefn a function that takes output and scores it against the input, metadata and params
#' @param scenario_seedlist named list of seeds to be used in each scenario. 
#' 
#' @return data frame of results from all methods run on all scenarios
#' @export 
run_dsc=function(scenarios,methods,scorefn){
  scen.names=unlist(lapply(scenarios,function(x){return(x$name)}))
  make_directories(scenarios,methods)
  make_data(scenarios)
  
  for(i in 1: length(methods)){
    apply_method(scenarios,methods[[i]])
  }
  
  for(i in 1: length(methods)){
    score_method(scenarios,methods[[i]],score)
  }
      
  res=aggregate_results(scenarios,methods)
  
  return(res)
}

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
                            paste0('\tR -q -e \'library(\"dscr\"); source(\"parammaker.R\"); source(\"datamaker.R\"); source(\"method.R\"); source(\"score.R\"); source(\"scenario.R\"); source(\"rundsc.R\")\';\\'), 
                            sep = "\n")
  } else {
    run_methods_str = rep(NA, length(run_methods))
    run_methods_str[1L] = paste(paste0('\tcd inst/examples/', run_methods[1L], 
                                       ' || { echo "Running method: ', run_methods[1L], 
                                       ' failed\"; exit 1; } ;\\'),
                                paste0('\tR -q -e \'library(\"dscr\"); source(\"parammaker.R\"); source(\"datamaker.R\"); source(\"method.R\"); source(\"score.R\"); source(\"scenario.R\"); source(\"rundsc.R\")\';\\'), 
                                sep = "\n")
    
    run_methods_str[-1L] = paste(paste0('\tcd ../', run_methods[-1L], 
                                        ' || { echo "Running method: ', run_methods[-1L], 
                                        ' failed\"; exit 1; } ;\\'),
                                 paste0('\tR -q -e \'library(\"dscr\"); source(\"parammaker.R\"); source(\"datamaker.R\"); source(\"method.R\"); source(\"score.R\"); source(\"scenario.R\"); source(\"rundsc.R\")\';\\'), 
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
                                  paste0('\tR -q -e \'unlink(setdiff(list.files(), c(\"parammaker.R\", \"datamaker.R\", \"method.R\", \"score.R\", \"scenario.R\", \"rundsc.R\")), recursive=TRUE)\';\\'), 
                                  sep = "\n")
  } else {
    run_methods_clean_str = rep(NA, length(run_methods))
    run_methods_clean_str[1L] = paste(paste0('\tcd inst/examples/', run_methods[1L], 
                                             ' || { echo "Cleaning method: ', run_methods[1L], 
                                             ' failed\"; exit 1; } ;\\'),
                                      paste0('\tR -q -e \'unlink(setdiff(list.files(), c(\"parammaker.R\", \"datamaker.R\", \"method.R\", \"score.R\", \"scenario.R\", \"rundsc.R\")), recursive=TRUE)\';\\'), 
                                      sep = "\n")
    
    run_methods_clean_str[-1L] = paste(paste0('\tcd ../', run_methods[-1L], 
                                              ' || { echo "Cleaning method: ', run_methods[-1L], 
                                              ' failed\"; exit 1; } ;\\'),
                                       paste0('\tR -q -e \'unlink(setdiff(list.files(), c(\"parammaker.R\", \"datamaker.R\", \"method.R\", \"score.R\", \"scenario.R\", \"rundsc.R\")), recursive=TRUE)\';\\'), 
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
