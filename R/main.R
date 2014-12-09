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
#' @param indexlist list that ``indexes" the comparison being used, with components
#' \itemize{
#' \item{"seed"}{ the seed for the pseudo-rng which identifies/indexes the file}
#' \item{"scenario"}{ string indicating scenario name}
#' }
#' 
#' @param datadir/paramdir/outputdir/resultsdir the (relative) path to the directory containing the relevant files
#' 
#' @return string containing path to file
#' 
#' @export
datafile = function(indexlist,datadir="data"){
  return(file.path(datadir,indexlist$scenario,paste0("data.",indexlist$seed,".RData")))
}

#' @export
data_subdir = function(indexlist,datadir="data"){
  return(file.path(datadir,indexlist$scenario))
}

#' @export
paramfile = function(indexlist,paramdir="param"){
  return(file.path(paramdir,indexlist$scenario,paste0("param.",indexlist$seed,".RData")))
}

#' @export
param_subdir = function(indexlist,paramdir="param"){
  return(file.path(paramdir,indexlist$scenario))
}

#' @export
outputfile = function(methodname,indexlist,flavor=NULL, outputdir="output"){
  methodname=long_methodname(methodname,flavor)
  return(file.path(outputdir,indexlist$scenario,methodname,paste0("output.",indexlist$seed,".RData")))
}

#' @export
output_subdir = function(methodname,indexlist,flavor=NULL, outputdir="output"){
  methodname=long_methodname(methodname,flavor)
  return(file.path(outputdir,indexlist$scenario,methodname))
}

#' @export
resultsfile = function(methodname,indexlist,flavor=NULL, resultsdir="results"){
  methodname=long_methodname(methodname,flavor)    
  return(file.path(resultsdir,indexlist$scenario,methodname,paste0("results.",indexlist$seed,".RData")))
}

#' @export
results_subdir = function(methodname,indexlist,flavor=NULL, resultsdir="results"){
  methodname=long_methodname(methodname,flavor)    
  return(file.path(resultsdir,indexlist$scenario,methodname))
}

#' @title combine a method name and flavor to produce a new method name 
#'
#' @description combine a method name and flavor to produce a new method name 
#' 
#' @param methodname (string) name of a method
#' @param flavor (sting) name of a flavor
#' 
#' @return string method.flavor, or if flavor is null then just method
#' 
#' @export
long_methodname=function(methodname,flavor=NULL){
  if(is.null(flavor)){ 
    return(methodname)
  }
  else{
    return(paste(methodname,flavor,sep="."))
  }
}

#' @title Apply a method to an input and produce output
#'
#' @description Apply a single method to a single input trial and produce (and save) corresponding output
#' 
#' @param indexlist list that ``indexes" the comparison being used, with components
#' \itemize{
#' \item{seed}{the seed for the pseudo-rng which identifies/indexes the file}
#' \item{scenario}{string indicating scenario name}
#' }
#' 
#' @param method a list with elements
#' \itemize{
#' \item{methodname}{string by which method should be identified}
#' \item{methodfn}{name of function that is used to call the method}
#' }
#' 
#' @param flavor a string indicating which element of methods$flavorlist to use as additional arguments 
#' 
#' @return output a list of appropriate format to be determined by the comparison being run
#' 
#' 
#' @export
apply_method_singletrial=function(indexlist,method,flavor=NULL){
  load(file=datafile(indexlist))
  if(!is.null(flavor)){
    output=do.call(method$fn,list(input=data$input,add.args=method$flavorlist$flavor))
  } else {
    output=do.call(method$fn,list(input=data$input))
  }
  save(output,file=outputfile(method$name,indexlist,flavor=flavor))
  return(output)
}

#' @title Apply a method to all inputs and produce corresponding outputs
#'
#' @description Apply a method to all inputs and produce corresponding outputs (by repeated application of apply_method_once).
#' Results are saved in a file
#'  
#' @param scenario_seedlist (list), one element for each scenario, each element contains the vector of seeds for the pseudo-rng which identifies/indexes trials
#' @param method a list with elements
#' \itemize{
#' \item{methodname}{string by which method should be identified}
#' \item{methodfn}{name of function that is used to call the method}  
#' }
#' 
#' @return none - the results are saved in a file determined by outputfile(seed, method$methodname)
#' 
#' 
#' @export
apply_method = function(scenario_seedlist,method,flavor=NULL){
  combo = melt(scenario_seedlist,value.name="seed")
  names(combo)[2]="scenario"  
  d_ply(combo,.(seed,scenario),apply_method_singletrial,method=method,flavor=flavor)
}

#' @title Score a method on a single trial and save results
#'
#' @description Score results of a single method for a single trial and produce (and save) corresponding results
#' 
#' @param indexlist list that ``indexes" the comparison being used, with components
#' \itemize{
#' \item{seed}{the seed for the pseudo-rng which identifies/indexes the file}
#' \item{scenario}{string indicating scenario name}
#' }
#' 
#' @param method a list with elements
#' \itemize{
#' \item{methodname}{string by which method should be identified}
#' \item{methodfn}{name of function that is used to call the method}
#' }
#' 
#' @param scorefn a function that scores output based on comparisons with input, parameters and metadata
#'
#' @return results, a list of appropriate format to be determined by the comparison being run (maybe required to be a dataframe?)
#' 
#' @export
score_method_singletrial = function(indexlist,method,scorefn,flavor=NULL){
  load(file=paramfile(indexlist))
  load(file=datafile(indexlist))
  load(file=outputfile(method$name,indexlist,flavor))
  
  results=scorefn(param,data,output)
  save(results,file=resultsfile(method$name,indexlist,flavor))
  return(results)
}

#' @title Score a method on all trials and save results
#'
#' @description Score a method on all trials and save results (by repeated application of score_method_singletrial)
#' 
#' @param scenario_seedlist (list) the seeds for the pseudo-rng for each scenario which identifies/indexes trials
#' @param method a list with elements
#' \itemize{
#' \item{methodname}{string by which method should be identified}
#' \item{methodfn}{name of function that is used to call the method}
#' }
#' 
#' @param scorefn a function that scores output based on comparisons with input, parameters and metadata
#'
#' @return results, a list of appropriate format to be determined by the comparison being run (maybe required to be a dataframe?)
#' 
#' @export
score_method=function(scenario_seedlist,method,scorefn,flavor=NULL){
  combo = melt(scenario_seedlist,value.name="seed")
  names(combo)[2]="scenario"
  d_ply(combo,.(seed,scenario),score_method_singletrial,method=method,scorefn=scorefn,flavor=flavor)
}





#' @title Get the results of a single method for a single trial
#'
#' @description Get the results of a single method for a single trial
#' 
#' @param indexlist list that ``indexes" the comparison being used, with components
#' \itemize{
#' \item{seed}{the seed for the pseudo-rng which identifies/indexes the file}
#' \item{scenario}{string indicating scenario name}
#' }
#' 
#' @param method a list with elements
#' \itemize{
#' \item{methodname}{string by which method should be identified}
#' \item{methodfn}{name of function that is used to call the method}  
#' }
#' @return results, a data frame of results, the details will depend on the comparison being run
#' 
#' @export

get_one_result = function(indexlist,method,flavor=NULL){
  load(file=resultsfile(method$name,indexlist,flavor))
  return(data.frame(results))
}

#' @title Get the results of a single method for multiple trials (one flavor)
#'
#' @description Get the results of a single method for multiple trials (one flavor)
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
get_results = function(method,scenario_seedlist,flavor=NULL){
  if(is.null(flavor)){
    flavorname="NA"
  } else {
    flavorname=flavor
  }
  combo = melt(scenario_seedlist,value.name="seed")
  names(combo)[2]="scenario"
  
  data.frame(method=method$name,flavor=flavorname,ddply(combo,.(seed,scenario),get_one_result,method=method,flavor=flavor))  
}


#' @export 
get_results_all_flavors = function(method,scenario_seedlist){
  if(is.null(method$flavorlist)){
    return(get_results(method,scenario_seedlist))
  } else {
    return(ldply(names(method$flavorlist),get_results,method=method,scenario_seedlist=scenario_seedlist))
  }  
}

#' @title Aggregate the results of multiple methods for multiple trials
#'
#' @description Aggregate the results of multiple methods for multiple trials
#' 
#' @param methodslist a list of methods. Each method is itself a list with elements
#' \itemize{
#' \item{methodname}{string by which method should be identified}
#' \item{methodfn}{name of function that is used to call the method}  
#' \item{flavorlist}{list of flavors of the method (each flavor is a list containing additional parameters to be passed to methodfn)}
#' }
#' @param seed (list of integers) the seeds for the pseudo-rng which identifies/indexes trials
#' @return a data frame of results, with one row for each trial/method combination. The details of the columns will depend on the comparison being run
#' 
#' 
#' 
#' @export 
# aggregate all the results into a data frame
# seed is a vector of seeds to use
# methods is a list of methods to use
aggregate_results = function(methodslist,scenario_seedlist){
  ldply(methodslist,get_results_all_flavors,scenario_seedlist=scenario_seedlist)
}


#' @export 
make_directories_singlemethod_singleflavor = function(method,flavor=NULL,scenario=NULL){
  if(is.null(scenario)) scenario = "default_scenario" 
  system(paste0("mkdir ",file.path("output",scenario,long_methodname(method$name,flavor))))
  system(paste0("mkdir ",file.path("results",scenario,long_methodname(method$name,flavor))))
}

#' @export 
make_directories_singlemethod = function(method,scenario=NULL){
  if(is.null(method$flavorlist)){
    make_directories_singlemethod_singleflavor(method=method,scenario=scenario)  
  } else {
    lapply(names(method$flavorlist),make_directories_singlemethod_singleflavor,method=method,scenario=scenario)
  }
}

#' @export 
make_scenario_subdirectories_singlescenario=function(methodslist,scenario=NULL){
  if(is.null(scenario)) scenario = "default_scenario" 
  system(paste0("mkdir ",file.path("param",scenario)))
  system(paste0("mkdir ",file.path("data",scenario)))
  system(paste0("mkdir ",file.path("output",scenario)))
  system(paste0("mkdir ",file.path("results",scenario)))
  l_ply(methodslist,make_directories_singlemethod,scenario)
}

#' @export 
make_directories = function(methodslist,scenario=NULL){
  system("mkdir param")
  system("mkdir data")
  system("mkdir output")
  system("mkdir results")
  if(is.null(scenario)) scenario="default_scenario"
  l_ply(scenario,make_scenario_subdirectories_singlescenario,methodslist=methodslist)
}


#' @title Make all the parameters for a DSC
#'
#' @description Make all the parameters for DSC using all combinations of seed and scenario
#' 
#' @param parammaker a function for making parameters from seeds and scenario combinations
#' @param scenario_seedlist a list of integer vectors, one for each scenario. Each list gives the seeds to be used for that scenario.
#' 
#' @return parameters are saved in files in the params subdirectory
#' @export 
make_params = function(parammaker,scenario_seedlist){
  combo = melt(scenario_seedlist,value.name="seed")
  names(combo)[2]="scenario"
  d_ply(combo,.(seed,scenario),parammaker)
}


#' @title Make all the inputs for a DSC
#'
#' @description Make all the parameters for DSC using all combinations of seed and scenario
#' 
#' @param parammaker a function for making parameters from seeds and scenario combinations
#' @param seed (list of integers) the seeds for the pseudo-rng which identifies/indexes trials
#' @param scenario (list of strings) the names of the scenarios 
#' 
#' @return parameters are saved in files in the params subdirectory
#' @export 
make_data = function(datamaker,scenario_seedlist){
  combo = melt(scenario_seedlist,value.name="seed")
  names(combo)[2]="scenario"
  d_ply(combo,.(seed,scenario),datamaker)
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
run_dsc=function(parammaker,datamaker,methods,scorefn,scenario_seedlist){
  make_directories(methods,names(scenario_seedlist))
  make_params(parammaker,scenario_seedlist)
  make_data(datamaker,scenario_seedlist)
  
  l_ply(methods,apply_method,scenario_seedlist=scenario_seedlist)
  l_ply(methods,score_method,scenario_seedlist = scenario_seedlist,scorefn=scorefn)
    
  res=aggregate_results(methods,scenario_seedlist)
  
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
