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
#' @import plyr reshape2 knitr assertthat ggplot2 shiny
#' @importFrom magrittr "%>%"
NULL

#' @title return the path to a data file, parameter file, output file or results file
#'
#' @description return the path to a data file, parameter file, output file or results file
#' 
#' @param seed
#' @param scenario
#' 
#' @param datadir/paramdir/outputdir/scoresdir the (relative) path to the directory containing the relevant files
#' 
#' @return string containing path to file
input_file_name = function(dsc,seed,scenario,inputdir="input",inputtype=NULL){
  if(is.null(inputtype)){inputtype=scenario$inputtype}
  return(file.path(dsc$file.dir,inputdir,scenario$name,inputtype,paste0("input.",seed,".rds")))
}

#' @title return the path to a meta file, parameter file, output file or results file
#'
#' @description return the path to a meta file, parameter file, output file or results file
#' 
#' @param dsc
#' @param seed
#' @param scenario
#' 
#' @param inputdir/metadir/outputdir/scoresdir the (relative) path to the directory containing the relevant files
#' 
#' @return string containing path to file
meta_file_name = function(dsc,seed,scenario,metadir="meta",metatype=NULL){
  if(is.null(metatype)){metatype=scenario$metatype}
  return(file.path(dsc$file.dir,metadir,scenario$name,metatype,paste0("meta.",seed,".rds")))
}

output_file_name = function(dsc,seed, scenario, method, outputdir="output",outputtype=NULL){
  if(is.null(outputtype)){outputtype=method$outputtype}
  return(file.path(dsc$file.dir,outputdir,scenario$name,method$name,outputtype,paste0("output.",seed,".rds")))
}

time_file_name = function(dsc,seed, scenario, method, outputdir="output"){
  outputtype=method$outputtype
  return(file.path(dsc$file.dir,outputdir,scenario$name,method$name,outputtype,paste0("time.",seed,".rds")))
}

scores_file_name = function(dsc,seed, scenario, method, score=NULL, scoresdir="scores"){
  if(is.null(score)){scorename="defaultscore"} else {scorename=score$name}
  return(file.path(dsc$file.dir,scoresdir,scenario$name,method$name,scorename,paste0("scores.",seed,".rds")))
}



#' @title Get the results of a single method for a single trial
#'
#' @description Get the results of a single method for a single trial
#' 
#' @param seed
#' @param scenario
#' @param method
#' @param score
#' 
#' @return results, a data frame of results, the details will depend on the comparison being run
get_results_singletrial = function(dsc,seed,scenario,method,score){
  if(file.exists(scores_file_name(dsc,seed,scenario,method,score))){
    results=readRDS(file=scores_file_name(dsc,seed,scenario,method,score))
  } else {results = NULL}
  #convert NULL to NA to stop data.frame crashing
  results <- lapply(results, function(x)ifelse(is.null(x), NA, x))
  temp = c(seed=seed, scenario=scenario$name, method=method$name, results)
  class(temp)='data.frame'
  row.names(temp)=1
  #system.time({X<-(list(x='SomeText', y=200,z=1:10000)); class(X) <- 'data.frame'; row.names(X)<-1})
  return(temp)
}

#' @title return the data and output for a single method for a single trial
#'
#' @description return a list containing data and output for a single method,trial
#' @param seed
#' @param scenario
#' @param method
#' 
#' @return results list with components data, output
#' 
#' @export
load_example = function(dsc,seed, scenarioname,methodname){
  assert_that(is.numeric(seed))
  check_valid_name(dsc,scenarioname)
  check_valid_name(dsc,methodname)
  assert_that(methodname %in% get_method_names(dsc))
  assert_that(scenarioname %in% get_scenario_names(dsc))
  scenario=dsc$scenarios[[scenarioname]]
  method=dsc$methods[[methodname]]
  output= readRDS(file=output_file_name(dsc,seed,scenario,method))
  input = readRDS(file=input_file_name(dsc,seed,scenario))
  meta = readRDS(file =meta_file_name(dsc,seed,scenario))
  return(list(input=input, meta=meta, output=output))
}

#' @title Get the results of a single method for a single scenario
#'
#' @description  Get the results of a single method for a single scenario
#' 
#' @param scenario a scenario
#' @param method a method
#' @return a data frame of results, with one row for each trial. The details of the columns will depend on the comparison being run
get_results_scenario = function(dsc,scenario, method,score){  
  print(paste0("Getting results for scenario ",scenario$name," , method ",method$name))
  ldply(scenario$seed, get_results_singletrial, scenario=scenario, method=method,score=score,dsc=dsc)
}

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
aggregate_results = function(dsc,scenarios,methods,score){
  ldply(methods,get_results,scenarios=scenarios,score=score,dsc=dsc)
}

make_dirs = function(namelist){
  for(i in 1:length(namelist)){dir.create(namelist[i],recursive=TRUE,showWarnings=FALSE)}
}

make_directories = function(dsc){
  scenarionames = names(dsc$scenarios)
  methodnames = names(dsc$methods)
  scorenames = names(dsc$scores)
  outputtypes = get_output_types(dsc)
  inputtypes = get_input_types(dsc)
  metatypes = get_meta_types(dsc)
  
  #directories corresponding to scenario-method combinations
  smdirs = as.vector(outer(scenarionames,methodnames,file.path))
  
  # scenario-method-scoretype combos
  smsdirs = as.vector(outer(smdirs,scorenames,file.path))
  
  #scenario-method-outputtype combos
  smodirs = as.vector(outer(smdirs,outputtypes,file.path))
  
  #scenario-metatype combos
  sMdirs = as.vector(outer(scenarionames,metatypes,file.path))
  sIdirs = as.vector(outer(scenarionames,inputtypes,file.path))
  
  
  make_dirs(outer(file.path(dsc$file.dir,"meta"),sMdirs,file.path))
  make_dirs(outer(file.path(dsc$file.dir,"input"),sIdirs,file.path))
  make_dirs(outer(file.path(dsc$file.dir,"output"),smodirs,file.path))  
  make_dirs(outer(file.path(dsc$file.dir,"scores"),smsdirs,file.path))
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
source_dir <- function(path, trace = TRUE, ...) {
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
new_dsc = function(name,file.dir){
  dsc=new.env()
  dsc$methods=list()
  dsc$scenarios=list()
  dsc$scores=list()
  dsc$outputParsers=list()
  dsc$name=name 
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
add_scenario = function(dsc,name, fn, args=NULL, seed,metatype="default_meta",inputtype="default_input"){
  check_valid_name(dsc,name) 
  check_unique_name(dsc,name)
  assert_that(is.function(fn), is.list(args) | is.null(args),is.numeric(seed))
  dsc$scenarios[[name]]=list(name=name,fn=fn,args=args,seed=seed,metatype=metatype,inputtype=inputtype)
}

#' @title Add a method to a dsc
#'
#' @description Adds a method to a dsc
#'
#' @param dsc the dsc to add the method to
#' @param name a character string name for the method
#' @param fn, a wrapper function that implements the method
#' @param args a list of additional arguments to fn
#' @param outputtype a string to indicate what type of output
#' @param gold_flag a flag to indicate if the method is a "gold" method (which gets passed meta data as well as input data)
#' 
#' @return nothing, but modifies the dsc environment
#' @export
add_method = function(dsc,name, fn, args=NULL,outputtype="default_output",gold_flag=FALSE){
  check_valid_name(dsc,name) 
  check_unique_name(dsc,name)
  assert_that(is.function(fn), is.list(args) | is.null(args))
  dsc$methods[[name]]=list(name=name,fn=fn,args=args,outputtype = outputtype)
}

#' @title Add a score function to a dsc
#'
#' @description Adds a score to a dsc
#'
#' @param dsc the dsc to add the score to
#' @param fn, a score function
#' @param name string giving a name by which the score function is to be known by

#' @param outputtype, the type of output the score function takes, as generated by a outputParser 
#' 
#' @return nothing, but modifies the dsc environment
#' @export
add_score = function(dsc,fn,name="default_score",outputtype="default_output"){
  check_valid_name(dsc,name)
  check_unique_name(dsc,name)
  assert_that(is.function(fn))
 
  dsc$scores[[name]]=list(name=name,fn=fn,outputtype=outputtype)
}

#' @title return outputtypes of the methods in a dsc
#'
#' @description return outputtypes of the methods in a dsc
#'
#' @param dsc a dsc 
#' @return list of outputtypes
#' @export
get_output_types=function(dsc){
  lapply(dsc$methods,function(x){return(x$outputtype)})  
}

#' @title return inputtypes of the scenarios in a dsc
#'
#' @description return inputtypes of the scenarios in a dsc
#'
#' @param dsc a dsc 
#' @return list of inputtypes
get_input_types=function(dsc){
  lapply(dsc$scenarios,function(x){return(x$inputtype)})  
}

#' @title return metatypes of the scenarios in a dsc
#'
#' @description return metatypes of the scenarios in a dsc
#'
#' @param dsc a dsc 
#' @return list of inputtypes
get_meta_types=function(dsc){
  lapply(dsc$scenarios,function(x){return(x$metatype)})  
}

#' @title Add a outputParser to a dsc
#'
#' @description Adds a outputParser to a dsc; a outputParser converts one type of output to another type
#'
#' @param dsc the dsc to add the outputParser
#' @param name string giving a name by which the outputParser function is to be known by
#' @param fn, a outputParser function
#' @param outputtype_from, string naming the type of output the outputParser function takes as input 
#' @param outputtype_to, string naming the type of output the outputParser function gives as output 
#' @return nothing, but creates output files in output/outputtype_to subdir
#' @export
add_output_parser = function(dsc,name,fn,outputtype_from="default_output",outputtype_to){
  check_valid_name(dsc,name)  
  check_unique_name(dsc,name)
  assert_that(is.function(fn))
  assert_that(outputtype_from %in% get_output_types(dsc))  
  dsc$outputParsers[[name]]=list(name=name,fn=fn,outputtype_from=outputtype_from,outputtype_to=outputtype_to)
}

get_scenario_names = function(dsc){return(names(dsc$scenarios))}
get_method_names = function(dsc){return(names(dsc$methods))}
get_output_parser_names = function(dsc){return(names(dsc$outputParsers))}
get_score_names = function(dsc){return(names(dsc$scores))}

get_all_names = function(dsc){return(c(get_scenario_names(dsc),get_method_names(dsc),
                                     get_output_parser_names(dsc),get_score_names(dsc),
                                     get_output_types(dsc),get_input_types(dsc),
                                     get_meta_types(dsc)))}

check_valid_name=function(dsc,name){
  assert_that(is.character(name))
  assert_that(!grepl("[/\\:*\"?<>|]",name)) #exclude characters not allowed in filename
}

check_unique_name=function(dsc,name){
  assert_that(!(name %in% get_all_names(dsc))) #make all names unique
}

scenario_exists = function(dsc,scenarioname){return(scenarioname %in% names(dsc$scenarios))}
method_exists = function(dsc,methodname){return(methodname %in% names(dsc$methods))}
output_parser_exists = function(dsc,outputParsername){return(outputParsername %in% names(dsc$outputParsers))}
score_exists = function(dsc,scorename){return(scorename %in% names(dsc$scores))}
  
#' @title List scenarios
#'
#' @description List scenarios
#' @param dsc the dsc to have its scenarios listed
#' 
#' @return nothing
#' @export
list_scenarios = function(dsc){print(get_scenario_names(dsc))}

#' @title List methods
#'
#' @description List methods
#' @param dsc the dsc to have its methods listed
#' 
#' @return nothing
#' @export
list_methods = function(dsc){print(get_method_names(dsc))}

#' @title List outputParsers
#'
#' @description List outputParsers
#' @param dsc the dsc to have its outputParsers listed
#' 
#' @return nothing
#' @export
list_output_parsers = function(dsc){print(get_output_parser_names(dsc))}

#' @title List scores
#'
#' @description List scores
#' @param dsc a dsc 
#' 
#' @return nothing
#' @export
list_scores = function(dsc){print(get_score_names(dsc))}

run_output_parser_once = function(dsc,outputParsername,infile,outfile){
  assert_that(file.exists(infile),is.character(outputParsername))
  if(!file.exists(outfile)){
    outputParser=dsc$outputParsers[[outputParsername]]
    output1=readRDS(infile)
    output2 = do.call(outputParser$fn,list(output=output1))
    saveRDS(output2,file=outfile)
  } 
}

#' @title Run a outputParser in a dsc
#'
#' @description Run the named outputParser to convert one type of output to another type. 
#' The outputParser is run on all outputs in the appropriate output directory (output/outputtype_from)
#' where outputtype_from is defined when the outputParser is added to the dsc
#' 
#' @param dsc the dsc to use
#' @param outputParsername string giving the name of the outputParser to be run
#' 
#' @return nothing, but outputs files to output/ directories
run_output_parser = function(dsc,outputParsername){
  assert_that(is.character(outputParsername))
  assert_that(output_parser_exists(dsc,outputParsername))
  print(paste0("running outputParser ",outputParsername))  
  outputParser = dsc$outputParsers[[outputParsername]]
  from.dir = Sys.glob(file.path(dsc$file.dir,"output","*","*",outputParser$outputtype_from))
  from.filename = Sys.glob(file.path(dsc$file.dir,"output","*","*",outputParser$outputtype_from,"output*"))
  to.filename = gsub(outputParser$outputtype_from,outputParser$outputtype_to,from.filename)
  to.dir = gsub(outputParser$outputtype_from,outputParser$outputtype_to,from.dir)
  make_dirs(to.dir)
  mapply(run_output_parser_once,from.filename,to.filename,MoreArgs=list(dsc=dsc,outputParsername=outputParsername))
}

#' @title Run all outputParsers in a dsc
#'
#' @description Run all outputParsers to convert one type of output to another type. 
#' Each outputParser is run on all outputs in the appropriate output directory (output/outputtype_from)
#' where outputtype_from is defined when the outputParser is added to the dsc
#' 
#' @param dsc the dsc to use
#' 
#' @return nothing, but outputs files to output/ directories
run_output_parsers = function(dsc){
  if(!is.null(dsc$outputParsers)){
      mapply(run_output_parser,names(dsc$outputParsers),MoreArgs=list(dsc=dsc))
  }
}

run_scenario=function(dsc,seed,scenarioname){
  scenario = dsc$scenarios[[scenarioname]]
  if(!file.exists(input_file_name(dsc,seed,scenario))){
    data <- seeded_function_call(scenario$fn, seed, list(args = scenario$args))
    saveRDS(data$meta,file=meta_file_name(dsc,seed,scenario))
    saveRDS(data$input,file=input_file_name(dsc,seed,scenario))
  }   
}

run_scenarios=function(dsc,ssub=NULL,seedsubset=NULL){
  df = expand_dsc(dsc, 'scenarios') %>%
    multiple_filter(scenarioname = ssub, seed = seedsubset)
  print(paste0("running Scenarios"))
  mapply(run_scenario,seed=df$seed,scenarioname=df$scenarioname,MoreArgs=list(dsc=dsc))
  #reg1 <- makeRegistry(id="my_reg1", seed=123, file.dir="my_job_dir1")
  #batchMap(reg1, run_scenario,seed=df$seed,scenarioname=df$scenarioname,more.args=list(dsc=dsc))
  #ids <- getJobIds(reg1)
  #submitJobs(reg1, ids)  
}

#' @title Score a method on a single trial and save results
#'
#' @description tries to deal with scores that don't have names
#' 
#' @param score, the vector of scores with some elements possibly named
#'
#' @return a vector of names for a score. For elements of score already named, the name stays the same; for unnamed elements the name is scorej where j is the index of the element
make_nice_score_names = function(s){  
  paste0(ifelse(names(s)=="","score",""),ifelse(names(s)=="",1:length(s),names(s)))
}

#' @title Score a method on a single trial and save results
#'
#' @description Score results of a single method for a single trial and produce (and save) corresponding results
#' 
#' @param seed the seed to score
#' @param scenarioname the scenario to score
#' @param methodname the method to score
#' @param scorename the score to use
#'
#' @return results, a list of appropriate format to be determined by the comparison being run (maybe required to be a dataframe?)
run_score = function(dsc,seed,scenarioname,methodname,scorename){
  assert_that(is.numeric(seed),is.character(scenarioname),is.character(methodname),is.character(scorename))
  assert_that(scenario_exists(dsc,scenarioname),method_exists(dsc,methodname),score_exists(dsc,scorename))
  
  score = dsc$scores[[scorename]]
  scenario=dsc$scenarios[[scenarioname]]
  method = dsc$methods[[methodname]]
  
  if(file.exists(output_file_name(dsc,seed,scenario,method,outputtype=score$outputtype))){
    if(!file.exists(scores_file_name(dsc,seed,scenario,method,score))){
      data=list(input=readRDS(file=input_file_name(dsc,seed,scenario)),
                meta=readRDS(file=meta_file_name(dsc,seed,scenario)))
      output=readRDS(file=output_file_name(dsc,seed,scenario,method,outputtype=score$outputtype)) #also loads timedata
      timedata=readRDS(file=time_file_name(dsc,seed,scenario,method))
      res=score$fn(data,output)
      names(res)=make_nice_score_names(res)
      results=c(res,as.list(timedata))
      saveRDS(results,file=scores_file_name(dsc,seed,scenario,method,score))
    }
  }
}

run_scores=function(dsc,ssub=NULL,msub=NULL,scoresub=NULL){
  ## TODO: Should there be a seed subset here too?
  df = expand_dsc(dsc, 'scenarios_methods_scores') %>%
    multiple_filter(scenarioname = ssub, methodname = msub,
                    scorename = scoresub)
  print(paste0("running Scores"))
  mapply(run_score,seed=df$seed,scenarioname=df$scenarioname,methodname=df$methodname,scorename=df$scorename,MoreArgs=list(dsc=dsc)) 
}

run_method=function(dsc,seed,scenarioname,methodname){
  print(paste0("running method ",methodname,", on scenario ", scenarioname, ", seed ",seed))
  scenario = dsc$scenarios[[scenarioname]]
  method = dsc$methods[[methodname]]
  if(!file.exists(output_file_name(dsc,seed,scenario,method))){
    if(method$gold_flag){
      input_frame <- data.frame(file_names = input_file_name(dsc, seed, scenario),
                              variable_names = 'input')
    }
    else{
      input_frame <- data.frame(file_names = 
                                  c(input_file_name(dsc, seed, scenario),
                                    meta_file_name(dsc, seed, scenario)),
                                   variable_names = c('input','meta'))
    }
  
    timedata <- system.time(
      output <- seeded_function_call(method$fn,
                                     seed + 1,
                                     list(args = method$args),
                                     input_frame)
    )
    saveRDS(output,file=output_file_name(dsc,seed,scenario,method))
    saveRDS(timedata,file=time_file_name(dsc,seed,scenario,method))
  }
}

run_methods=function(dsc,ssub=NULL,msub=NULL,seedsubset=NULL){
  df = expand_dsc(dsc, 'scenarios_methods') %>%
    multiple_filter(scenarioname = ssub, methodname = msub, seed = seedsubset)
  
  print(paste0("running Methods"))
  mapply(run_method,seed=df$seed,scenarioname=df$scenarioname,methodname=df$methodname,MoreArgs=list(dsc=dsc))
  #reg2 <- makeRegistry(id="my_reg2", seed=123, file.dir="my_job_dir2")
  #batchMap(reg2, run_method,seed=df$seed,scenarioname=df$scenarioname,methodname=df$methodname,more.args=list(dsc=dsc))
  #ids <- getJobIds(reg2)
  #submitJobs(reg2, ids)  
}

#' @title Removes all data, output and results for the dsc
#'
#' @description Removes all files in scores/ meta/, input/ and output/ subdirectories. Mostly useful for testing purposes.
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

#' @title Removes all output and scoress for a method
#'
#' @description Removes all output and scores for a method; primary intended purpose is to force re-running of that method.
#'
#' @param method string indicating name of methods to remove output
#' @param force boolean, indicates whether to proceed without prompting user (prompt is to be implemented)
#' 
#' @return nothing; simply deletes files
#' @export
reset_method = function(dsc,methodname,force=FALSE){
  assert_that(is.character(methodname))
      
  if(!force){
    if(interactive()){
      force = (readline("Are you sure? [y to confirm]:")=="y")
    } else {
      stop("Error: Must specify force = TRUE in reset_method in non-interactive mode.")
    } 
  }
  
  if(force){
    file.remove(Sys.glob(file.path(dsc$file.dir,"output","*",methodname,"*","*")))
    file.remove(Sys.glob(file.path(dsc$file.dir,"scores","*",methodname,"*","*")))    
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
  assert_that(is.character(scenarioname))
        
  if(!force){
    if(interactive()){
    force = (readline("Are you sure? [y to confirm]:")=="y")
    } else {
      stop("Error: Must specify force = TRUE in reset_scenario in non-interactive mode.")
    } 
  }
  
  if(force){
    file.remove(Sys.glob(file.path(dsc$file.dir,"meta",scenarioname,"*","*")))
    file.remove(Sys.glob(file.path(dsc$file.dir,"input",scenarioname,"*","*")))
    file.remove(Sys.glob(file.path(dsc$file.dir,"output",scenarioname,"*","*","*")))
    file.remove(Sys.glob(file.path(dsc$file.dir,"scores","*",scenarioname,"*","*")))    
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
#' @param seedsubset a vector of which seeds to run. Eg 1:2 would do seeds 1 and 2 (for scenarios that use that seed)
#'
#' @return data frame of results from all methods run on all scenarios
#' @export
run_dsc=function(dsc,scenariosubset=NULL, methodsubset=NULL,seedsubset=NULL){
  scenarios=dsc$scenarios
  methods=dsc$methods
  
  if(!is.null(scenariosubset)){
    scenarionames=lapply(scenarios,function(x){return(x$name)})
    ssub = scenarionames %in% scenariosubset
  } else {ssub = rep(TRUE, length(scenarios))}
    
  if(!is.null(methodsubset)){
    methodnames=lapply(methods,function(x){return(x$name)})
    msub = methodnames %in% methodsubset
  } else { msub= rep(TRUE, length(methods))}
  
  make_directories(dsc)
  
  run_scenarios(dsc,scenariosubset,seedsubset)
  run_methods(dsc,scenariosubset,methodsubset,seedsubset)
  run_output_parsers(dsc)
  run_scores(dsc,scenariosubset,methodsubset)
  
  #  make_data(scenarios[ssub])
#  apply_methods(scenarios[ssub],methods[msub])
  
#  score_methods(dsc,scenarios[ssub],methods[msub],score)
  
  if(length(dsc$scores)>1){
    dsc$res=lapply(dsc$scores,aggregate_results,
             dsc=dsc,scenarios=scenarios[ssub],methods=methods[msub])
  } else {
    dsc$res=aggregate_results(dsc=dsc,scenarios=scenarios[ssub],methods=methods[msub],dsc$scores[[1]])
  }
  #if only one score, return a simple data frame rather than list of dataframes
  #if(length(dsc$res)==1){dsc$res=dsc$res[[1]]}
  return(dsc$res)
}


#' @title plot results for DSC
#'
#' @description interactive plot for results of DSC
#'
#' @param res
#' @return a shiny plot
#' @export
shiny_plot=function(res){
  scenario_names = as.character(unique(res$scenario))
  method_names = as.character(unique(res$method))
  numeric_criteria = names(res)[unlist(lapply(res,is.numeric))]

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
        res.filter = dplyr::filter(res,scenario %in% input$scen.subset & method %in% input$method.subset)
        print(input)
        res.filter$value = res.filter[[input$criteria]]
        ggplot(res.filter,aes(method,value,color=method)) + geom_boxplot() + facet_grid(.~scenario)
      })
    }
  )
  
  shinyApp(ui=ui,server=server)
}
