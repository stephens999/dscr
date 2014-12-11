#!/usr/bin/env Rscript
# score_method.R
# usage is Rscript score_method.R score.R method.R seed scenario 

arguments <- commandArgs(trailingOnly=TRUE)
scorefilename = arguments[1]
methodfilename = arguments[2]
seed = as.integer(arguments[3])
scenario=arguments[4]
indexlist = list(seed=seed,scenario=scenario)

library("dscr")

source(scorefilename) #scorefilename should define the function score()
source(methodfilename) #filename should define the variable method, a list with components method$name and method$fn
#and should also define the function with name method$fn
dir.create(results_subdir(method$name,indexlist),recursive=TRUE)
load(paramfile(indexlist))
load(datafile(indexlist))
load(outputfile(method$name,indexlist))

results=do.call(score,list(param,data,output))
save(results,file=resultsfile(method$name,indexlist))