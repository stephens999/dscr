#!/usr/bin/env Rscript
# run_method.R
# usage is Rscript run_method.R method.R seed scenario 

arguments <- commandArgs(trailingOnly=TRUE)
filename = arguments[1]
seed = as.integer(arguments[2])
scenario=arguments[3]
indexlist = list(seed=seed,scenario=scenario)

library("dscr")

source(filename) #filename should define the variable method, a list with components method$name and method$fn
#and should also define the function with name method$fn
dir.create(output_subdir(method$name,indexlist),recursive=TRUE)
load(datafile(indexlist))
output=do.call(method$fn,list(data$input))
save(output,file=outputfile(method$name,indexlist))
