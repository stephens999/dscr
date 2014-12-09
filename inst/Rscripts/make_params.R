#!/usr/bin/env Rscript
# make_params.R
# usage is Rscript make_params.R parammaker.R seed scenario 
 
arguments <- commandArgs(trailingOnly=TRUE)
filename = arguments[1]
seed = as.integer(arguments[2])
scenario=arguments[3]
indexlist = list(seed=seed,scenario=scenario)

library("dscr")

source(filename) #filename should define a function parammaker
dir.create(param_subdir(indexlist),recursive=TRUE)
do.call(parammaker,list(indexlist))
