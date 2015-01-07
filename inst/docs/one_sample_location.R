## ------------------------------------------------------------------------
library(dscr)
#  install_github(repo="stephens999/dscr")

## ------------------------------------------------------------------------
datamaker = function(seed,args){

  set.seed(seed)
    
  nsamp=args$nsamp
  disttype=args$disttype

  #here is the meat of the function that needs to be defined for each dsc to be done
  if(disttype=="normal"){
    input = list(x=rnorm(nsamp,0,1))
    meta =  list(truemean=0)
  }
  
  if(disttype=="uniform"){
    input = list(x=runif(nsamp,-1,1))
    meta = list(truemean=0)
  }
  
  if(disttype=="Cauchy"){
    input = list(x=rt(nsamp,df=1))
    meta = list(truemean=0)
  }
  #end of meat of function
  
  data = list(meta=meta,input=input)
  
  return(data)
}

## ------------------------------------------------------------------------
scenarios=list()
scenarios[[1]]=list(name="normal",fn=datamaker,args=list(disttype="normal",nsamp=1000),seed=1:100)
scenarios[[2]]=list(name="uniform",fn=datamaker,args=list(disttype="uniform",nsamp=1000),seed=1:100)
scenarios[[3]]=list(name="Cauchy",fn=datamaker,args=list(disttype="Cauchy",nsamp=1000),seed=1:100)

## ------------------------------------------------------------------------
library(psych)

mean.wrapper = function(input,args){
  return(list(meanest = mean(input$x)))  
}

median.wrapper = function(input,args){
  return(list(meanest = median(input$x)))    
}

winsor.wrapper = function(input,args){
  return(list(meanest = winsor.mean(input$x,trim=0.2)))
}


## ------------------------------------------------------------------------
  methods=list()
  methods[[1]] = list(name="mean",fn =mean.wrapper,args=NULL)
  methods[[2]] = list(name="median",fn=median.wrapper,args=NULL)
  methods[[3]] = list(name="winsor",fn=winsor.wrapper,args=NULL)

## ------------------------------------------------------------------------
score = function(data, output){
  return(list(squared_error = (data$meta$truemean-output$meanest)^2, abs_error = abs(data$meta$truemean-output$meanest)))
}

## ------------------------------------------------------------------------
  library(dscr)
  res=run_dsc(scenarios,methods,score)

## ------------------------------------------------------------------------
  head(res)

## ------------------------------------------------------------------------
  aggregate(abs_error~method+scenario,res,mean)
  aggregate(squared_error~method+scenario,res,mean)

## ------------------------------------------------------------------------
  trimmedmean.wrapper = function(input,args){
    return(list(meanest=mean(input$x,trim=args$trim)))
  }

  methods[[4]] = list(name="trimmedmean1",fn = trimmedmean.wrapper,args=list(trim=0.2))
  methods[[5]] = list(name="trimmedmean2",fn = trimmedmean.wrapper,args=list(trim=0.4))

  res=run_dsc(scenarios, methods, score)
  aggregate(abs_error~method+scenario,res,mean)
  aggregate(squared_error~method+scenario,res,mean)

## ------------------------------------------------------------------------
  res = run_dsc(scenarios, methods, score, c("Cauchy","normal"),c("trimmedmean1"))
  aggregate(abs_error~method+scenario,res,mean)
  aggregate(squared_error~method+scenario,res,mean)

