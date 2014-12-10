datamaker = function(x){
  seed=x$seed
  scenario=x$scenario
  outputfile=x$outputfile
  nsamp=x$nsamp
  set.seed(seed)
  
  #here is the meat of the function that needs to be defined for each dsc to be done
  if(scenario=="normal"){
    input = list(x=rnorm(nsamp,0,1))
    meta =  list(truemean=0)
  }
  
  if(scenario=="uniform"){
    input = list(x=runif(nsamp,-1,1))
    meta = list(truemean=0)
  }
  
  if(scenario=="Cauchy"){
    input = list(x=rt(nsamp,df=1))
    meta = list(truemean=0)
  }
  #end of meat of function
  
  data = list(meta=meta,input=input)
  save(data,file=outputfile)
  return(data)
}
