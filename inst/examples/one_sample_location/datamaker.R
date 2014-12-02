datamaker = function(indexlist){
  load(file=paramfile(indexlist))
  set.seed(param$dataseed)
  
  #here is the meat of the function that needs to be defined for each dsc to be done
  if(indexlist$scenario=="normal"){
    input = list(x=rnorm(param$nsamp,0,1))
    meta =  list(truemean=0)
  }
  
  if(indexlist$scenario=="uniform"){
    input = list(x=runif(param$nsamp,-1,1))
    meta = list(truemean=0)
  }
  
  if(indexlist$scenario=="Cauchy"){
    input = list(x=rt(param$nsamp,df=1))
    meta = list(truemean=0)
  }
  #end of meat of function
  
  data = list(meta=meta,input=input)
  save(data,file=datafile(indexlist))
  return(data)
}
