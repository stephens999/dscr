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
