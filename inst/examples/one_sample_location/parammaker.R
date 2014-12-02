parammaker = function(indexlist){
  set.seed(indexlist$seed)
  
  #here is the meat of the function that needs to be defined for each dsc to be done
  param = list(nsamp=1000, dataseed=indexlist$seed+1)
  # end meat of function
  
  
  save(param,file=paramfile(indexlist))
  return(param)
}
