scenarios=list()
scenarios[[1]]=list(name="normal",fn=datamaker,args=list(disttype="normal",nsamp=1000),seed=1:100)
scenarios[[2]]=list(name="uniform",fn=datamaker,args=list(disttype="uniform",nsamp=1000),seed=1:100)
scenarios[[3]]=list(name="Cauchy",fn=datamaker,args=list(disttype="Cauchy",nsamp=1000),seed=1:100)


