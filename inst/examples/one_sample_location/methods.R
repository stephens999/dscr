sourceDir("methods")
methods=list()
  methods[[1]] = list(name="mean",fn =mean.wrapper,args=NULL)
  methods[[2]] = list(name="median",fn=median.wrapper,args=NULL)
  methods[[3]] = list(name="winsor",fn=winsor.wrapper,args=NULL)


