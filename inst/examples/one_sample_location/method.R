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

  methods=list()
  methods[[1]] = list(name="mean",fn =mean.wrapper,args=NULL)
  methods[[2]] = list(name="median",fn=median.wrapper,args=NULL)
  methods[[3]] = list(name="winsor",fn=winsor.wrapper,args=NULL)


