library(psych)

mean.wrapper = function(input){
  return(list(meanest = mean(input$x)))  
}

median.wrapper = function(input){
  return(list(meanest = median(input$x)))    
}

winsor.wrapper = function(input){
  return(list(meanest = winsor.mean(input$x,trim=0.2)))
}

methods=list()
methods$mean = list(name="mean",fn = "mean.wrapper")
methods$median = list(name="median",fn="median.wrapper")
methods$winsor = list(name="winsor",fn="winsor.wrapper")


