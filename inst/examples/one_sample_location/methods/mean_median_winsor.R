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

