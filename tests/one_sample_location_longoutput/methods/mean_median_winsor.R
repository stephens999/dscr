library(psych)

mean.wrapper = function(input,args){
  return(list(meanest = mean(input$x),test=1:10000))  
}

median.wrapper = function(input,args){
  return(list(meanest = median(input$x),test=1:10000))    
}

winsor.wrapper = function(input,args){
  return(list(meanest = winsor.mean(input$x,trim=0.2),test=1:10000))
}

