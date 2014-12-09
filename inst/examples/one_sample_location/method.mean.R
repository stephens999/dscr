mean.wrapper = function(input){
  return(list(meanest = mean(input$x)))  
}

method=list(name="mean",fn="mean.wrapper")
