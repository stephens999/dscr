score = function(data, output){
  return(list(squared_error = (data$meta$truemean-output$meanest)^2, abs_error = abs(data$meta$truemean-output$meanest)))
}
