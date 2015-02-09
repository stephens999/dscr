score = function(data, output){
  X = c((data$meta$truemean-output$meanest)^2,
          abs(data$meta$truemean-output$meanest), 
          output$test)
           
 # class(X) <- 'data.frame'; 
  names(X) <- c('squared_error', 'abs_error', paste0('C', 3:length(X))); 
  #row.names(X)<-1
  return(X)
#  return(c(squared_error = (data$meta$truemean-output$meanest)^2, 
#              abs_error = abs(data$meta$truemean-output$meanest), 
#              setNames(as.list(output$test),paste0('C',1:length(output$test)))))
}
