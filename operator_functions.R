cell_mean <- function(y, 
                      multiplicator=1,
                      should_multiply=TRUE,
                      power1=NULL, 
                      power2=NULL){
  
  if( should_multiply == TRUE ){
    y <- y * multiplicator
  }
  
  if( is.null(power1) == FALSE){
    y <- y ** as.numeric(power1)
  }
  
  if( is.null(power2) == FALSE){
    y <- y ** as.numeric(power2)
  }
  
  return(mean(y))
}