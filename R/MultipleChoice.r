
#' @export
MultipleChoice <- function(x,y,tvectors=tvectors,remove.punctuation=TRUE){

 
  cos <- vector(length=length(y))
  names(cos) <- y
  
  for(j in 1:length(y)){value <- costring(x,y[j],
                                           tvectors=tvectors,remove.punctuation=remove.punctuation)
  
  if(is.na(value)){cos[j] <- -2}
  if(!is.na(value)){cos[j] <- value}                      
                        
  }
  
  
  mc <- names(cos)[which(cos == max(cos))]
  return(mc)
}
