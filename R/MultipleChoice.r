
#' @export
MultipleChoice <- function(x,y,tvectors=tvectors,breakdown=TRUE){
  
  
  cos <- vector(length=length(y))
  names(cos) <- y
  
  for(i in 1:length(y)){cos[i] <- costring(x,y[i],
                                           tvectors=tvectors,
                                           breakdown=breakdown)}
  
  
  names(cos)[which(cos == max(cos))]
}
