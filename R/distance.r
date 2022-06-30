#### Distance Metrics

#' @export

distance <- function(x,y,method="euclidean",tvectors=tvectors){
  
  if(is.data.frame(tvectors)){
    tvectors <- as.matrix(tvectors)
  }else if(inherits(tvectors,"textmatrix")){
    tvectors <- matrix(tvectors,
                       nrow=nrow(tvectors),ncol=ncol(tvectors),
                       dimnames=list(rownames(tvectors),colnames(tvectors)))
  }
  
  if(is.matrix(tvectors)){
    
    if(!inherits(x,"character")){
      x <- as.character(x)
      message("Note: x converted to character")
    }
    
    if(!inherits(y,"character")){
      y <- as.character(y)
      message("Note: y converted to character")
    }

    
    
    if(x %in% rownames(tvectors) && y %in% rownames(tvectors)){
      
      v <- tvectors[x,]
      w <- tvectors[y,]
      
      if(method == "euclidean"){
        
        dist <- sqrt(sum((v - w)^2))
        
      }
      
      
      if(method == "cityblock"){
        
        dist <- sum( abs(v - w))
        
      }
      
      
    }else{
      
      warning("x and y must be words in rownames(tvectors)")
      return(NA)
      
    }
    
  }else{stop("tvectors must be a matrix!")}
  
  return(dist)
}