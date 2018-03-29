#### Distance Metrics

#' @export

distance <- function(x,y,method="euclidean",tvectors=tvectors,breakdown=FALSE){
  
  if(class(tvectors) == "data.frame"){
    tvectors <- as.matrix(tvectors)
  }else if(class(tvectors) == "textmatrix"){
    tvectors <- matrix(tvectors,
                       nrow=nrow(tvectors),ncol=ncol(tvectors),
                       dimnames=list(rownames(tvectors),colnames(tvectors)))
  }
  
  if(class(tvectors) == "matrix"){
    
    if(class(x) != "character"){
      x <- as.character(x)
      message("Note: x converted to character")
    }
    
    if(class(y) != "character"){
      y <- as.character(y)
      message("Note: y converted to character")
    }
    
    if(breakdown==TRUE){
      
      x <- breakdown(x)
      
      y <- breakdown(y)
      
    }
    
    
    if(x %in% rownames(tvectors) && y %in% rownames(tvectors)){
      
      v <- tvectors[x,]
      w <- tvectors[y,]
      
      if(method == "euclidean"){
        
        dist <- sqrt( sum( (v - w)^2 ) )
        
      }
      
      
      if(method == "cityblock"){
        
        dist <- sum( abs(v - w))
        
      }
      
      
    }else{
      
      warning("x and y must be words in rownames(tvectors)")
      return(NA)
      
    }
    
  }else{stop("tvectors must be a matrix!")}
  
}