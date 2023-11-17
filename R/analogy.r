##### Analogies

### Function

#' @export
analogy <- function(x1,x2,y1=NA,n,tvectors=tvectors){
  
  if(is.data.frame(tvectors)){
    tvectors <- as.matrix(tvectors)
  }else if(inherits(tvectors,"textmatrix")){
    tvectors <- matrix(tvectors,
                       nrow=nrow(tvectors),ncol=ncol(tvectors),
                       dimnames=list(rownames(tvectors),colnames(tvectors)))
  }
  
  if(is.matrix(tvectors)){
    
    
    if(!inherits(x1,"character")){
      x1 <- as.character(x1)
      message("Note: x1 converted to character")
    }
    
    if(!inherits(x2,"character")){
      x2 <- as.character(x2)
      message("Note: x2 converted to character")
    }
    
    
    if(!x1 %in% rownames(tvectors)){
      warning("x1 not in rownames(tvectors")
      return(NA)
    }
    
    if(!x2 %in% rownames(tvectors)){
      warning("x2 not in rownames(tvectors)")
      return(NA)
    }
    
    
    if(!is.na(y1)){
      if(!inherits(y1,"character")){
        y1 <- as.character(y1)
        message("Note: y1 converted to character")
      }
      
      if(!y1 %in% rownames(tvectors)){
        warning("y1 not in rownames(tvectors)")
        return(NA)
      }
      
      if(all(c(x1,x2,y1) %in% rownames(tvectors))){
        
        analog  <- - normalize(tvectors[x1,]) + normalize(tvectors[x2,])  + normalize(tvectors[y1,])
        results <- neighbors(analog,n=n,tvectors=tvectors) 
        
      }
      
      
      return(list(y2_vec=analog,y2_neighbors=results))
      
      
    }else if(is.na(y1)){
      
      if(all(c(x1,x2) %in% rownames(tvectors))){
        
        analog  <- - normalize(tvectors[x1,]) + normalize(tvectors[x2,])
        results <- neighbors(analog,n=n,tvectors=tvectors) 
        
      }
      
      
      return(list(x_diff_vec=analog,x_diff_neighbors=results))
      
      
    }
    
  }else{
    stop("tvectors must be a matrix!")
  } 
}
