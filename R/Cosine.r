#### Easier Two-Word Cosine 

#' @export
#' @importFrom lsa cosine 

Cosine <- function(x,y,tvectors=tvectors,breakdown=FALSE){
  
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
    
    if(breakdown==TRUE){
      
      x <- breakdown(x)
      
      y <- breakdown(y)
      
    }
    
    
    if(x %in% rownames(tvectors) && y %in% rownames(tvectors)){
      
      out <- as.numeric(cosine(tvectors[x,],tvectors[y,]))
      return(out)
      
    }else{
      
      warning("x and y must be words in rownames(tvectors)")
      return(NA)
      
    }
    
  }else{stop("tvectors must be a matrix!")}
  
}