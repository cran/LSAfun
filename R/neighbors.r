##### Neighbors

### Function

#' @export
#' @importFrom lsa cosine
neighbors <- function(x,n,tvectors=tvectors){
  
  if(is.data.frame(tvectors)){
    tvectors <- as.matrix(tvectors)
  }else if(inherits(tvectors,"textmatrix")){
    tvectors <- matrix(tvectors,
                       nrow=nrow(tvectors),ncol=ncol(tvectors),
                       dimnames=list(rownames(tvectors),colnames(tvectors)))
  }
  
  if(is.matrix(tvectors)){
    
    if(inherits(x,"factor")){
      x <- as.character(x)
      message("Note: x converted to character")
    }
    
    
    if(inherits(x,"character")){
      
    satz1 <- x
      
      satz1split <- strsplit(satz1,split=" ")[[1]]
      
      if(any(satz1split %in% rownames(tvectors))){
        
        used1      <- satz1split[satz1split %in% rownames(tvectors)]
        
        if(length(used1) > 1){satz1vec <- colSums(tvectors[used1,])}
        if(length(used1) == 1){satz1vec <- (tvectors[used1,])}
        
      }else{
        
        warning("x must be a word in rownames(tvectors)")
        return(NA)
      }
    }
    
    if(inherits(x,"numeric")){
      
      satz1vec <- x
    }
    
    if(exists("satz1vec")){
      
      allcosines <- vector(length=nrow(tvectors))
      
      for(i in 1:nrow(tvectors)){
        
        allcosines[i] <- cosine(satz1vec,tvectors[i,])
      }     
      
      names(allcosines) <- rownames(tvectors)
      
      words <- sort(allcosines,decreasing=T)[1:n]  # m nearest types to P
      
      return(words)
    }
    
  }else{
    stop("tvectors must be a matrix!")
  } 
}
