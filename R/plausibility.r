##### Neighbors

### Function

#' @export
#' @importFrom lsa cosine

plausibility <- function(x,method,n=10,stem,tvectors=tvectors){
  
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
      
      if(method == "n_density"){
        
        allcosines <- vector(length=nrow(tvectors))
        
        for(i in 1:nrow(tvectors)){
          
          allcosines[i] <- cosine(satz1vec,tvectors[i,])
        }     
        
        names(allcosines) <- rownames(tvectors)
        
        words <- sort(allcosines,decreasing=T)[2:(n+1)]  # m nearest types to P
        
        n_density <- mean(words)
        return(n_density)
        
      }
      
      
      
      if(method == "length"){
        
        length <- sqrt(sum(satz1vec^2))
        return(length)
      }
      
      
      
      if(method == "proximity"){
        
        if(exists("stem")){
          
        vstem <- tvectors[stem,]
        
        proximity <- as.numeric(cosine(satz1vec,tvectors[stem,]))
        return(proximity)
        
        }
        
        if(!exists("stem")){
          
          stop("Define a stem to compute the proximity to it!")
          
        }        
        
      }
      
      
      if(method == "entropy"){
        
        if(any(satz1vec < 0)){
          warning("negative values in x detected and omitted. Results will be flawed!")
        }
          
          K <- length(satz1vec)
        
          ## exclude zeros 
          
          satz1vec_pos <- satz1vec[satz1vec > 0]
          
          entropy <- log(K) - (sum(satz1vec_pos*log(satz1vec_pos))) / K
          return(entropy)        
        
      }
      
      
      
    }
    
  }else{
    stop("tvectors must be a matrix!")
  } 
}
