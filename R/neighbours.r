##### Neighbours

### Function

#' @export
neighbours <- function(x,n,tvectors=tvectors,breakdown=TRUE){
  
  if(class(tvectors) == "matrix"){
    
    if(class(x) == "character"){
      
      if(breakdown==TRUE){satz1 <- breakdown(x)}  
      
      satz1split <- strsplit(satz1,split=" ")[[1]]
      
      if(any(satz1split %in% rownames(tvectors))){
        
        used1      <- satz1split[satz1split %in% rownames(tvectors)]
        
        if(length(used1) > 1){satz1vec <- colSums(tvectors[used1,])}
        if(length(used1) == 1){satz1vec <- (tvectors[used1,])}
        
      }else{
        
        warning("x must be a word in rownames(tvectors)")
      }
    }
    
    if(class(x) == "numeric"){
      
      satz1vec <- x
    }
    
    if(exists("satz1vec")){
      
      allcosines <- vector(length=nrow(tvectors))
      
      for(i in 1:nrow(tvectors)){
        
        allcosines[i] <- cosine(satz1vec,tvectors[i,])
      }     
      
      names(allcosines) <- rownames(tvectors)
      
      words <- sort(allcosines,decreasing=T)[1:n]  # m nearest types to P
      
      words
    }
    
  }else{
    warning("tvectors must be a matrix!")
  } 
}
