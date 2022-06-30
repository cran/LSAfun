##### Generate Targets

#' @export
#' @importFrom lsa cosine 

choose.target <- function(x,lower,upper,n,tvectors=tvectors){
  
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
    

    allwords <- vector(length=nrow(tvectors))
    
    satz1 <- x
    satz1split <- strsplit(satz1,split=" ")[[1]]
    
    used1     <- satz1split[satz1split %in% rownames(tvectors)]
    if(length(used1)==0){(warning("no element of x found in rownames(tvectors)"))
                         return(NA)}
    
    if(length(used1) >1){satz1vec <- colSums(tvectors[used1,])}
    if(length(used1)==1){satz1vec <- tvectors[used1,]}
    
    for(i in 1:nrow(tvectors)){
      
      allwords[i] <- cosine(satz1vec,tvectors[i,])
    }     
    
    names(allwords) <- rownames(tvectors)
    
    allwords <- allwords[!is.na(allwords)]
    
    a <- sample(allwords[allwords >= lower & allwords <= upper])[1:n]
        
    return(a)
    
  }else{warning("tvectors must be a matrix!")}
  
}
