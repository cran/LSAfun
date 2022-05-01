##### Multicostring

#' @export
#' @importFrom lsa cosine
multicostring <- function(x,y,tvectors=tvectors,split=" ",remove.punctuation=TRUE,breakdown=FALSE){
  
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
      
      satz1 <- breakdown(x)
      y     <- breakdown(y)
      
    }
    
    if(breakdown==FALSE){
      
      satz1 <- x
      y     <- y
      
    }
    
    if(remove.punctuation == TRUE){
      satz1 <- gsub(satz1,pattern="[[:punct:]]",replacement = "")
      y <- gsub(y,pattern="[[:punct:]]",replacement = "")
    }
    
    ### turn x into a single vector (added vectors of elements)
    
    if(length(satz1) == 1){
      satz1split <- strsplit(satz1,split=split)[[1]]
    }
    
    if(length(satz1)  > 1){satz1split <- satz1}
    
    used1     <- satz1split[satz1split %in% rownames(tvectors)]
    if(length(used1)==0){(warning("no element of x found in rownames(tvectors)"))
      return(NA)}else if(length(used1) < length(satz1split)){
        (cat("Note: not all elements in x were found in rownames(tvectors)\n\n")) 
      }
    
    rest1    <- satz1split[!(satz1split %in% rownames(tvectors))]
    
    if(length(used1) >1){satz1vec <- colSums(tvectors[used1,])}
    if(length(used1)==1){satz1vec <- tvectors[used1,]}
    
    ### split up y into single words
    
    
    if(length(y) == 1){
      ysplit <- strsplit(y,split=" ")[[1]]
    }
    
    if(length(y)  > 1){ysplit <- y}
    
    
    used2     <- ysplit[ysplit %in% rownames(tvectors)]
    if(length(used2)==0){(warning("no element of y found in rownames(tvectors)"))
                         return(NA)}else if(length(used2) < length(ysplit)){
      cat("Note: not all elements in y were found in rownames(tvectors)\n\n")}
    
    vecs2 <- matrix(tvectors[used2,],nrow=length(used2))
    
    cosmat <- matrix(nrow=1,ncol=nrow(vecs2))
    rownames(cosmat) <- "expression in x"
    colnames(cosmat) <- used2
    
    
    
    for(j in 1:nrow(vecs2)){
      
      cosmat[1,j] <- cosine(satz1vec,vecs2[j,])
      
    }
    
    
    return(cosmat)
    
  }else{stop("tvectors must be a matrix!")}
  
}
