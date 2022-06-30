##### Vector x Vector Comparison

#' @export
#' @importFrom lsa cosine
multicos <- function(x,y=x,tvectors=tvectors){
  
  if(is.data.frame(tvectors)){
    tvectors <- as.matrix(tvectors)
  }else if(inherits(tvectors,"textmatrix")){
    tvectors <- matrix(tvectors,
                       nrow=nrow(tvectors),ncol=ncol(tvectors),
                       dimnames=list(rownames(tvectors),colnames(tvectors)))
  }
  
  if(is.matrix(tvectors)){
    
    ## handle x
    
    if(inherits(x,"factor")){
      x <- as.character(x)
      message("Note: x converted to character")
    }
    
    if(inherits(x,"character")){
      
      if(length(x) == 1){
        xsplit <- strsplit(x,split=" ")[[1]]
      }
      
      if(length(x)  > 1){xsplit <- x}
      
      used1     <- xsplit[xsplit %in% rownames(tvectors)]
      
      if(length(used1)==0){warning("no element of x found in rownames(tvectors)")
                           return(NA)}
      if(length(used1) < length(xsplit)){
        cat("Note: not all elements in x were found in rownames(tvectors)\n\n")}
      
      vecs1 <- matrix(tvectors[used1,],nrow=length(used1))
      
    }
    
    if(inherits(x,"numeric")){
      used1 <- "Expression in x"
      vecs1 <- t(as.matrix(x))
    }
    
    ## handle y
    
    
    if(inherits(y,"factor")){
      y <- as.character(y)
      message("Note: y converted to character")
    }
    
    if(length(y) == 1){
      ysplit <- strsplit(y,split=" ")[[1]]
    }
    
    if(length(y)  > 1){ysplit <- y}
    
    used2 <- ysplit[ysplit %in% rownames(tvectors)]
    
    if(length(used2)==0){warning("no element of y found in rownames(tvectors)")
                         return(NA)}
    if(length(used2) < length(ysplit)){
      cat("Note: not all elements in y were found in rownames(tvectors)\n\n")}
    
    vecs2 <- matrix(tvectors[used2,],nrow=length(used2))
    
    ## compute cosine matrix
    
    cosmat <- matrix(nrow=nrow(vecs1),ncol=nrow(vecs2))
    rownames(cosmat) <- used1
    colnames(cosmat) <- used2
    
    for(i in 1:nrow(vecs1)){
      
      line <- vecs1[i,]
      
      for(j in 1:nrow(vecs2)){
        
        cosmat[i,j] <- cosine(line,vecs2[j,])
        
      }
    }
    
    return(cosmat)
    
  }else{stop("tvectors must be a matrix!")}
  
}
