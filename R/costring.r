##### Sentence Comparisons #####################

#' @export
#' @importFrom lsa cosine 
 
costring <- function(x,y,tvectors=tvectors,split=" ",remove.punctuation=TRUE,breakdown=FALSE){
  
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
      satz2 <- breakdown(y)
      
    }
    
    if(breakdown==FALSE){
      
      satz1 <- x
      satz2 <- y
      
    }
    
    if(remove.punctuation == TRUE){
      satz1 <- gsub(satz1,pattern="[[:punct:]]",replacement = "")
      satz2 <- gsub(satz2,pattern="[[:punct:]]",replacement = "")
    }
    
    if(length(satz1) == 1){
      satz1split <- strsplit(satz1,split=split)[[1]]
    }
    if(length(satz2) == 1){
      satz2split <- strsplit(satz2,split=split)[[1]]
    }
    
    if(length(satz1)  > 1){satz1split <- satz1}
    if(length(satz2)  > 1){satz2split <- satz2}
    
    
    used1     <- satz1split[satz1split %in% rownames(tvectors)]
    if(length(used1)==0){(warning("no element of x found in rownames(tvectors)"))
                         return(NA)}else if(length(used1) < length(satz1split)){
                           (cat("Note: not all elements in x were found in rownames(tvectors)\n\n")) 
                         }
    
    used2     <- satz2split[satz2split %in% rownames(tvectors)]
    if(length(used2)==0){(warning("no element of y found in rownames(tvectors)"))
                         return(NA)}else if(length(used1) < length(satz1split)){
                           (cat("Note: not all elements in y were found in rownames(tvectors)\n\n")) 
                         }
    
    rest1    <- satz1split[!(satz1split %in% rownames(tvectors))]
    rest2    <- satz2split[!(satz2split %in% rownames(tvectors))]
    
    if(length(used1) >1){satz1vec <- colSums(tvectors[used1,])}
    if(length(used1)==1){satz1vec <- tvectors[used1,]}
    
    if(length(used2) >1){satz2vec <- colSums(tvectors[used2,])}
    if(length(used2)==1){satz2vec <- tvectors[used2,]}
    
    cos      <- as.numeric(cosine(satz1vec,satz2vec))
    
    #   out <- list(cos=cos,used1=used1,used2=used2,rest1=rest1,rest2=rest2)
    #   print(out)

    return(cos)
    
  }else{stop("tvectors must be a matrix!")}
  
}
