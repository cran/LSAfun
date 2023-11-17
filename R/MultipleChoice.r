
#' @export
MultipleChoice <- function(x,y,tvectors=tvectors,remove.punctuation=TRUE, stopwords = NULL, method ="Add", all.results=FALSE){
  
  
  cos <- vector(length=length(y))
  names(cos) <- y
  
  if(method == "Analogy"){
    satz1 <- x
    
    if(remove.punctuation == TRUE){
      satz1 <- gsub(satz1,pattern="[[:punct:]]",replacement = "")
    }
    
    satz1split <- strsplit(satz1,split=" ")[[1]]
    if(all(satz1split %in% rownames(tvectors))){
      satz1vec <- normalize(tvectors[satz1split[2],]) - normalize(tvectors[satz1split[1],])
    }else{
      stop("not all elements in x were found in rownames(tvectors)")
    }
    
    for(j in 1:length(y)){
      satz2 <- y[j]
      
      if(remove.punctuation == TRUE){
        satz2 <- gsub(satz2,pattern="[[:punct:]]",replacement = "")
      }
      
      satz2split <- strsplit(satz2,split=" ")[[1]]
      if(all(satz2split %in% rownames(tvectors))){
        satz2vec <- normalize(tvectors[satz2split[2],]) - normalize(tvectors[satz2split[1],])
        cos[j] <- as.numeric(cosine(satz1vec,satz2vec))
      }else{
        cos[j] <- NA
        warning("not all elements in x were found in rownames(tvectors)")
      }
      
      }
    
  }else{
    for(j in 1:length(y)){
      cos[j] <- costring(x,y[j],tvectors=tvectors,remove.punctuation=remove.punctuation,stopwords=stopwords,method=method)
    }
    # if(is.na(value)){cos[j] <- -2}
    # if(!is.na(value)){cos[j] <- value}                      
    
  }
  
  
  if(all.results == FALSE){
    cos <- cos[!is.na(cos)]
    mc <- names(cos)[which(cos == max(cos))]
  }else if(all.results == TRUE){
    mc <- sort(cos,decreasing = T)
  }
  return(mc)
}
