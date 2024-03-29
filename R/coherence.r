##### Coherence of a paragraph

#' @export
coherence <- function(x,split=c(".","!","?"),tvectors=tvectors, remove.punctuation=TRUE, stopwords = NULL, method ="Add"){
  
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
    

    sentences <- x
    
    for(i in 1:length(split)){
      sentences <- unlist(strsplit(sentences,split=split[i],fixed=TRUE))
    }
    
    local <- vector(length=length(sentences)-1)
    
    suppressWarnings({
      
      for(i in 1:(length(sentences)-1)){
        local[i] <- costring(sentences[i],sentences[i+1],
                             tvectors=tvectors,stopwords=stopwords,remove.punctuation=remove.punctuation,method=method)
      }
      
    })
    
    if(NA %in% local){warning("no element of s found in rownames(tvectors) for some sentences s in x")}
    
    global <- mean(local,na.rm=TRUE)
    
    out <- list(local=local,global=global)
    return(out)
    
  }else{stop("tvectors must be a matrix!")}
}