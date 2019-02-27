##### Sentence Comparisons #####################

#' @export
#' @importFrom lsa cosine 

multidocs <- function(x,y=x,chars=10,tvectors=tvectors,breakdown=FALSE){
  
  if(class(tvectors) == "data.frame"){
    tvectors <- as.matrix(tvectors)
  }else if(class(tvectors) == "textmatrix"){
    tvectors <- matrix(tvectors,
                       nrow=nrow(tvectors),ncol=ncol(tvectors),
                       dimnames=list(rownames(tvectors),colnames(tvectors)))
  }
  
  if(class(tvectors) == "matrix"){
    
    if(class(x) != "character"){
      x <- as.character(x)
      message("Note: x converted to character")
    }
    
    if(class(y) != "character"){
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
    
    
    doc1 <- NULL
    xdocs <- NULL
    
    for(i in 1:length(satz1)){
      satz1split <- strsplit(satz1[i],split=" ")[[1]]
      used1     <- satz1split[satz1split %in% rownames(tvectors)]
      if(length(used1)==0){(warning("no element of x found in rownames(tvectors)"))
        return(NA)}
      rest1    <- satz1split[!(satz1split %in% rownames(tvectors))]
      if(length(used1) >1){satz1vec <- colSums(tvectors[used1,])}
      if(length(used1)==1){satz1vec <- tvectors[used1,]}
      doc1 <- rbind(doc1,satz1vec)
      
      if(chars > 0){
        rownames(doc1)[i] <- paste("document x",i,": ",paste(unlist(strsplit(satz1[i],split=""))[1:chars],collapse=""),"[...]",sep="")
      }
      if(chars == 0){
        rownames(doc1)[i] <- paste("document x",i,sep="")
      }
      
      if(length(unlist(strsplit(satz1[i],split=""))) <= 1000){
        xdocs[i] <- paste("document x",i,": ",paste(unlist(strsplit(satz1[i],split=""))[1:(length(unlist(strsplit(satz1[i],split=""))))],collapse=""),sep="")
      }
      if(length(unlist(strsplit(satz1[i],split=""))) > 1000){
        xdocs[i] <- paste("document x",i,": ",paste(unlist(strsplit(satz1[i],split=""))[1:1000],collapse=""),"[...]",sep="")
      }
    }
    
    doc2 <- NULL
    ydocs <- NULL
    
    for(i in 1:length(satz2)){
      satz2split <- strsplit(satz2[i],split=" ")[[1]]
      used2     <- satz2split[satz2split %in% rownames(tvectors)]
      if(length(used2)==0){(warning("no element of x found in rownames(tvectors)"))
        return(NA)}
      rest2    <- satz2split[!(satz2split %in% rownames(tvectors))]
      if(length(used2) >1){satz2vec <- colSums(tvectors[used2,])}
      if(length(used2)==1){satz2vec <- tvectors[used2,]}
      doc2 <- rbind(doc2,satz2vec)
      
      if(chars > 0){
        rownames(doc2)[i] <- paste("document y",i,": ",paste(unlist(strsplit(satz2[i],split=""))[1:chars],collapse=""),"[...]",sep="")
      }
      if(chars == 0){
        rownames(doc2)[i] <- paste("document y",i,sep="")
      }
      
  
      
      if(length(unlist(strsplit(satz2[i],split=""))) <= 1000){
        ydocs[i] <- paste("document y",i,": ",paste(unlist(strsplit(satz2[i],split=""))[1:length(unlist(strsplit(satz2[i],split="")))],collapse=""),sep="")
      }
      if(length(unlist(strsplit(satz2[i],split=""))) > 1000){
        ydocs[i] <- paste("document y",i,": ",paste(unlist(strsplit(satz2[i],split=""))[1:1000],collapse=""),"[...]",sep="")
      }
    }
    
    
    cosmat <- matrix(nrow=nrow(doc1),ncol=nrow(doc2))
    rownames(cosmat) <- rownames(doc1)
    colnames(cosmat) <- rownames(doc2)
    
    for(i in 1:nrow(doc1)){
      
      line <- doc1[i,]
      
      for(j in 1:nrow(doc2)){
        
        cosmat[i,j] <- cosine(line,doc2[j,])
        
      }
    }
    
    
    
    
    return(list(cosmat=cosmat,xdocs=xdocs,ydocs=ydocs))
    
    
  }else{stop("tvectors must be a matrix!")}
  
}
