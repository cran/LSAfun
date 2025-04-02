##### Centroid Analysis #####################

#' @export
#' @importFrom lsa cosine 



####


centroid_analysis <- function(responses,targets = NULL,split=" ",unique.responses = FALSE,reference.list = NULL,verbose = FALSE,rank.responses = FALSE,tvectors=tvectors){
  
  if(is.data.frame(tvectors)){
    tvectors <- as.matrix(tvectors)
  }else if(inherits(tvectors,"textmatrix")){
    tvectors <- matrix(tvectors,
                       nrow=nrow(tvectors),ncol=ncol(tvectors),
                       dimnames=list(rownames(tvectors),colnames(tvectors)))
  }
  
  if(is.matrix(tvectors)){
    
    if(!inherits(responses,"character")){
      responses <- as.character(responses)
      message("Note: responses converted to character")
    }
    
    
    satz1 <- responses
    
    
    if(length(satz1) == 1){
      satz1split <- strsplit(satz1,split=split)[[1]]
    }
    
    
    if(length(satz1)  > 1){satz1split <- satz1}
    
    
    used1     <- satz1split[satz1split %in% rownames(tvectors)]
    if(length(used1)==0){(warning("no element of responses found in rownames(tvectors)"))
      return(NA)}else if(length(used1) < length(satz1split)){
        (cat("Note: not all elements in responses were found in rownames(tvectors)\n\n")) 
      }
    
    
    # compute centroid of responses
    
    
    
    if(unique.responses == F){
      vecs <- tvectors[used1,]
    }else if(unique.responses == T){
      vecs <- tvectors[unique(used1),]
    }
    
    if(is.null(nrow(vecs)) & length(vecs) == ncol(tvectors)){
      vecs <- matrix(vecs,nrow=1,ncol=ncol(tvectors),byrow = T)
    }
    
    if(nrow(vecs) == 1){
      centroid    <- as.vector(vecs)
    }else if(nrow(vecs) > 1){
      centroid    <- colMeans(vecs)
    }
    
    out <- list(centroid=centroid)
    
    # If there are targets
    
    if(!is.null(targets)){
      
      if(!inherits(targets,"character")){
        targets <- as.character(targets)
        message("Note: targets converted to character")
      }
      
      satz2 <- targets
      
      if(length(satz2) == 1){
        satz2split <- strsplit(satz2,split=split)[[1]]
      }
      
      if(length(satz2)  > 1){satz2split <- satz2}
      
      used2     <- satz2split[satz2split %in% rownames(tvectors)]
      if(length(used2)==0){(warning("no element of targets found in rownames(tvectors)"))
        return(NA)}else if(length(used2) < length(satz2split)){
          (cat("Note: not all elements in targets were found in rownames(tvectors)\n\n")) 
        }
      
      # compute neighborhood rank with respect to targets
      
      if(is.null(reference.list)){
        reference.list <- rownames(tvectors)
      }
      if(!all(reference.list %in% rownames(tvectors))){
        warning("all elements of reference.list must be found in rownames(tvectors)")
      }
      
      ranks.target <- rep(NA,length(targets))
      names(ranks.target) <- targets
      
      ranks.centroid <- rep(NA,length(targets))
      names(ranks.centroid) <- targets
      
      cosines <- rep(NA,length(targets))
      names(cosines) <- targets
      
      neigh_centroid <- neighbors(centroid,n=length(reference.list),tvectors = tvectors[reference.list,])
      neigh_centroid <- neigh_centroid[!is.na(neigh_centroid)]
      
      
      if(rank.responses == F){
        ### exclude responses: these can't be the target word
        neigh_centroid <- neigh_centroid[!(names(neigh_centroid) %in% used1)]
      }
      
      for(target in targets){
        if(verbose==T){
          print(paste("target:",target))
        }
        ref <- unique(c(reference.list,target))
        neigh_target <- neighbors(tvectors[target,],n=length(ref),tvectors = tvectors[ref,])
        neigh_target <- neigh_target[!is.na(neigh_target)]
        
        cos <- as.numeric(cosine(centroid,tvectors[target,]))
        
        cosines[target]          <- cos
        ranks.target[target]     <- length(neigh_target[neigh_target > cos])
        ranks.centroid[target]   <- length(neigh_centroid[neigh_centroid > cos]) + 1
        ### +1 because, unlike for the target, the centroid itself is not part of the list
        
      }
      
      out <- list(centroid=centroid,cosines=cosines,ranks.target=ranks.target,ranks.centroid=ranks.centroid)
      
    }
    
    class(out) <- "centroid_analysis"
    return(out)
    
  }else{stop("tvectors must be a matrix!")}
  
}

