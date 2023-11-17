##### Semantic Neighborhood Density

### Function

#' @export
#' @importFrom lsa cosine
#' @importFrom stats sd
SND <- function(x,n=NA,threshold=3.5,tvectors=tvectors){
  
  
  all <- neighbors(x=x,tvectors=tvectors,n=nrow(tvectors))
  all <- all[!is.na(all)]
  all <- all[names(all) != x]
  
  if(is.na(n)){
    
    scall <- (all-mean(all))/sd(all)
    
    if(inherits(threshold,"numeric")){
      n_set <- names(scall[scall > threshold])
      neighbors <- all[n_set]
    }else{
      stop("threshold must be a numeric")
    }
    
    if(length(neighbors > 0)){
      ndensity <- mean(neighbors)
    }else{
      ndensity <- NA
    }
    
  }else if(inherits(n,"numeric")){
    
    neighbors <- all[1:n]
    ndensity <- mean(neighbors)
    
  }else{
    stop("n must be either NA or a numeric")
  }
  
  return(list(neighbors=neighbors,n_size=length(neighbors),SND=ndensity))
  
}
