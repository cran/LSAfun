##### Two-Word Composition #####################

#' @export
compose <- function(x,y,method="Add",a=1,b=1,c=1,m,k,
                    tvectors=tvectors,breakdown=TRUE,
                    norm="none"){
  
  if(class(tvectors) == "matrix"){
    
    if(breakdown==TRUE){
      
      x <- breakdown(x)
      y <- breakdown(y)
      
    }
    
    if(x %in% rownames(tvectors) && y %in% rownames(tvectors)){
      
      if(method=="Add"){
        
        if(norm=="none"){ 
          comp <- tvectors[x,]+tvectors[y,]}
        
        if(norm=="all"){
          comp <- normalize(tvectors[x,])+normalize(tvectors[y,])} 
        
        if(norm=="block"){warning("Blocked normalization not defined for this composition method")}
      }
      
      
      
      if(method=="WeightAdd"){
        
        if(norm=="none"){ 
          comp <- a*tvectors[x,]+b*tvectors[y,]}
        
        if(norm=="all"){
          comp <- a*normalize(tvectors[x,])+b*normalize(tvectors[y,])}  
        
        if(norm=="block"){warning("Blocked normalization not defined for this composition method")}
      }
      
      
      
      if(method=="Multiply"){
        
        if(norm=="none"){
          comp <- tvectors[x,]*tvectors[y,]}
           
           if(norm=="all"){
             comp <- normalize(tvectors[x,])*normalize(tvectors[y,])}
              
              message("Normalization does not change the orientation of result vector for this method")
              
              if(norm=="block"){warning("Blocked normalization not defined for this composition method")}
      }
      
      
      
      if(method=="Combined"){
        
        if(norm=="none"){
          comp <- (a*tvectors[x,])+(b*tvectors[y,])+c*(tvectors[x,]*tvectors[y,])}
           
           if(norm=="all"){
             comp <- (a*normalize(tvectors[x,]))+(b*normalize(tvectors[y,]))+c*(normalize(tvectors[x,]*tvectors[y,]))}              
              
              if(norm=="block"){warning("Blocked normalization not defined for this composition method")}  
      }
      
      
      
      if(method=="Predication")
      {comp <- Predication(P=x,A=y,m,k,tvectors=tvectors,norm=norm)$PA}
      
      
      

    }
    
  
  
  if(!x %in% rownames(tvectors)){
    warning("x not in rownames(tvectors")
    return(NA)
  }
  
  if(!y %in% rownames(tvectors)){
    warning("y not in rownames(tvectors)")
    return(NA)
  }
  
  comp
  
}else{
  stop("tvectors must be a matrix!")
} 

}
