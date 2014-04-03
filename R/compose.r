##### Two-Word Composition #####################

#' @export
compose <- function(x,y,method="Add",a=1,b=1,c=1,m,k,tvectors=tvectors,breakdown=TRUE){
  
  if(class(tvectors) == "matrix"){
    
    if(breakdown==TRUE){
      
      x <- breakdown(x)
      y <- breakdown(y)
      
    }
    
    if(x %in% rownames(tvectors) && y %in% rownames(tvectors)){
      
      if(method=="Add")
      {comp <- tvectors[x,]+tvectors[y,]}
      
      if(method=="WeightAdd")
      {comp <- (a*tvectors[x,])+(b*tvectors[y,])}
      
      if(method=="Multiply")
      {comp <- tvectors[x,]*tvectors[y,]}
      
      if(method=="Combined")
      {comp <- (a*tvectors[x,])+(b*tvectors[y,])+c*(tvectors[x,]*tvectors[y,])}
      
      if(method=="Predication")
      {comp <- Predication(P=x,A=y,m,k,tvectors=tvectors)$PA}
      
      
    }
    
    if(!x %in% rownames(tvectors)){
      warning("x not in rownames(tvectors")
    }
    
    if(!y %in% rownames(tvectors)){
      warning("y not in rownames(tvectors)")
    }
    
    comp
    
  }else{
    warning("tvectors must be a matrix!")
  } 
  
}
