##### Predication ##############################


# Print Method

print.Pred <- function(obj){
  cat("\nPredication Results\n\n")
  cat("Input: Predicate    ", obj$P,"    Argument:    ", obj$A, "\n\n")
  cat("Predication Vector ($PA):\n ",(obj$PA), "\n\n")
  cat("Predicate Vector without Argument ($P.Pred):\n ",(obj$P.Pred), "\n\n")
  cat("Used neighbourhood words ($neighbours):\n", obj$neighbours, "\n")
  invisible(obj)
}

# Function

#' @export
Predication <- function(P,A,m,k,tvectors=tvectors,breakdown=TRUE){
  
  if(class(tvectors) == "matrix"){
    
    if(breakdown==TRUE){
      
      P <- breakdown(P)
      A <- breakdown(A)
      
    }
    
    tvectors_P   <- tvectors[P,]
    tvectors_A   <- tvectors[A,]
    nrow         <- nrow(tvectors)
    
    
    # m nearest types to P
    
   
    near.P <- neighbours(P,(m+1),tvectors=tvectors,
                         breakdown=breakdown)[2:(m+1)]   

    
    
    # k nearest to A
    
    near.PA <- multicos(A,names(near.P),tvectors=tvectors,
                        breakdown=breakdown)[1,]
    near.PA <- sort(near.PA,decreasing=T)[1:k]
    
    
    neighbours <- names(near.PA)
    tvectors_PA <- tvectors[names(near.PA),]
    
    if(k==1){
      PA <- tvectors[P,]+tvectors[A,]+tvectors[names(near.PA),]
      P.Pred <- tvectors[P,]+tvectors[names(near.PA),] 
    } 
    if(k >1){
      PA <- tvectors[P,]+tvectors[A,]+colSums(tvectors[names(near.PA),])
      P.Pred <- tvectors[P,]+colSums(tvectors[names(near.PA),])
    }
    
    
    
    out <- list(PA=PA,P.Pred=P.Pred,neighbours=neighbours,P=P,A=A)
    
    class(out) <- "Pred"
    out
    
  }else{
    warning("tvectors must be a matrix!")
  } 
  
}
