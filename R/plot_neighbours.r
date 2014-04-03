################################################
##### Plot Neighbours ##########################

#' @export plot_neighbours
plot_neighbours <- function(x,n,connect.lines=0,
                            start.lines=T,axes=F,box=F,
                            tvectors=tvectors,breakdown=TRUE,
                            ...){
  
  ### Compute neighbours
  
  if(class(tvectors) == "matrix"){
    
    if(class(x) == "character"){
      
      if(breakdown==TRUE){satz1 <- breakdown(x)}  
      
      satz1split <- strsplit(satz1,split=" ")[[1]]
      used1      <- satz1split[satz1split %in% rownames(tvectors)]
      
      if(length(used1) > 1){satz1vec <- colSums(tvectors[used1,])}
      if(length(used1) == 1){satz1vec <- (tvectors[used1,])}
      
      if(length(used1)==0){return(warning(
        "no element of x found in rownames(tvectors)"))
      }
      
      
      near      <- names(neighbours(satz1vec,n,
                                    tvectors=tvectors,
                                    breakdown=breakdown))
      
      
      
    }
    
    
    if(class(x) == "numeric"){
      
      satz1vec  <- x
      
      near      <- names(neighbours(x,n,tvectors=tvectors,
                                    breakdown=breakdown))
      
      
      
      
    }
    
    nearwords <- paste(near,collapse=" ")
    
    
    cos.near  <- multicos(nearwords,tvectors=tvectors,
                          breakdown=breakdown)
    
    
    #### Add Phrase to diagram
    
    
    if((class(x) == "character")){
      
      #       cos.near  <- rbind(near,cos.near)
      #       near.plus <- c(1,near)
      #       cos.near  <- cbind(near.plus,cos.near)
      #       
      #       strange stuff going on here
      
      if(length(satz1split) > 1){
        
        letters     <- unlist(strsplit(x,""))[1:15]
        letters     <- letters[!is.na(letters)]
        expressionW <- paste(letters,sep="",collapse="")
        if(expressionW != x){
          expressionW <- paste(expressionW,"[...]",sep="")}
        
        rownames(cos.near)[1] <- expressionW
        colnames(cos.near)[1] <- expressionW
      }
    }
    
    
    if(class(x) == "numeric"){
      
      #       cos.near  <- rbind(near,cos.near)
      #       near.plus <- c(1,near)
      #       cos.near  <- cbind(near.plus,cos.near)
      #      
      #       still strange stuff
      
      rownames(cos.near)[1] <- "Input Vector"
      colnames(cos.near)[1] <- "Input Vector"
    }
    
    
    ## Reduce to three dimensions
    
    pca1 <- princomp(covmat=cos.near)
    L    <- loadings(pca1) %*% diag(pca1$sdev)
    Lt   <- varimax(L[,1:3])$loadings
    
    Lt           <- as.data.frame(Lt[,])
    colnames(Lt) <- c("x","y","z")
    Lt$words     <- rownames(Lt)
    Lt$words     <- iconv(Lt$words, to="ASCII//TRANSLIT")
    
    
    ## Plot
    
    with(Lt,plot3d(x,y,z,box=box,axes=axes,xlab="",ylab="",zlab=""
                   ,xakt="n",yakt="n",zakt="n",...))
    with(Lt,text3d(x,y,z,words))
    
    
    ### lines from original word to vectors
    
    if(start.lines==T){
      
      steps                 <- vector(length=(2*n))
      steps[seq(1,2*n-1,2)] <- rep(1,n)
      steps[seq(2,2*n,2)]   <- 1:(n) 
      
      segments3d(Lt$x[steps],Lt$y[steps],Lt$z[steps])
    }
    
    ### connect.lines : X nearest
    
    if(connect.lines > 0){
      which.indices       <- t(apply(cos.near, 1, order, decreasing = T)[ 1:(connect.lines+1), ])
      which.indices       <- as.data.frame(which.indices[,-1])
      which.indices[,3:4] <- 1:n
      which.indices       <- which.indices[,c(3,1,4,2)]
      
      indices       <- as.vector(t(which.indices))
      
      segments3d(Lt$x[indices],Lt$y[indices],Lt$z[indices])
    }
    
  }else{warning("tvectors must be a matrix!")}
}
