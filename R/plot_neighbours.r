################################################
##### Plot Neighbours ##########################

#' @export plot_neighbours
#' @importFrom rgl plot3d
#' @importFrom rgl text3d
#' @importFrom rgl segments3d
#' 
plot_neighbours <- function(x,n,connect.lines=0,
                            start.lines=T,axes=F,box=F,
                            alpha=.5, col="black", 
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
                          breakdown=FALSE)
    
    
    #### Add Phrase to diagram
    
    
    if((class(x) == "character")){
      
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
    Lt$words2     <- iconv(Lt$words, to="ASCII//TRANSLIT")
    
    
    ## Plot
    
    
    with(Lt,plot3d(x,y,z,box=box,axes=axes,xlab="",ylab="",zlab=""
                   ,xakt="n",yakt="n",zakt="n",col="black",...))
    
    
    
    with(Lt,text3d(x,y,z,words2))
    
    
    
    ### Rainbow palette
    
    palette <- rainbow(101,start=0,end=0.95)
    
    
    
    ### connect.lines : X nearest
    ### connect.lines use argument [2] from alpha and col!
    
    if(connect.lines == "all"){
      
      
      #### Create all pairwise combinations as vector
      
      comb  <- combn(rownames(Lt),m=2)
      combs <- c(combn(rownames(Lt),m=2))
      
      #### Pairwise combination similarities
      
      pwsim  <- cos.near[lower.tri(cos.near)]
      pwsim2 <- rep(pwsim,each=2)
      
      #### Prepare dataframe for segments: Connected pairs ordered in rows
      
      segm <- Lt[combs,]
      
      #### Add Pairwise similarities
      
      segm$pwsim <- pwsim2
      
      #### Add colours
      
      colour      <- palette[round(pos(pwsim2*100)+1)]
      segm$colour <- colour
      
      #### Plot lines
      
      if(col != "rainbow"){suppressWarnings(segments3d(segm,alpha=(segm$pwsim)^2))}
      
      if(col == "rainbow"){suppressWarnings(segments3d(segm,alpha=(segm$pwsim)^2,col=segm$colour))}
      
    }
    
    
    
    
    if(length(alpha)==1){alpha <- rep(alpha,2)}
    if(length(col)==1 && col != "rainbow"){col <- rep(col,2)}
    
    
    
    
    
    if(class(connect.lines) == "numeric" && connect.lines > 0){
      
      ### Find nearest to each word
      
      which.indices       <- t(apply(cos.near, 1, order, decreasing = T)[ 1:(connect.lines+1), ])
      which.indices       <- as.data.frame(which.indices[,-1])
      which.indices[,3:4] <- 1:n
      which.indices       <- which.indices[,c(3,1,4,2)]
      
      indices       <- as.vector(t(which.indices))
      
      
      ### Prepare Segments
      
      segm <- Lt[indices,]
      
      ### Add pairwise similarities
      
      pwsim <- vector(length=nrow(segm)/2)
      
      for(i in seq(1,nrow(segm),2)){
        
        pwsim[ceiling(i/2)] <- cos.near[segm[i,"words"],segm[(i+1),"words"]]
        
      }
      
      pwsim2 <- rep(pwsim,each=2)
      
      segm$pwsim <- pwsim2
      
      
      #### Add colours
      
      colour      <- palette[round(pos(pwsim2*100)+1)]
      segm$colour <- colour
      
      ### Plot
      
      if(col[1] != "rainbow" && alpha[1] != "shade"){
        suppressWarnings(segments3d(segm,alpha=alpha[2],col=col[2]))
      }
      
      if(col[1] != "rainbow" && alpha[1] == "shade"){
        suppressWarnings(segments3d(segm,alpha=(segm$pwsim)^2,col=col[2]))
      }
      
      if(col[1] == "rainbow"){
        
        suppressWarnings(segments3d(segm,alpha=(segm$pwsim)^2,col=segm$colour))
        
      }
      
    }    
    
    
    
    
    ### lines from original word to vectors
    ### start.lines use argument [1] from alpha and col!
    
    
    if(start.lines==T && connect.lines != "all"){
      
      steps                 <- vector(length=(2*n))
      steps[seq(1,2*n-1,2)] <- rep(1,n)
      steps[seq(2,2*n,2)]   <- 1:(n) 
      
      #### Prepare Segments
      
      segm0 <- Lt[steps,]
      
      pwsim0      <- rep(cos.near[1,],each=2)
      segm0$pwsim <- pwsim0
      
      colour0      <- palette[round(pos(pwsim0*100)+1)]
      segm0$colour  <- colour0
      
      
      
      if(col[1] != "rainbow" && alpha[1] != "shade"){
        suppressWarnings(segments3d(segm0,alpha=alpha[1],col=col[1]))
      }
      
      if(col[1] != "rainbow" && alpha[1] == "shade"){
        suppressWarnings(segments3d(segm0,alpha=(segm0$pwsim)^2,col=col[1]))
      }  
      
      if(col[1] == "rainbow"){
        
        suppressWarnings(segments3d(segm0,alpha=(segm0$pwsim)^2,col=segm0$colour))
        
      }  
      
      
    }
    
    
    
  }else{warning("tvectors must be a matrix!")}
}