################################################
##### Plot neighbors ##########################

#' @export plot_neighbors
#' @importFrom rgl plot3d
#' @importFrom rgl text3d
#' @importFrom rgl segments3d
#' @importFrom rgl par3d
#' @importFrom rgl bgplot3d
#' @importFrom grDevices rainbow
#' @importFrom grDevices heat.colors
#' @importFrom grDevices terrain.colors
#' @importFrom grDevices topo.colors
#' @importFrom grDevices cm.colors
#' @importFrom grDevices as.raster
#' @importFrom graphics rasterImage
#' @importFrom grDevices colorRampPalette
#' @importFrom graphics plot
#' @importFrom graphics par
#' @importFrom graphics text
#' @importFrom stats cmdscale
#' @importFrom stats loadings
#' @importFrom stats princomp
#' @importFrom stats varimax
#' @importFrom utils combn
#' 
plot_neighbors <- function(x,n,connect.lines="all",
                           start.lines=T,method="PCA",dims=3,
                           axes=F,box=F,cex=1,legend=T, size = c(800,800),
                           alpha="graded", alpha.grade = 1, col="rainbow", 
                           tvectors=tvectors,breakdown=FALSE,
                           ...){
  
  ### Compute neighbors
  
  if(!(dims %in% 2:3)){stop("Please set dim to 2 or 3")}
  
  if(is.data.frame(tvectors)){
    tvectors <- as.matrix(tvectors)
  }else if(inherits(tvectors,"textmatrix")){
    tvectors <- matrix(tvectors,
                       nrow=nrow(tvectors),ncol=ncol(tvectors),
                       dimnames=list(rownames(tvectors),colnames(tvectors)))
  }
  
  if(is.matrix(tvectors)){
    
    if(inherits(x,"factor")){
      x <- as.character(x)
      message("Note: x converted to character")
    }
    
    
    if(inherits(x,"character")){
      
      if(breakdown==TRUE){satz1 <- breakdown(x)}  
      if(breakdown==FALSE){satz1 <- x}  
      
      satz1split <- strsplit(satz1,split=" ")[[1]]
      used1      <- satz1split[satz1split %in% rownames(tvectors)]
      
      if(length(used1) > 1){satz1vec <- colSums(tvectors[used1,])}
      if(length(used1) == 1){satz1vec <- (tvectors[used1,])}
      
      if(length(used1)==0){return(warning(
        "no element of x found in rownames(tvectors)"))
      }
      
      
      near      <- names(neighbors(satz1vec,n,
                                   tvectors=tvectors,
                                   breakdown=breakdown))
      
      
      
    }
    
    
    if(inherits(x,"numeric")){
      
      satz1vec  <- x
      
      near      <- names(neighbors(x,n,tvectors=tvectors,
                                   breakdown=breakdown))
      
      
      
      
    }
    
    nearwords <- paste(near,collapse=" ")
    
    
    cos.near  <- multicos(nearwords,tvectors=tvectors,
                          breakdown=FALSE)
    
    
    #### Add Phrase to diagram
    
    
    if((inherits(x,"character"))){
      
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
    
    
    if(inherits(x,"numeric")){
      
      rownames(cos.near)[1] <- "Input Vector"
      colnames(cos.near)[1] <- "Input Vector"
    }
    
    
    ## Reduce dimensions
    
    if(method=="PCA"){
      pca1 <- princomp(covmat=cos.near)
      L    <- loadings(pca1) %*% diag(pca1$sdev)
      Lt   <- varimax(L[,1:dims])$loadings
    }
    
    if(method=="MDS"){
      dissim <- 1 - cos.near  
      
      mds1 <- cmdscale(dissim,eig=TRUE, k=dims) 
      Lt   <- mds1$points   
    }
    
    Lt           <- as.data.frame(Lt[,])
    
    if(dims==2){colnames(Lt) <- c("x","y")}
    if(dims==3){colnames(Lt) <- c("x","y","z")}
    
    Lt$words     <- rownames(Lt)
    Lt$words2     <- iconv(Lt$words, to="ASCII//TRANSLIT")
    
    
    ## Plot 2d
    
    if(dims==2){
      plot(Lt$x,Lt$y,xlab="Dimension 1",ylab="Dimension 2",pch=20,type="n",
           xlim=c(min(Lt$x)-0.1,max(Lt$x)+0.1),ylim=c(min(Lt$y)-0.1,max(Lt$y)+0.1))
      with(Lt,points(x,y,cex=.6,pch=20))
      with(Lt,text(x,y,words2,cex=cex))
    }
    
    
    ## Plot 3d
    
    
    if(dims == 3){
      par3d(windowRect = c(20, 30, size[1] + 20, size[2] + 30))
      
      with(Lt,plot3d(x,y,z,box=box,axes=axes,xlab="",ylab="",zlab=""
                     ,xakt="n",yakt="n",zakt="n",col="black",...))
      
      
      
      with(Lt,text3d(x,y,z,words2))
      
      
      
      
      
      
      if(length(alpha)==1){alpha <- rep(alpha,2)}
      if(length(col)==1){col <- rep(col,2)}
      
      
      
      ### alpha="shade" backwards compatibility
      if(alpha[1]=="shade"){alpha <- rep("graded",2)}
      
      
      
      ### Color palette
      
      palette1 <- rainbow(101,start=0,end=0.95)
      palette2 <- rainbow(101,start=0,end=0.95)
      
      if(col[1] == "heat.colors"){palette1 <- heat.colors(101)}
      if(col[1] == "terrain.colors"){palette1 <- terrain.colors(101)}
      if(col[1] == "topo.colors"){palette1 <- topo.colors(101)}
      if(col[1] == "cm.colors"){palette1 <- topo.colors(101)}
      
      if(col[2] == "heat.colors"){palette2 <- heat.colors(101)}
      if(col[2] == "terrain.colors"){palette2 <- terrain.colors(101)}
      if(col[2] == "topo.colors"){palette2 <- topo.colors(101)}
      if(col[2] == "cm.colors"){palette2 <- topo.colors(101)}
      
      if(length(col) >= 3){
        palette1 <- colorRampPalette(col)(101)
        palette2 <- colorRampPalette(col)(101)
      }
      
      
      
      
      
      ### connect.lines : X nearest
      ### connect.lines use argument [2] from alpha and col!
      
      if(is.numeric(connect.lines) & connect.lines > (nrow(Lt) -1)){
        stop("cannot plot more connecting lines than number of points minus one")
      }
      
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
        
        colour      <- palette2[round(pos(pwsim2*100)+1)]
        segm$colour <- colour
        
        #### Plot lines
        
        if(alpha[2] == "graded"){
          if(!(col[2] %in% c("rainbow","heat.colors","terrain.colors","topo.colors","cm.colors") | length(col) >= 3)){
            suppressWarnings(segments3d(segm,alpha=alpha.grade*(segm$pwsim)^2,col=col[2]))}
          
          if(col[2] %in% c("rainbow","heat.colors","terrain.colors","topo.colors","cm.colors") | length(col) >= 3){
            suppressWarnings(segments3d(segm,alpha=alpha.grade*(segm$pwsim)^2,col=segm$colour))}
        }
        
        if(is.numeric(alpha[2])){
          if(!(col[2] %in% c("rainbow","heat.colors","terrain.colors","topo.colors","cm.colors") | length(col) >= 3)){
            suppressWarnings(segments3d(segm,alpha=alpha[2],col=col[2]))}
          
          if(col[2] %in% c("rainbow","heat.colors","terrain.colors","topo.colors","cm.colors") | length(col) >= 3){
            suppressWarnings(segments3d(segm,alpha=alpha[2],col=segm$colour))}
        }
        
      
      
      
      #### Legend
      
      if(legend == T){
        
        bgplot3d({
          par(mar=c(0,0,0,0))
          par(oma=c(0,0,0,0))
          
          if(col[2] %in% c("rainbow","heat.colors","terrain.colors","topo.colors","cm.colors") | length(col) >= 3){
            legend_image <- as.raster(matrix(rev(palette2), ncol=1))
          }
          if(!(col[2] %in% c("rainbow","heat.colors","terrain.colors","topo.colors","cm.colors") | length(col) >= 3)){
            legend_image <- as.raster(matrix(rep(col[2],101), ncol=1))
          }
          plot(c(0,2),c(0,1),type = 'n', axes = F,xlab = '', ylab = '')
          text(x=0.3, y = seq(0.1,.35,l=5), labels = seq(0,1,l=5),cex=1)
          rasterImage(legend_image, 0.1, 0.1, 0.22,0.35)
          text(0.18,0.05,"cosine similarity")
        })
      }
      
      
      
    }
    
    
    
    
    
    if(inherits(connect.lines,"numeric") && connect.lines > 0){
      
      ### Find nearest to each word
      
      which.indices       <- t(apply(cos.near, 1, order, decreasing = T)[ 1:(connect.lines+1), ])
      which.indices       <- as.data.frame(which.indices[,-1])
      
      pre           <- as.vector(t(which.indices))
      alternate     <- rep((1:n),each=connect.lines)
      
      indices       <- c(rbind(alternate,pre))
      
      
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
      
      colour      <- palette2[round(pos(pwsim2*100)+1)]
      segm$colour <- colour
      
      ### Plot
      
      if(alpha[2] == "graded"){
        if(!(col[2] %in% c("rainbow","heat.colors","terrain.colors","topo.colors","cm.colors") | length(col) >= 3)){
          suppressWarnings(segments3d(segm,alpha=alpha.grade*(segm$pwsim)^2,col=col[2]))
        }
        
        if(col[2] %in% c("rainbow","heat.colors","terrain.colors","topo.colors","cm.colors") | length(col) >= 3){
          suppressWarnings(segments3d(segm,alpha=alpha.grade*(segm$pwsim)^2,col=segm$colour))}
      }
      
      if(is.numeric(alpha[2])){
        if(!(col[2] %in% c("rainbow","heat.colors","terrain.colors","topo.colors","cm.colors") | length(col) >= 3)){
          suppressWarnings(segments3d(segm,alpha=alpha[2],col=col[2]))
        }
        
        if(col[2] %in% c("rainbow","heat.colors","terrain.colors","topo.colors","cm.colors") | length(col) >= 3){
          suppressWarnings(segments3d(segm,alpha=alpha[2],col=segm$colour))}
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
      
      colour0      <- palette1[round(pos(pwsim0*100)+1)]
      segm0$colour  <- colour0
      
      
      
      if(!(col[1] %in% c("rainbow","heat.colors","terrain.colors","topo.colors","cm.colors") | length(col) >= 3) && is.numeric(alpha[1])){
        suppressWarnings(segments3d(segm0,alpha=alpha[1],col=col[1]))
      }
      
      if(!(col[1] %in% c("rainbow","heat.colors","terrain.colors","topo.colors","cm.colors") | length(col) >= 3) && alpha[1] == "graded"){
        suppressWarnings(segments3d(segm0,alpha=alpha.grade*(segm0$pwsim)^2,col=col[1]))
      }  
      
      if((col[1] %in% c("rainbow","heat.colors","terrain.colors","topo.colors","cm.colors") | length(col) >= 3) && is.numeric(alpha[1])){
        suppressWarnings(segments3d(segm0,alpha=alpha[1],col=segm0$colour))
      }  
      
      if((col[1] %in% c("rainbow","heat.colors","terrain.colors","topo.colors","cm.colors") | length(col) >= 3) && alpha[1] == "graded"){
        suppressWarnings(segments3d(segm0,alpha=alpha.grade*(segm0$pwsim)^2,col=segm0$colour))
      } 
      
      
    }
    
  }
  
  Lt[,-which(colnames(Lt) %in% c("words","words2"))]
  
}else{warning("tvectors must be a matrix!")}
}
