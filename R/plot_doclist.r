################################################
##### Plot Wordlist ############################

#' @export plot_doclist
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
plot_doclist <- function(x,connect.lines="all",
                          method="PCA",dims=3,
                          axes=F,box=F,cex=1,chars=10,
                          legend=T, size = c(800,800),
                          alpha="graded", alpha.grade = 1, col="rainbow", 
                          tvectors=tvectors,breakdown=FALSE,
                          ...){
  
  ### Compute neighbors
  
  if(!(dims %in% 2:3)){stop("Please set dim to 2 or 3")}
  
  if(class(tvectors) == "data.frame"){
    tvectors <- as.matrix(tvectors)
  }else if(class(tvectors) == "textmatrix"){
    tvectors <- matrix(tvectors,
                       nrow=nrow(tvectors),ncol=ncol(tvectors),
                       dimnames=list(rownames(tvectors),colnames(tvectors)))
  }
  
  if(class(tvectors) == "matrix"){
    
    if(class(x) == "factor"){
      x <- as.character(x)
      message("Note: x converted to character")
    }
    
    
    if(class(x) == "character"){
      
      if(breakdown==TRUE){satz1 <- breakdown(x)} 
      if(breakdown==FALSE){satz1 <- x}  
      
      used1      <- satz1
      
      n <- length(used1)
      
      cosis  <- multidocs(used1,tvectors=tvectors,chars=chars,
                            breakdown=FALSE)
      
      cos.near <- cosis$cosmat
      colnames(cos.near) <- rownames(cos.near)
      xdocs    <- cosis$xdocs
      ydocs    <- cosis$ydocs
      
      
    }
    
    
    if(class(x) != "character"){
      
      stop("x must be a character vector!")    
      
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
    
    if(dims == 2){
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
      
      ### alpha="shade" backwards compatibility
      if(alpha=="shade"){alpha <- "graded"}
      
      
      ### Colour palette
      
      palette <- rainbow(101,start=0,end=0.95)
      
      if(col[1] == "heat.colors"){palette <- heat.colors(101)}
      if(col[1] == "terrain.colors"){palette <- terrain.colors(101)}
      if(col[1] == "topo.colors"){palette <- topo.colors(101)}
      if(col[1] == "cm.colors"){palette <- topo.colors(101)}
      
      if(length(col) == 2){
        col <- rep(col,each=2)
      }
      
      if(length(col) >= 3){
        palette <- colorRampPalette(col)(101)
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
        
        colour      <- palette[round(pos(pwsim2*100)+1)]
        segm$colour <- colour
        
        #### Plot lines
        
        if(alpha == "graded"){
          if(!(col[1] %in% c("rainbow","heat.colors","terrain.colors","topo.colors","cm.colors")  | length(col) >= 3)){
            suppressWarnings(segments3d(segm,alpha=alpha.grade*(segm$pwsim)^2,col=col))}
          
          if(col[1] %in% c("rainbow","heat.colors","terrain.colors","topo.colors","cm.colors")  | length(col) >= 3){
            suppressWarnings(segments3d(segm,alpha=alpha.grade*(segm$pwsim)^2,col=segm$colour))}
        }
        
        if(is.numeric(alpha)){
          if(!(col[1] %in% c("rainbow","heat.colors","terrain.colors","topo.colors","cm.colors")  | length(col) >= 3)){
            suppressWarnings(segments3d(segm,alpha=alpha,col=col))}
          
          if(col[1] %in% c("rainbow","heat.colors","terrain.colors","topo.colors","cm.colors")  | length(col) >= 3){
            suppressWarnings(segments3d(segm,alpha=alpha,col=segm$colour))}
        }
        
      }
      
      #### Legend
      
      if(legend == T){
        
        bgplot3d({
          par(mar=c(0,0,0,0))
          par(oma=c(0,0,0,0))
          
          if(col[1] %in% c("rainbow","heat.colors","terrain.colors","topo.colors","cm.colors") | length(col) >= 3){
            legend_image <- as.raster(matrix(rev(palette), ncol=1))
          }
          if(!(col[1] %in% c("rainbow","heat.colors","terrain.colors","topo.colors","cm.colors") | length(col) >= 3)){
            legend_image <- as.raster(matrix(rep(col[1],101), ncol=1))
          }
          plot(c(0,2),c(0,1),type = 'n', axes = F,xlab = '', ylab = '')
          text(x=0.3, y = seq(0.1,.35,l=5), labels = seq(0,1,l=5),cex=1)
          rasterImage(legend_image, 0.1, 0.1, 0.22,0.35)
          text(0.18,0.05,"cosine similarity")
        })
      }
    
      
      
      
      if(class(connect.lines) == "numeric" && connect.lines > 0){
        
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
        
        colour      <- palette[round(pos(pwsim2*100)+1)]
        segm$colour <- colour
        
        ### Plot
        
        #### Plot lines
        
        if(alpha == "graded"){
          if(!(col[1] %in% c("rainbow","heat.colors","terrain.colors","topo.colors","cm.colors")   | length(col) >= 3)){
            suppressWarnings(segments3d(segm,alpha=alpha.grade*(segm$pwsim)^2,col=col))}
          
          if(col[1] %in% c("rainbow","heat.colors","terrain.colors","topo.colors","cm.colors")   | length(col) >= 3){
            suppressWarnings(segments3d(segm,alpha=alpha.grade*(segm$pwsim)^2,col=segm$colour))}
        }
        
        if(is.numeric(alpha)){
          if(!(col[1] %in% c("rainbow","heat.colors","terrain.colors","topo.colors","cm.colors")   | length(col) >= 3)){
            suppressWarnings(segments3d(segm,alpha=alpha,col=col))}
          
          if(col[1] %in% c("rainbow","heat.colors","terrain.colors","topo.colors","cm.colors")   | length(col) >= 3){
            suppressWarnings(segments3d(segm,alpha=alpha,col=segm$colour))}
        }
        
      }    
      
      

      
    }
    
    return(list(coordinates=Lt[,-which(colnames(Lt) %in% c("words","words2"))],
                xdocs=xdocs))
    
  }else{warning("tvectors must be a matrix!")}
}
