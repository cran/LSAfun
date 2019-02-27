\name{plot_neighbors}
\alias{plot_neighbors}
\title{2D- or 3D-Plot of neighbors}
\encoding{latin1}

\description{2D- or 3D-Approximation of the neighborhood of a given word/sentence}

\details{
Attempts to create an image of the semantic neighborhood (based on cosine similarity) to a
given word, sentence/ document, or vector. An attempt is made to depict this subpart of the LSA
space in a two- or three-dimensional plot.\cr
To achieve this, either a Principal Component
Analysis (PCA) or a Multidimensional Scaling (MDS) is computed to preserve the interconnections between all the words in this
neighborhod as good as possible. Therefore, it is important to note that the image created from
this function is only the best two- or three-dimensional approximation to the true LSA space subpart.\cr
\cr
For creating pretty plots showing the similarity structure within this neighborhood best, set \code{connect.lines="all"} and \code{col="rainbow"}

}

\usage{plot_neighbors(x,n,connect.lines="all",start.lines=T,
   method="PCA",dims=3,axes=F,box=F,cex=1,legend=T, size = c(800,800),
   alpha="graded",alpha.grade = 1, col="rainbow",tvectors=tvectors,breakdown=FALSE,\dots)}

\arguments{
  \item{x}{a character vector of \code{length(x) = 1} or a numeric of \code{length=ncol(tvectors)} vector with same dimensionality as LSA space}
  \item{n}{the number of neighbors to be computed}
  \item{dims}{the dimensionality of the plot; set either \code{dims = 2} or \code{dims = 3}}
  \item{method}{the method to be applied; either a Principal Component Analysis (\code{method="PCA"}) or a Multidimensional Scaling (\code{method="MDS"}) }
  \item{connect.lines}{(3d plot only) the number of closest associate words each word is connected with via line. Setting \code{connect.lines="all"} (default) will draw all connecting lines and will automatically apply \code{alpha="graded"}; it will furthermore override the \code{start.lines} argument}
  \item{start.lines}{(3d plot only) whether lines shall be drawn between \code{x} and all the neighbors}
  \item{axes}{(3d plot only) whether axes shall be included in the plot}
  \item{box}{(3d plot only) whether a box shall be drawn around the plot}
  \item{cex}{(2d Plot only) A numerical value giving the amount by which plotting text should be magnified relative to the default.}
    \item{legend}{(3d plot only) whether a legend shall be drawn illustrating the color scheme of the \code{connect.lines}. The legend is inserted as a background bitmap to the plot using \code{\link[rgl]{bgplot3d}}. Therefore, they do not resize very gracefully (see the \code{\link[rgl]{bgplot3d}} documentation for more information).}
    \item{size}{(3d plot only) A numeric vector with two elements, the first specifying the width and the second specifying the height of the plot device.}
  \item{tvectors}{the semantic space in which the computation is to be done (a numeric matrix where every row is a word vector)}
  \item{breakdown}{if \code{TRUE}, the function \code{\link[LSAfun]{breakdown}} is applied to the input}
  \item{alpha}{(3d plot only) a vector of one or two numerics between 0 and 1 specifying the luminance of \code{start.lines} (first entry) and \code{connect.lines} (second entry). Specifying only one numeric will pass this value to both kinds of lines. With setting \code{alpha="graded"}, the luminance of every line will be adjusted to the cosine between the two words it connects.}
\item{alpha.grade}{(3d plot only) Only relevant if \code{alpha="graded"}. Specify a numeric value for \code{alpha.grade} to scale the luminance of all \code{start.lines} and \code{connect.lines} up (\code{alpha.grade} > 1) or down (\code{alpha.grade} < 1) by that factor.}
  \item{col}{(3d plot only) a vector of one or two characters specifying the color of \code{start.lines} (first entry) and \code{connect.lines} (second entry). Specifying only one colour will pass this colour to both kinds of lines. With setting \code{col ="rainbow"} (default), the colour of every line will be adjusted to the cosine between the two words it connects, according to the rainbow palette. Other available color palettes for this purpose are \code{heat.colors}, \code{terrain.colors}, \code{topo.colors}, and \code{cm.colors} (see \code{\link[grDevices]{rainbow}}). Additionally, you can customize any color scale of your choice by providing an input specifying more than two colors  \cr (for example \code{col = c("black","blue","red")}).}
  \item{\dots}{additional arguments which will be passed to \code{\link[rgl]{plot3d}} (in a three-dimensional plot only)}
}

\value{For three-dimensional plots:see \code{\link[rgl]{plot3d}}: this function is called for the side effect of drawing the plot; a vector of object IDs is returned\cr
\code{plot_neighbors} also gives the coordinate vectors of the words in the plot as a data frame}

\author{
  Fritz Günther, Taylor Fedechko
}
\seealso{
  \code{\link[lsa]{cosine}},
  \code{\link[LSAfun]{neighbors}},
  \code{\link[LSAfun]{multicos}},
  \code{\link[LSAfun]{plot_wordlist}},
  \code{\link[rgl]{plot3d}},
  \code{\link[stats]{princomp}}

}

\references{Landauer, T.K., & Dumais, S.T. (1997). A solution to Plato's problem: The Latent Semantic Analysis theory of acquisition, induction and representation of knowledge. \emph{Psychological Review, 104,} 211-240.\cr\cr

Mardia, K.V., Kent, J.T., & Bibby, J.M. (1979). \emph{Multivariate Analysis}, London: Academic Press.
}

\examples{data(wonderland)

## Standard Plot
plot_neighbors("cheshire",n=20,tvectors=wonderland)  

## Pretty Plot
plot_neighbors("cheshire",n=20,tvectors=wonderland,
                connect.lines="all",col="rainbow")  



plot_neighbors(compose("mad","hatter",tvectors=wonderland),
                n=20, connect.lines=2,tvectors=wonderland)}