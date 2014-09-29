\name{plot_wordlist}
\alias{plot_wordlist}
\title{2D- or 3D-Plot of a list of words}
\encoding{latin1}

\description{2D or 3D-Plot of mutual word similarities to a given list of words}

\details{
Computes all pairwise similarities within a given list of words. On this similarity matrix, a Principal Component Analysis (PCA) or a Multidimensional Sclaing (MDS) is applied to get a two- or three-dimensional solution that best captures the similarity structure. This solution is then plottet.
\cr
For creating pretty plots showing the similarity structure within this list of words best, set \code{connect.lines="all"} and \code{col="rainbow"}

}

\usage{plot_wordlist(x,connect.lines=0,method="PCA",dims=3,
   axes=F,box=F,cex=1,alpha=0.5,col="black",
   tvectors=tvectors,breakdown=TRUE,\dots)}

\arguments{
  \item{x}{a character vector of \code{length(x) > 1} that contains multiple words}
  \item{dims}{the dimensionality of the plot; set either \code{dims = 2} or \code{dims = 3}}
  \item{method}{the method to be applied; either a Principal Component Analysis (\code{method="PCA"}) or a Multidimensional Scaling (\code{method="MDS"}) }
  \item{connect.lines}{(3d plot only) the number of closest associate words each word is connected with via line. Setting \code{connect.lines="all"} will draw all connecting lines and will automatically apply \code{alpha="shade"}; it will furthermore override the \code{start.lines} argument}
  \item{axes}{(3d plot only) whether axes shall be included in the plot}
  \item{box}{(3d plot only) whether a box shall be drawn around the plot}
  \item{cex}{(2d Plot only) A numerical value giving the amount by which plotting text should be magnified relative to the default.}
  \item{tvectors}{the semantic space in which the computation is to be done (a numeric matrix where every row is a word vector)}
  \item{breakdown}{if \code{TRUE}, the function \code{\link[LSAfun]{breakdown}} is applied to the input}
  \item{alpha}{(3d plot only) a vector of one or two numerics between 0 and 1 specifying the luminance of \code{start.lines} (first entry) and \code{connect.lines} (second entry). Specifying only one numeric will pass this value to both kinds of lines. With setting \code{alpha="shade"}, the luminance of every line will be adjusted to the cosine between the two words it connects.}
  \item{col}{(3d plot only) a vector of one or two characters specifying the color of \code{start.lines} (first entry) and \code{connect.lines} (second entry). Specifying only one colour will pass this colour to both kinds of lines. With setting \code{col ="rainbow"}, the colour of every line will be adjusted to the cosine between the two words it connects. Setting \code{col ="rainbow"} will alsp apply \code{alpha="shade"}}
  \item{\dots}{additional arguments which will be passed to \code{\link[rgl]{plot3d}} (in a three-dimensional plot only)}
 
 
}
\value{see \code{\link[rgl]{plot3d}}: this function is called for the side effect of drawing the plot; a vector of object IDs is returned.\cr
\code{plot_neighbors} also gives the coordinate vectors of the words in the plot as a data frame}

\author{
  Fritz Günther
}
\seealso{
  \code{\link[lsa]{cosine}},
  \code{\link[LSAfun]{neighbors}},
  \code{\link[LSAfun]{multicos}},
  \code{\link[LSAfun]{plot_neighbors}},
  \code{\link[rgl]{plot3d}},
  \code{\link[stats]{princomp}}

}

\references{Landauer, T.K., & Dumais, S.T. (1997). A solution to Plato's problem: The Latent Semantic Analysis theory of acquisition, induction and representation of knowledge. \emph{Psychological Review, 104,} 211-240.\cr\cr

Mardia, K.V., Kent, J.T., & Bibby, J.M. (1979). \emph{Multivariate Analysis}, London: Academic Press.
}

\examples{data(wonderland)

## Standard Plot

words <- c("alice","hatter","queen","knight","hare","cheshire") 
            
plot_wordlist(words,tvectors=wonderland,method="MDS",dims=2)

}
