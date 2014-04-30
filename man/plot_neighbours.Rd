\name{plot_neighbours}
\alias{plot_neighbours}
\title{3D-Plot of neighbours}
\encoding{latin1}

\description{3D-Approximation of the neighbourhood of a given word/sentence}

\details{
Attempts to create an image of the semantic neighbourhood (based on cosine similarity) to a
given word, sentence/ document, or vector. An attempt is made to depict this subpart of the LSA
space in a three-dimensional plot.\cr
To achieve this, a Principal Component
Analysis (PCA) is computed to preserve the interconnections between all the words in this
neighbourhod as good as possible. Therefore, it is important to note that the image created from
this function is only the best three-dimensional approximation to the true LSA space subpart.\cr
\cr
For creating pretty plots showing the similarity structure within this neighbourhood best, set \code{connect.lines="all"} and \code{col="rainbow"}

}

\usage{plot_neighbours(x,n,connect.lines=0,start.lines=T,
 axes=F,box=F,alpha=0.5,col="black",tvectors=tvectors,breakdown=TRUE,\dots)}

\arguments{
  \item{x}{a character vector of \code{length(x) = 1} or a numeric of \code{length=ncol(tvectors)} vector with same dimensionality as LSA space}
  \item{n}{the number of neighbours to be computed}
  \item{connect.lines}{the number of closest associate words each word is connected with via line. Setting \code{connect.lines="all"} will draw all connecting lines and will automatically apply \code{alpha="shade"}; it will furthermore override the \code{start.lines} argument}
  \item{start.lines}{whether lines shall be drawn between \code{x} and all the neighbours}
  \item{axes}{whether axes shall be included in the plot}
  \item{box}{whether a box shall be drawn around the plot}
  \item{tvectors}{the semantic space in which the computation is to be done (a numeric matrix where every row is a word vector)}
  \item{breakdown}{if \code{TRUE}, the function \code{\link[LSAfun]{breakdown}} is applied to the input}
  \item{alpha}{a vector of one or two numerics between 0 and 1 specifying the luminance of \code{start.lines} (first entry) and \code{connect.lines} (second entry). Specifying only one numeric will pass this value to both kinds of lines. With setting \code{alpha="shade"}, the luminance of every line will be adjusted to the cosine between the two words it connects.}
  \item{col}{a vector of one or two characters specifying the color of \code{start.lines} (first entry) and \code{connect.lines} (second entry). Specifying only one colour will pass this colour to both kinds of lines. With setting \code{col ="rainbow"}, the colour of every line will be adjusted to the cosine between the two words it connects. Setting \code{col ="rainbow"} will alsp apply \code{alpha="shade"}}
  \item{\dots}{additional arguments which will be passed to \code{\link[rgl]{plot3d}}}
 
 
}
\value{see \code{\link[rgl]{plot3d}}: this function is called for the side effect of drawing the plot; a vector of object IDs is returned}

\author{
  Fritz Günther
}
\seealso{
  \code{\link[lsa]{cosine}},
  \code{\link[LSAfun]{neighbours}},
  \code{\link[LSAfun]{multicos}},
  \code{\link[rgl]{plot3d}},
  \code{\link[stats]{princomp}}

}

\references{Landauer, T.K., & Dumais, S.T. (1997). A solution to Plato's problem: The Latent Semantic Analysis theory of acquisition, induction and representation of knowledge. \emph{Psychological Review, 104,} 211-240.\cr\cr

Mardia, K.V., Kent, J.T., & Bibby, J.M. (1979). \emph{Multivariate Analysis}, London: Academic Press.
}

\examples{data(wonderland)

## Standard Plot
plot_neighbours("cheshire",n=20,tvectors=wonderland)  

## Pretty Plot
plot_neighbours("cheshire",n=20,tvectors=wonderland,
                connect.lines="all",col="rainbow")  



plot_neighbours(compose("mad","hatter",tvectors=wonderland),
                n=20, connect.lines=2,tvectors=wonderland)}