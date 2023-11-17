\name{plot_doclist}
\alias{plot_doclist}
\title{2D- or 3D-Plot of a list of sentences/documents}
\encoding{latin1}

\description{2D or 3D-Plot of mutual word similarities to a given list of sentences/documents}

\details{Computes all pairwise similarities within a given list of sentences/documents. On this similarity matrix, a Principal Component Analysis (PCA) or a Multidimensional Sclaing (MDS) is applied to get a two- or three-dimensional solution that best captures the similarity structure. This solution is then plotted.


In the traditional LSA approach, the vector \emph{D} for a document (or a sentence) consisting of the words \emph{(t1, . , tn)} is computed as
             \deqn{D = \sum\limits_{i=1}^n t_n}
This function then computes the the cosines between two sets of documents (or sentences).


     The format of \code{x} should be of the kind \code{x <- c("this is the first text","here is another text")}
     
     
For creating pretty plots showing the similarity structure within this list of words best, set \code{connect.lines="all"} and \code{col="rainbow"}
}

\usage{plot_doclist(x,connect.lines="all",method="PCA",dims=3,
   axes=F,box=F,cex=1,chars=10,legend=T, size = c(800,800),
   alpha="graded",alpha.grade=1,col="rainbow",
   tvectors=tvectors,remove.punctuation=TRUE,\dots)}

\arguments{
  \item{x}{a character vector of \code{length(x) > 1} that contains multiple sentences/documents}
  \item{dims}{the dimensionality of the plot; set either \code{dims = 2} or \code{dims = 3}}
  \item{method}{the method to be applied; either a Principal Component Analysis (\code{method="PCA"}) or a Multidimensional Scaling (\code{method="MDS"}) }
  \item{connect.lines}{(3d plot only) the number of closest associate words each word is connected with via line. Setting \code{connect.lines="all"} (default) will draw all connecting lines and will automatically apply \code{alpha="graded"}}
  \item{axes}{(3d plot only) whether axes shall be included in the plot}
  \item{box}{(3d plot only) whether a box shall be drawn around the plot}
  \item{cex}{(2d Plot only) A numerical value giving the amount by which plotting text should be magnified relative to the default.}
  \item{chars}{an integer specifying how many letters (starting from the first) of each sentence/document are to be printed in the plot}
    \item{legend}{(3d plot only) whether a legend shall be drawn illustrating the color scheme of the \code{connect.lines}. The legend is inserted as a background bitmap to the plot using \code{\link[rgl]{bgplot3d}}. Therefore, they do not resize very gracefully (see the \code{\link[rgl]{bgplot3d}} documentation for more information).}
    \item{size}{(3d plot only) A numeric vector with two elements, the first specifying the width and the second specifying the height of the plot device.}
  \item{tvectors}{the semantic space in which the computation is to be done (a numeric matrix where every row is a word vector)}
      \item{remove.punctuation}{removes punctuation from \code{x} and \code{y}; \code{TRUE} by default}
  \item{alpha}{(3d plot only) A numeric vector specifying the luminance of the \code{connect.lines}. By setting \code{alpha="graded"}, the luminance of every line will be adjusted to the cosine between the two words it connects.}
  \item{alpha.grade}{(3d plot only) Only relevant if \code{alpha="graded"}. Specify a numeric value for \code{alpha.grade} to scale the luminance of all \code{connect.lines} up (\code{alpha.grade} > 1) or down (\code{alpha.grade} < 1) by that factor.}
  \item{col}{(3d plot only) A vector specifying the color of the \code{connect.lines}.  With setting \code{col ="rainbow"} (default), the color of every line will be adjusted to the cosine between the two words it connects, according to the rainbow palette. Other available color palettes for this purpose are \code{heat.colors}, \code{terrain.colors}, \code{topo.colors}, and \code{cm.colors} (see \code{\link[grDevices]{rainbow}}). Additionally, you can customize any color scale of your choice by providing an input specifying more than one color (for example \code{col = c("black","blue","red")}).}
  \item{\dots}{additional arguments which will be passed to \code{\link[rgl]{plot3d}} (in a three-dimensional plot only)}
}

\value{see \code{\link[rgl]{plot3d}}: this function is called for the side effect of drawing the plot; a vector of object IDs is returned.


\code{plot_doclist} further prints a list with two elements:
\item{coordinates}{the coordinate vectors of the sentences/documents in the plot as a data frame}
\item{xdocs}{A legend for the sentence/document labels in the plot and in the \code{coordinates}}
}

\author{
  Fritz Guenther, Taylor Fedechko
}

\seealso{
  \code{\link[lsa]{cosine}},
  \code{\link[LSAfun]{multidocs}},
  \code{\link[LSAfun]{plot_neighbors}},
  \code{\link[LSAfun]{plot_wordlist}},
  \code{\link[rgl]{plot3d}},
  \code{\link[stats]{princomp}},
  \code{\link[grDevices]{rainbow}}
}

\references{Landauer, T.K., & Dumais, S.T. (1997). A solution to Plato's problem: The Latent Semantic Analysis theory of acquisition, induction and representation of knowledge. \emph{Psychological Review, 104,} 211-240.


Mardia, K.V., Kent, J.T., & Bibby, J.M. (1979). \emph{Multivariate Analysis}, London: Academic Press.
}

\examples{data(wonderland)

## Standard Plot

docs <- c("alice was beginning to get very tired.",
          "the red queen greeted alice.",
          "the mad hatter and the mare hare are having a party.",
          "the hatter sliced the cup of tea in half.")
          
plot_doclist(docs,tvectors=wonderland,method="MDS",dims=2)

}
