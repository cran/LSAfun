\name{neighbors}
\alias{neighbors}
\title{Find nearest neighbors}
\encoding{latin1}

\description{Returns the n nearest words to a given word or sentence/document}

\details{
The format of \code{x} should be of the kind \code{x <- "word1 word2 word3"} instead of \cr
\code{x <- c("word1", "word2", "word3")} if sentences/documents are used as input. This allows for simple copy&paste-inserting of text.\cr\cr
To import a document \emph{Document.txt} to from a directory for comparisons, set your working
directory to this directory using \code{setwd()}. Then use the following command lines:\cr
\code{fileName1 <- "Alice_in_Wonderland.txt"}\cr
\code{x <- readChar(fileName1, file.info(fileName1)$size)}.\cr\cr
Since \code{x} can also be chosen to be any vector of the active LSA Space, this function can be
combined with \code{compose()} to compute neighbors of complex expressions (see examples)
}

\usage{neighbors(x,n,tvectors=tvectors,breakdown=TRUE)}

\arguments{
  \item{x}{a character vector of \code{length(x) = 1} or a numeric of \code{length=ncol(tvectors)} vector with same dimensionality as LSA space}
  \item{n}{the number of neighbors to be computed}
  \item{tvectors}{the semantic space in which the computation is to be done (a numeric matrix where every row is a word vector)}
  \item{breakdown}{if \code{TRUE}, the function \code{\link[LSAfun]{breakdown}} is applied to the input}
 
}
\value{A named numeric vector. The neighbors are given as names of the vector, and their respective cosines to the input as vector entries.}

\author{
  Fritz Günther
}
\seealso{
  \code{\link[lsa]{cosine}},
  \code{\link[LSAfun]{plot_neighbors}},
  \code{\link[LSAfun]{compose}}
  
}

\references{Landauer, T.K., & Dumais, S.T. (1997). A solution to Plato's problem: The Latent Semantic Analysis theory of acquisition, induction and representation of knowledge. \emph{Psychological Review, 104,} 211-240.\cr\cr

Dennis, S. (2007). How to use the LSA Web Site. In T. K. Landauer, D. S. McNamara, S. Dennis, & W. Kintsch (Eds.), \emph{Handbook of Latent
Semantic Analysis} (pp. 35-56). Mahwah, NJ: Erlbaum.\cr\cr

\url{http://lsa.colorado.edu/}
}

\examples{data(wonderland)

neighbors("cheshire",n=20,tvectors=wonderland) 

neighbors(compose("mad","hatter",method="Add",tvectors=wonderland),
n=20,tvectors=wonderland)}