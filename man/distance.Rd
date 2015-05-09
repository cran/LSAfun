\name{distance}
\alias{distance}
\title{Compute distance}
\encoding{latin1}

\description{Computes distance metrics for two single words}

\details{Computes Minkowski metrics, i.e. geometric distances between the vectors for two given words. Possible options are \code{euclidean} for the Euclidean Distance, \eqn{ d(x,y) = \sqrt{\sum{(x-y)^2}}}, and \code{cityblock} for the City Block metric, \eqn{ d(x,y) = \sum{|x-y|}}
}

\usage{distance(x,y,method="euclidean",tvectors=tvectors,breakdown=TRUE)}

\arguments{
  \item{x}{A single word, given as a character of \code{length(x) = 1}}
  \item{y}{A single word, given as a character of \code{length(y) = 1}}
  \item{method}{Specifies whether to compute \code{euclidean} or \code{cityblock} metric}
  \item{tvectors}{the semantic space in which the computation is to be done (a numeric matrix where every row is a word vector)}
  \item{breakdown}{if \code{TRUE}, the function \code{\link[LSAfun]{breakdown}} is applied to the input}

}
\value{The distance value as a numeric}

\author{
  Fritz Günther
}
\seealso{
  \code{\link[LSAfun]{Cosine}}
  \code{\link[LSAfun]{asym}}
  
}




\examples{data(wonderland)

distance("alice","rabbit",method="euclidean",tvectors=wonderland)}