\name{Predication}
\alias{Predication}
\title{Compute Vector for Predicate-Argument-Expressions}
\encoding{latin1}

\description{Computes vectors for complex expressions of type PREDICATE[ARGUMENT] by applying the method of Kintsch (2001) (see \emph{Details}).}

\details{
The vector for the expression is computed following the Predication Process by Kintsch (2001):\cr
The \code{m} nearest neighbors to the Predicate are computed. Of those, the \code{k} nearest neighbors to
the Argument are selected. The vector for the expression is then computed as the sum of
Predicate vector, Argument vector, and the vectors of those \code{k} neighbors (the \code{k}-neighborhood).
}

\usage{Predication(P,A,m,k,tvectors=tvectors,breakdown=TRUE,norm="none")}

\arguments{
  \item{P}{Predicate of the expression, a single word (character vector)}
  \item{A}{Argument of the expression, a single word (character vector)}
  \item{m}{number of nearest words to the Predicate that are initially activated}
  \item{k}{size of the \code{k}-neighborhood; \code{k} \eqn{\le} \code{m}}
  \item{tvectors}{the semantic space in which the computation is to be done (a numeric matrix where every row is a word vector)}
  \item{breakdown}{if \code{TRUE}, the function \code{\link[LSAfun]{breakdown}} is applied to the input}
  \item{norm}{whether to \code{\link[LSAfun]{normalize}} the single word vectors before applying a composition function. Setting \code{norm = "none"} will not perform any normalizations, setting \code{norm = "all"} will normalize every involved word vector (Predicate, Argument, and every single activated neighbor). Setting \code{norm = "block"} will normalize the Argument vector and will normalize the [Predicate + neighbors] vector, to weight the Argument and the "Predicate in context" equally.}
  
  }
 

\value{An object of class \code{Pred}: This object is a list consisting of:\cr
\item{\code{$PA}}{The vector for the complex expression as described above}
\item{\code{$P.Pred}}{The vector for Predicate plus the \emph{k}-neighborhoodvectors without the Argument vector}
\item{\code{$neighbors}}{The words in the \emph{k}-neighborhood.} 
\item{\code{$P}}{The Predicate given as input}
\item{\code{$A}}{The Argument given as input}
}

\author{
  Fritz Günther
}

\seealso{
  \code{\link[lsa]{cosine}},
  \code{\link[LSAfun]{neighbors}},
  \code{\link[LSAfun]{multicos}},
  \code{\link[LSAfun]{compose}}
  
}

\references{
Kintsch, W. (2001). Predication. \emph{Cognitive science, 25,} 173-202.
}

\examples{data(wonderland)

Predication(P="mad",A="hatter",m=20,k=3,tvectors=wonderland)}