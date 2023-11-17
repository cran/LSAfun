\name{analogy}
\alias{analogy}
\title{Analogy}
\encoding{latin1}

\description{Implements the \emph{king - man + woman = queen} analogy solving algorithm}

\details{
The analogy task is a popular benchmark for vector space models of meaning/word embeddings.
It is based on the rationale that proportinal analogies \emph{x1 is to x2 as y1 is to y2}, like \emph{man : king = woman : ?}  (correct answer: \emph{queen}), can be solved via the following operation on the respective word vectors (all normalized to unit norm) \code{king - man + woman = queen} (that is, the nearest vector to \code{king - man + woman} should be \code{queen}) (Mikolov et al., 2013).


The \code{analogy()} function comes in two variants, taking as input either three words (\code{x1}, \code{x2}, and \code{y1}) or two words (\code{x1} and \code{x2})

\itemize{

\item The variant with three input words (\code{x1}, \code{x2}, and \code{y1}) implements the standard analogy solving algorithm for analogies of the type \code{x1 : x2 = y1 : ?}, searching the \code{n} nearest neighbors for \code{x2 - x1 + y1} (all normalized to unit norm) as the best-fitting candidates for \code{y2}

\item The variant with two input words (\code{x1} and \code{x2}) only computes the difference between the two vectors (both normalized to unit norm) and the \code{n} nearest neighbors to the resulting difference vector 

}

}

\usage{analogy(x1,x2,y1=NA,n,tvectors=tvectors)}

\arguments{
  \item{x1}{a character vector specifying the first word of the first pair (\emph{man} in \emph{man : king = woman : ?})}
  \item{x2}{a character vector specifying the second word of the first pair (\emph{king} in \emph{man : king = woman : ?})}
  \item{y1}{a character vector specifying the first word of the second pair (\emph{woman} in \emph{man : king = woman : ?})}
  \item{n}{the number of neighbors to be computed}
  \item{tvectors}{the semantic space in which the computation is to be done (a numeric matrix where every row is a word vector)}

 
}
\value{
Returns a list containing a numeric vector and the nearest neighbors to that vector:

\itemize{

\item In the variant with three input words (\code{x1}, \code{x2}, and \code{y1}), returns:

\itemize{
\item{\code{y2_vec}} The result of \code{x2 - x1 + y1} (all normalized to unit norm) as a numeric vector 
\item{\code{y2_neighbors}} A named numeric vector of the \code{n} nearest neighbors to \code{y2_vec}. The neighbors are given as names of the vector, and their respective cosines to \code{y2_vec} as vector entries.
}

\item In the variant with two input words (\code{x1} and \code{x2}), returns:

\itemize{
\item{\code{x_diff_vec}} The result of \code{x2 - x1} (both normalized to unit norm) as a numeric vector 
\item{\code{x_diff_neighbors}} A named numeric vector of the \code{n} nearest neighbors to \code{x_diff_vec}. The neighbors are given as names of the vector, and their respective cosines to \code{x_diff_vec} as vector entries.
}
}

}

\author{
  Fritz Guenther
}
\seealso{
  \code{\link[LSAfun]{neighbors}}
}

\references{Mikolov, T., Yih, W. T., & Zweig, G. (2013). Linguistic regularities in continuous space word representations. In  \emph{Proceedings of the 2013 Conference of the North American Chapter of the Association for Computational Linguistics: Human Language Technologies (NAACL-HLT-2013)}. Association for Computational Linguistics.
}

\examples{data(wonderland)

analogy(x1="hatter",x2="mad",y1="cat",n=10,tvectors=wonderland)

analogy(x1="hatter",x2="mad",n=10,tvectors=wonderland)
}