\name{multicos}
\alias{multicos}
\title{Vector x Vector Comparison}
\encoding{latin1}

\description{Computes a cosine matrix from given word vectors}

\details{Submit a character vector consisting of \emph{n} words to get a \emph{n x n} cosine matrix of all their pairwise
cosines.\cr
Alternatively, submit two different character vectors to get their pairwise cosines.
Single words are also possible arguments.\cr
Also allows for computation of cosines between a given numeric vector with the same
dimensionality as the LSA space and a vector consisting of n words.
}

\usage{multicos(x,y=x,tvectors=tvectors,breakdown=TRUE)}

\arguments{
  \item{x}{a character vector or numeric of \code{length=ncol(tvectors)}
(vector with same dimensionality as LSA space)}
  \item{y}{a character vector; y = x by default}
  \item{tvectors}{the semantic space in which the computation is to be done (a numeric matrix where every row is a word vector)}
    \item{breakdown}{if \code{TRUE}, the function \code{\link[LSAfun]{breakdown}} is applied to the input}


}
\value{A matrix containing the pairwise cosines of \code{x} and \code{y}}

\author{
  Fritz Günther
}
\seealso{
  \code{\link[lsa]{cosine}},
  \code{\link[LSAfun]{Cosine}},
  \code{\link[LSAfun]{costring}},
  \code{\link[LSAfun]{multicostring}}
}

\references{Landauer, T.K., & Dumais, S.T. (1997). A solution to Plato's problem: The Latent Semantic Analysis theory of acquisition, induction and representation of knowledge. \emph{Psychological Review, 104,} 211-240.\cr\cr

Dennis, S. (2007). How to use the LSA Web Site. In T. K. Landauer, D. S. McNamara, S. Dennis, & W. Kintsch (Eds.), \emph{Handbook of Latent
Semantic Analysis} (pp. 35-56). Mahwah, NJ: Erlbaum.\cr\cr

\url{http://lsa.colorado.edu/}
}


\examples{data(wonderland)
multicos("mouse rabbit cat","king queen",
          tvectors=wonderland)}