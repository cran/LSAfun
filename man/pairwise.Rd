\name{pairwise}
\alias{pairwise}
\title{Pairwise cosine computation}
\encoding{latin1}

\description{Computes pairwise cosine similarities}

\details{Computes pairwise cosine similarities for two vectors of words. These vectors need to have the same length.}

\usage{pairwise(x,y,tvectors=tvectors)}

\arguments{
  \item{x}{a character vector}
  \item{y}{a character vector}
  \item{tvectors}{the semantic space in which the computation is to be done (a numeric matrix where every row is a word vector)}



}
\value{A vector of the same length as \code{x} and \code{y} containing the pairwise cosine similarities. Returns \code{NA} if at least one word in a pair is not found in the semantic space.}

\author{
  Fritz Guenther
}
\seealso{
  \code{\link[lsa]{cosine}},
  \code{\link[LSAfun]{Cosine}},
  \code{\link[LSAfun]{multicos}},
}

\references{Landauer, T.K., & Dumais, S.T. (1997). A solution to Plato's problem: The Latent Semantic Analysis theory of acquisition, induction and representation of knowledge. \emph{Psychological Review, 104,} 211-240.


Dennis, S. (2007). How to use the LSA Web Site. In T. K. Landauer, D. S. McNamara, S. Dennis, & W. Kintsch (Eds.), \emph{Handbook of Latent Semantic Analysis} (pp. 35-56). Mahwah, NJ: Erlbaum.


\url{http://wordvec.colorado.edu/}
}


\examples{data(wonderland)
pairwise("mouse rabbit cat","king queen hearts",
          tvectors=wonderland)}