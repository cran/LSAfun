\name{Cosine}
\alias{Cosine}
\title{Compute cosine similarity}
\encoding{latin1}

\description{Computes the cosine similarity for two single words}

\details{Instead of using numeric vectors, as the \code{cosine()} function from the lsa package does, this function allows for the direct computation of the cosine between two single words (i.e. Characters). which are automatically searched for in the LSA space given in as \code{tvectors}.}


\usage{Cosine(x,y,tvectors=tvectors,breakdown=TRUE)}

\arguments{
  \item{x}{A single word, given as a character of \code{length(x) = 1}}
  \item{y}{A single word, given as a character of \code{length(y) = 1}}
  \item{tvectors}{the semantic space in which the computation is to be done (a numeric matrix where every row is a word vector)}
  \item{breakdown}{if \code{TRUE}, the function \code{\link[LSAfun]{breakdown}} is applied to the input}

}
\value{The cosine similarity as a numeric}

\author{
  Fritz Günther
}
\seealso{
  \code{\link[LSAfun]{distance}}
  \code{\link[LSAfun]{asym}}
  
}

\references{Landauer, T.K., & Dumais, S.T. (1997). A solution to Plato's problem: The Latent Semantic Analysis theory of acquisition, induction and representation of knowledge. \emph{Psychological Review, 104,} 211-240.\cr\cr

Dennis, S. (2007). How to use the LSA Web Site. In T. K. Landauer, D. S. McNamara, S. Dennis, & W. Kintsch (Eds.), \emph{Handbook of Latent
Semantic Analysis} (pp. 35-56). Mahwah, NJ: Erlbaum.\cr\cr

\url{http://lsa.colorado.edu/}
}


\examples{data(wonderland)

Cosine("alice","rabbit",tvectors=wonderland)}