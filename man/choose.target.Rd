\name{choose.target}
\alias{choose.target}
\title{Random Target Selection}
\encoding{latin1}

\description{Randomly samples words within a given similarity range to the input}

\details{
Computes cosine values between the input \code{x} and all the word vectors in \code{tvectors}. Then only selects words with a cosine similarity between \code{lower} and \code{upper} to the input, and randomly samples \code{n} of these words.\cr
This function is designed for randomly selecting target words with a predefined similarity towards a given prime word (or sentence/document).
}

\usage{choose.target(x,lower,upper,n,tvectors=tvectors,breakdown=TRUE)}

\arguments{
  \item{x}{a character vector of \code{length(x) = 1} specifying a word or a sentence/document}
  \item{lower}{the lower bound of the similarity range; a numeric}
  \item{upper}{the upper bound of the similarity range; a numeric}
  \item{n}{an integer giving the number of target words to be sampled}
  \item{tvectors}{the semantic space in which the computation is to be done (a numeric matrix where every row is a word vector)}
  \item{breakdown}{if \code{TRUE}, the function \code{\link[LSAfun]{breakdown}} is applied to the input}

 
}

\value{A named numeric vector. The names of the vector give the target words, the entries their respective cosine similarity to the input.}

\author{
  Fritz Günther
}
\seealso{
  \code{\link[lsa]{cosine}},
  \code{\link[LSAfun]{Cosine}},
  \code{\link[LSAfun]{neighbors}}

}

\references{Landauer, T.K., & Dumais, S.T. (1997). A solution to Plato's problem: The Latent Semantic Analysis theory of acquisition, induction and representation of knowledge. \emph{Psychological Review, 104,} 211-240.\cr\cr
}

\examples{data(wonderland)

choose.target("mad hatter",lower=.2,upper=.3,
                n=20, tvectors=wonderland)}