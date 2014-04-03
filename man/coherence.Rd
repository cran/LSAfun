\name{coherence}
\alias{coherence}
\title{Coherence of a text}
\encoding{latin1}

\description{Computes coherence of a given paragraph/document}

\details{
This function applies the method described in Landauer & Dumais (1997): The \emph{local coherence} is the cosine
between two adjacent sentences. The \emph{global coherence} is then computed as the mean value of these local
coherences.\cr
The format of \code{x} (or \code{y}) should be of the kind \code{x <- "sentence1. sentence2. sentence3"} Every sentence can also just consist of one single word.\cr
To import a document Document.txt to from a directory for coherence computation, set your working
directory to this directory using \code{setwd()}. Then use the following command lines:\cr\cr
\code{fileName1 <- "Alice_in_Wonderland.txt"}\cr
\code{x <- readChar(fileName1, file.info(fileName1)$size)}

}

\usage{coherence(x,split=c(".","!","?"),tvectors=tvectors,breakdown=TRUE)}

\arguments{
  \item{x}{a character vector of \code{length(x) = 1} containing the document}
  \item{split}{a vector of expressions that determine where to split sentences}
  \item{tvectors}{the semantic space in which the computation is to be done (a numeric matrix where every row is a word vector)}
  \item{breakdown}{if \code{TRUE}, the function \code{\link[LSAfun]{breakdown}} is applied to the input}
 
}
\value{A list of two elements; the first element (\code{$local}) contains the local coherences as a numeric vector, the second element (\code{$global}) contains the global coherence as a numeric.}

\author{
  Fritz Günther
}
\seealso{
  \code{\link[lsa]{cosine}},
  \code{\link[LSAfun]{Cosine}},
  \code{\link[LSAfun]{costring}}

}

\references{Landauer, T.K., & Dumais, S.T. (1997). A solution to Plato's problem: The Latent Semantic Analysis theory of acquisition, induction and representation of knowledge. \emph{Psychological Review, 104,} 211-240.\cr\cr
}

\examples{data(wonderland)

coherence ("There was certainly too much of it in the air. Even the Duchess
sneezed occasionally; and as for the baby, it was sneezing and howling
alternately without a moment's pause. The only things in the kitchen
that did not sneeze, were the cook, and a large cat which was sitting on
the hearth and grinning from ear to ear.",
tvectors=wonderland)}