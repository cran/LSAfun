\name{costring}
\alias{costring}
\title{Sentence Comparison}
\encoding{latin1}

\description{Computes cosine values between sentences and/or documents}

\details{
In the traditional LSA approach, the vector \emph{D} for a document (or a sentence) consisting of the words \emph{(t1, . , tn)} is computed as
             \deqn{D = \sum\limits_{i=1}^n t_n}
             
This function computes the cosine between two documents (or sentences) or the cosine between a single word and a document (or sentence).\cr\cr
     The format of \code{x} (or \code{y}) can be of the kind \code{x <- "word1 word2 word3"} , but also of the kind \code{x <- c("word1", "word2", "word3")}. This allows for simple copy&paste-inserting of text,
but also for using character vectors, e.g. the output of \code{neighbors()}.\cr\cr
To import a document \emph{Document.txt} to from a directory for comparisons, set your working
directory to this directory using \code{setwd()}. Then use the following command lines:\cr\cr
\code{fileName1 <- "Alice_in_Wonderland.txt"}\cr
\code{x <- readChar(fileName1, file.info(fileName1)$size)}
}

\usage{costring(x,y,tvectors=tvectors,breakdown=TRUE)}

\arguments{
  \item{x}{a character vector}
  \item{y}{a character vector}
  \item{tvectors}{the semantic space in which the computation is to be done (a numeric matrix where every row is a word vector)}
  \item{breakdown}{if \code{TRUE}, the function \code{\link[LSAfun]{breakdown}} is applied to the input}


 
}
\value{A numeric giving the cosine between the input sentences/documents}

\author{
  Fritz Günther
}
\seealso{
  \code{\link[lsa]{cosine}},
  \code{\link[LSAfun]{Cosine}},
  \code{\link[LSAfun]{multicos}},
  \code{\link[LSAfun]{multicostring}}
}

\references{Landauer, T.K., & Dumais, S.T. (1997). A solution to Plato's problem: The Latent Semantic Analysis theory of acquisition, induction and representation of knowledge. \emph{Psychological Review, 104,} 211-240.\cr\cr

Dennis, S. (2007). How to use the LSA Web Site. In T. K. Landauer, D. S. McNamara, S. Dennis, & W. Kintsch (Eds.), \emph{Handbook of Latent
Semantic Analysis} (pp. 35-56). Mahwah, NJ: Erlbaum.\cr\cr

\url{http://lsa.colorado.edu/}
}

\examples{data(wonderland)
costring("Alice was beginning to get very tired.",
      "A white rabbit with a clock ran close to her.",
      tvectors=wonderland)}