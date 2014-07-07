\name{multicostring}
\alias{multicostring}
\title{Sentence x Vector Comparison}
\encoding{latin1}

\description{Computes cosines between a sentence/ document and multiple words}

\details{
The format of \code{x} (or \code{y}) can be of the kind \code{x <- "word1 word2 word3"} , but also of the kind \code{x <- c("word1", "word2", "word3")}. This allows for simple copy&paste-inserting of text, but also for using character vectors, e.g. the output of \code{\link[LSAfun]{neighbors}}.\cr
Both x and y can also just consist of one single word.
For computing the vector for the document/ sentence specified in \code{x}, the simple Addition model is used (see \code{\link[LSAfun]{costring}}).
}

\usage{multicostring(x,y,tvectors=tvectors,breakdown=TRUE)}

\arguments{
  \item{x}{a character vector specifying a sentence/ document (or also a single word)}
  \item{y}{a character vector specifying multiple single words}
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

multicostring("Alice was beginning to get very tired.",
        "A white rabbit with a clock ran close to her.",
        tvectors=wonderland)

multicostring("Suddenly, a cat appeared in the woods",
names(neighbors("cheshire",n=20,tvectors=wonderland)), 
tvectors=wonderland)}