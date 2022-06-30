\name{multidocs}
\alias{multidocs}
\title{Comparison of sentence sets}
\encoding{latin1}

\description{Computes cosine values between sets of sentences and/or documents}

\details{
In the traditional LSA approach, the vector \emph{D} for a document (or a sentence) consisting of the words \emph{(t1, . , tn)} is computed as
             \deqn{D = \sum\limits_{i=1}^n t_n}
             
This function computes the cosines between two sets of documents (or sentences).\cr\cr
     The format of \code{x} (or \code{y}) should be of the kind \code{x <- c("this is the first text","here is another text")}
}

\usage{multidocs(x,y=x,chars=10,tvectors=tvectors,remove.punctuation=TRUE)}

\arguments{
  \item{x}{a character vector containing different sentences/documents}
  \item{y}{a character vector containing different sentences/documents (\code{y = x} by default)}
  \item{chars}{an integer specifying how many letters (starting from the first) of each sentence/document are to be printed in the row.names and col.names of the output matrix}
  \item{tvectors}{the semantic space in which the computation is to be done (a numeric matrix where every row is a word vector)}
    \item{remove.punctuation}{removes punctuation from \code{x} and \code{y}; \code{TRUE} by default}



 
}
\value{A list of three elements:
\item{\code{cosmat}}{A numeric matrix giving the cosines between the input sentences/documents}
\item{\code{xdocs}}{A legend for the row.names of \code{cosmat}}
\item{\code{ydocs}}{A legend for the col.names of \code{cosmat}}
}

\author{
  Fritz Guenther
}
\seealso{
  \code{\link[lsa]{cosine}},
  \code{\link[LSAfun]{Cosine}},
  \code{\link[LSAfun]{multicos}},
  \code{\link[LSAfun]{costring}}
}

\references{Landauer, T.K., & Dumais, S.T. (1997). A solution to Plato's problem: The Latent Semantic Analysis theory of acquisition, induction and representation of knowledge. \emph{Psychological Review, 104,} 211-240.\cr\cr

Dennis, S. (2007). How to use the LSA Web Site. In T. K. Landauer, D. S. McNamara, S. Dennis, & W. Kintsch (Eds.), \emph{Handbook of Latent
Semantic Analysis} (pp. 35-56). Mahwah, NJ: Erlbaum.\cr\cr

\url{http://lsa.colorado.edu/}
}

\examples{data(wonderland)
multidocs(x = c("alice was beginning to get very tired.",
                "the red queen greeted alice."),
          y = c("the mad hatter and the mare hare are having a party.",
                "the hatter sliced the cup of tea in half."), 
      tvectors=wonderland)}