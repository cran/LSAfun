\name{coherence}
\alias{coherence}
\title{Coherence of a text}
\encoding{latin1}

\description{Computes coherence of a given paragraph/document}

\details{
This function applies the method described in Landauer & Dumais (1997): The \emph{local coherence} is the cosine
between two adjacent sentences. The \emph{global coherence} is then computed as the mean value of these local
coherences.


The format of \code{x} should be of the kind \code{x <- "sentence1. sentence2. sentence3"} Every sentence can also just consist of one single word.


To import a document Document.txt to from a directory for coherence computation, set your working
directory to this directory using \code{setwd()}. Then use the following command lines:


\code{fileName1 <- "Alice_in_Wonderland.txt"}


\code{x <- readChar(fileName1, file.info(fileName1)$size)}

In the traditional LSA approach, the vector \emph{D} for a document (or a sentence) consisting of the words \emph{(t1, . , tn)} is computed as
             \deqn{D = \sum\limits_{i=1}^n t_n}
This is the default method (\code{method="Add"}) for this function. Alternatively, this function provided the possibility of computing the document vector from its word vectors using element-wise multiplication (see Mitchell & Lapata, 2010 and \code{\link[LSAfun]{compose}}).



A note will be displayed whenever not all words of one input string are found in the semantic space. \emph{\bold{Caution:}} In that case, the function will still produce a result, by omitting the words not found in the semantic space. Depending on the specific requirements of a task, this may compromise the results. Please check your input when you receive this message. 


A warning message will be displayed whenever no word of one  input string is found in the semantic space.

}

\usage{coherence(x,split=c(".","!","?"),tvectors=tvectors, remove.punctuation=TRUE, 
stopwords = NULL, method ="Add")}

\arguments{
  \item{x}{a character vector of \code{length(x) = 1} containing the document}
  \item{split}{a vector of expressions that determine where to split sentences}
  \item{tvectors}{the semantic space in which the computation is to be done (a numeric matrix where every row is a word vector)}
    \item{remove.punctuation}{removes punctuation from \code{x} \emph{after} splitting the sentences; \code{TRUE} by default}
 \item{stopwords}{a character vector defining a list of words that are \emph{not} used to compute the  sentence vectors for \code{x}}
 \item{method}{the compositional model to compute the document vector from its word vectors. The default option \code{method = "Add"} computes the document vector as the vector sum. With \code{method = "Multiply"}, the document vector is computed via element-wise multiplication (see \code{\link[LSAfun]{compose}}).}



 
}
\value{A list of two elements; the first element (\code{$local}) contains the local coherences as a numeric vector, the second element (\code{$global}) contains the global coherence as a numeric.}

\author{
  Fritz Guenther
}
\seealso{
  \code{\link[lsa]{cosine}},
  \code{\link[LSAfun]{Cosine}},
  \code{\link[LSAfun]{costring}}

}

\references{Landauer, T.K., & Dumais, S.T. (1997). A solution to Plato's problem: The Latent Semantic Analysis theory of acquisition, induction and representation of knowledge. \emph{Psychological Review, 104,} 211-240.


Mitchell, J., & Lapata, M. (2010). Composition in Distributional Models of Semantics. 
\emph{Cognitive Science, 34,} 1388-1429.


}

\examples{data(wonderland)

coherence ("there was certainly too much of it in the air. even the duchess
sneezed occasionally; and as for the baby, it was sneezing and howling
alternately without a moment's pause. the only things in the kitchen
that did not sneeze, were the cook, and a large cat which was sitting on
the hearth and grinning from ear to ear.",
tvectors=wonderland)}