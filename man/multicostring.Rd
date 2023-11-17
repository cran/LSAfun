\name{multicostring}
\alias{multicostring}
\title{Sentence x Vector Comparison}
\encoding{latin1}

\description{Computes cosines between a sentence/ document and multiple words}

\details{
The format of \code{x} (or \code{y}) can be of the kind \code{x <- "word1 word2 word3"} , but also of the kind \code{x <- c("word1", "word2", "word3")}. This allows for simple copy&paste-inserting of text, but also for using character vectors, e.g. the output of \code{\link[LSAfun]{neighbors}}.


Both x and y can also just consist of one single word.
In the traditional LSA approach, the vector \emph{D} for the document (or sentence) \code{x} consisting of the words \emph{(t1, . , tn)} is computed as
             \deqn{D = \sum\limits_{i=1}^n t_n}
This is the default method (\code{method="Add"}) for this function. Alternatively, this function provided the possibility of computing the document vector from its word vectors using element-wise multiplication (see Mitchell & Lapata, 2010 and \code{\link[LSAfun]{compose}}). See also \code{\link[LSAfun]{costring}}).



A note will be displayed whenever not all words of one input string are found in the semantic space. \emph{\bold{Caution:}} In that case, the function will still produce a result, by omitting the words not found in the semantic space. Depending on the specific requirements of a task, this may compromise the results. Please check your input when you receive this message.



A warning message will be displayed whenever no word of one  input string is found in the semantic space.

}

\usage{multicostring(x,y,tvectors=tvectors,split=" ",remove.punctuation=TRUE, 
stopwords = NULL, method ="Add")}

\arguments{
  \item{x}{a character vector specifying a sentence/ document (or also a single word)}
  \item{y}{a character vector specifying multiple single words}
  \item{tvectors}{the semantic space in which the computation is to be done (a numeric matrix where every row is a word vector)}
     \item{split}{a character vector defining the character used to split the documents into words (white space by default)}
  \item{remove.punctuation}{removes punctuation from \code{x} and \code{y}; \code{TRUE} by default}
 \item{stopwords}{a character vector defining a list of words that are \emph{not} used to compute the document/sentence vector for \code{x}}
 \item{method}{the compositional model to compute the document vector from its word vectors. The default option \code{method = "Add"} computes the document vector as the vector sum. With \code{method = "Multiply"}, the document vector is computed via element-wise multiplication (see \code{\link[LSAfun]{compose}}).}

 
}
\value{A numeric giving the cosine between the input sentences/documents}

\author{
  Fritz Guenther
}
\seealso{
  \code{\link[lsa]{cosine}},
  \code{\link[LSAfun]{Cosine}},
  \code{\link[LSAfun]{multicos}},
  \code{\link[LSAfun]{costring}}
}

\references{Landauer, T.K., & Dumais, S.T. (1997). A solution to Plato's problem: The Latent Semantic Analysis theory of acquisition, induction and representation of knowledge. \emph{Psychological Review, 104,} 211-240.


Dennis, S. (2007). How to use the LSA Web Site. In T. K. Landauer, D. S. McNamara, S. Dennis, & W. Kintsch (Eds.), \emph{Handbook of Latent Semantic Analysis} (pp. 35-56). Mahwah, NJ: Erlbaum.


Mitchell, J., & Lapata, M. (2010). Composition in Distributional Models of Semantics. 
\emph{Cognitive Science, 34,} 1388-1429.


\url{http://wordvec.colorado.edu/}
}

\examples{data(wonderland)

multicostring("alice was beginning to get very tired.",
        "a white rabbit with a clock ran close to her.",
        tvectors=wonderland)

multicostring("suddenly, a cat appeared in the woods",
names(neighbors("cheshire",n=20,tvectors=wonderland)), 
tvectors=wonderland)}