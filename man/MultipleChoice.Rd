\name{MultipleChoice}
\alias{MultipleChoice}
\title{Answers Multiple Choice Questions}
\encoding{latin1}

\description{Selects the nearest word to an input out of a set of options}

\details{
Computes all the cosines between a given sentence/document or word and multiple answer options. Then
selects the nearest option to the input (the option with the highest cosine). This function relies entirely on the \code{\link[LSAfun]{costring}} function.


A note will be displayed whenever not all words of one answer alternative are found in the semantic space. \emph{\bold{Caution:}} In that case, the function will still produce a result, by omitting the words not found in the semantic space. Depending on the specific requirements of a task, this may compromise the results. Please check your input when you receive this message.


A warning message will be displayed whenever no word of one answer alternative is found in the semantic space.

Using \code{method="Analogy"} requires the input in \emph{both} \code{x} and \code{y} to only consist of word pairs (for example \code{x = c("helmet head")} and  \code{y = c("kneecap knee", "atmosphere earth", "grass field")}). In that case, the function will try to identify the best-fitting answer in \code{y} by applying the \code{king - man + woman = queen} rationale to solve \emph{man : king = woman : ?} (Mikolov et al., 2013): In that case, one should also have \code{king - man = queen - woman}. With \code{method="Analogy"}, the function will compute the  difference between the normalized vectors \code{head - helmet}, and search the nearest of the vector differences \code{knee - kneecap}, \code{earth - atmosphere}, and \code{field - grass}.


}

\usage{MultipleChoice(x,y,tvectors=tvectors,remove.punctuation=TRUE, stopwords = NULL,
   method ="Add", all.results=FALSE)}

\arguments{
  \item{x}{a character vector of \code{length(x) = 1} specifying a sentence/ document (or also a single word)}
  \item{y}{a character vector specifying multiple answer options (with each element of the vector being one answer option)}
  \item{tvectors}{the semantic space in which the computation is to be done (a numeric matrix where every row is a word vector)}
    \item{remove.punctuation}{removes punctuation from \code{x} and \code{y}; \code{TRUE} by default}
 \item{stopwords}{a character vector defining a list of words that are \emph{not} used to compute the document/sentence vector for \code{x} and \code{y}}
 \item{method}{the compositional model to compute the document vector from its word vectors. The default option \code{method = "Add"} computes the document vector as the vector sum. With \code{method = "Multiply"}, the document vector is computed via element-wise multiplication (see \code{\link[LSAfun]{compose}} and \code{\link[LSAfun]{costring}}). With \code{method = "Analogy"}, the document vector is computed via vector subtraction; see \emph{Description} for more information.}
 \item{all.results}{If \code{all.results=FALSE} (default), the function will only return the best answer as a character string. If \code{all.results=TRUE}, it will return a named numeric vector, where the names are the different answer options in \code{y} and the numeric values their respective cosine similarity to \code{x}, sorted by decreasing similarity.}


}
\value{If \code{all.results=FALSE} (default), the function will only return the best answer as a character string. If \code{all.results=TRUE}, it will return a named numeric vector, where the names are the different answer options in \code{y} and the numeric values their respective cosine similarity to \code{x}, sorted by decreasing similarity.}

\author{
  Fritz Guenther
}
\seealso{
  \code{\link[lsa]{cosine}},
  \code{\link[LSAfun]{Cosine}},
  \code{\link[LSAfun]{costring}},
  \code{\link[LSAfun]{multicostring}},
  \code{\link[LSAfun]{analogy}}


}

\references{Landauer, T.K., & Dumais, S.T. (1997). A solution to Plato's problem: The Latent Semantic Analysis theory of acquisition, induction and representation of knowledge. \emph{Psychological Review, 104,} 211-240.


Mikolov, T., Yih, W. T., & Zweig, G. (2013). Linguistic regularities in continuous space word representations. In  \emph{Proceedings of the 2013 Conference of the North American Chapter of the Association for Computational Linguistics: Human Language Technologies (NAACL-HLT-2013)}. Association for Computational Linguistics.
}

\examples{data(wonderland)

LSAfun:::MultipleChoice("who does the march hare celebrate his unbirthday with?",
                 c("mad hatter","red queen","caterpillar","cheshire Cat"),
                 tvectors=wonderland)}