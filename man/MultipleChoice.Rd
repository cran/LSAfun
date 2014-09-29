\name{MultipleChoice}
\alias{MultipleChoice}
\title{Answers Multiple Choice Questions}
\encoding{latin1}

\description{Selects the nearest word to an input out of a set of options}

\details{
Computes all the cosines between a given sentence/document or word and multiple answer options. Then
selects the nearest option to the input (the option with the highest cosine).This function relies entirely on the \code{\link[LSAfun]{costring}} function.\cr
A warning message will be displayed if all words of one answer alternative are not found in the semantic space.

}

\usage{MultipleChoice(x,y,tvectors=tvectors,breakdown=TRUE)}

\arguments{
  \item{x}{a character vector of \code{length(x) = 1} specifying a sentence/ document (or also a single word)}
  \item{y}{a character vector specifying multiple answer options}
  \item{tvectors}{the semantic space in which the computation is to be done (a numeric matrix where every row is a word vector)}
  \item{breakdown}{if \code{TRUE}, the function \code{\link[LSAfun]{breakdown}} is applied to the input}
 
}
\value{The nearest option to \code{x} as a character}

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

LSAfun:::MultipleChoice("Who does the march hare celebrate his unbirthday with?",
                 c("Mad Hatter","Red Queen","Caterpillar","Cheshire Cat"),
                 tvectors=wonderland)}