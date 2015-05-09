\name{plausibility}
\alias{plausibility}
\title{Compute word (or compound) plausibility}
\encoding{latin1}

\description{Gives measures of semantic transparency (plausibility) for words or compounds}

\details{
The format of \code{x} should be of the kind \code{x <- "word1 word2 word3"} instead of\cr\code{x <- c("word1", "word2", "word3")} if phrases of more than one word are used as input. Simple vector addition of the constituent vectors is then used to compute the phrase vector.\cr
Since \code{x} can also be chosen to be any vector of the active LSA Space, this function can be combined with \code{compose()} to compute semantic transparency measures of complex expressions (see examples). Since semantic transparency methods were developed as measures for composed vectors, applying them makes most sense for those.\cr\cr
The methods are defined as follows:
\itemize{

  \item{\code{method = "n_density"} The average cosine between a (word or phrase) vector and its \emph{n} nearest neighbors (see \code{\link[LSAfun]{neighbors}})}
  \item{\code{method = "length"} The length of a vector (as computed by the standard Euclidean norm)}
  \item{\code{method = "proximity"} The cosine similarity between a compound vector and its stem word (for example between \emph{mad hatter} and \emph{hatter} or between \emph{objectify} and \emph{object})}
  \item{\code{method = "entropy"} The entropy of the \emph{K}-dimensional vector with the vector components \eqn{t_1,...,t_K }, as computed by 
                        \deqn{entropy = \log{K} - \sum{t_i * \log{t_i}}    }}
  
  
}
}

\usage{plausibility(x,method, n=10,stem,tvectors=tvectors,breakdown=TRUE)}

\arguments{
  \item{x}{a character vector of \code{length(x) = 1} or a numeric of \code{length=ncol(tvectors)} vector with same dimensionality as LSA space}
  \item{method}{the measure of semantic transparency, can be one of \code{n_density},\code{length}, \code{proximity}, or \code{entropy} (see \emph{Details})}
  \item{n}{the number of neighbors for the \code{n_density} method}
  \item{stem}{the stem (or word) of comparison for the \code{proximity} method}
  \item{tvectors}{the semantic space in which the computation is to be done (a numeric matrix where every row is a word vector)}
  \item{breakdown}{if \code{TRUE}, the function \code{\link[LSAfun]{breakdown}} is applied to the input}
 
}
\value{The semantic transparency as a numeric}

\author{
  Fritz Günther
}
\seealso{
  \code{\link[LSAfun]{Cosine}},
  \code{\link[LSAfun]{neighbors}},
  \code{\link[LSAfun]{compose}}
  
}

\references{
Lazaridou, A., Vecchi, E., & Baroni, M. (2013). Fish transporters and miracle homes:
How compositional distributional semantics can help NP parsing. In \emph{Proceedings
of EMNLP 2013} (pp. 1908 - 1913). Seattle, WA.

Marelli, M., & Baroni, M. (in press). Affixation in semantic space: Modeling morpheme meanings with compositional distributional semantics. \emph{Psychological Review.}

Vecchi, E. M., Baroni, M., & Zamparelli, R. (2011). (Linear) maps of the impossible:
Capturing semantic anomalies in distributional space. In \emph{Proceedings of the
ACL Workshop on Distributional Semantics and Compositionality} (pp. 1-9).
Portland, OR.

}

\examples{data(wonderland)

plausibility("cheshire cat",method="n_density",n=10,tvectors=wonderland) 

plausibility(compose("mad","hatter",method="Multiply",tvectors=wonderland),
method="proximity",stem="hatter",tvectors=wonderland)}