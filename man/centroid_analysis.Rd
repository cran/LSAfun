\name{centroid_analysis}
\alias{centroid_analysis}
\title{Centroid Analysis}
\encoding{latin1}

\description{Performs a centroid analysis for a set of words}

\details{The centroid analysis computes the average vector for a set of words. The intended use case is that these words are responses towards a given concept; the centroid then serves as the estimated vector representation for that concept.}

\usage{centroid_analysis(responses,targets = NULL,split=" ",unique.responses = FALSE,
reference.list = NULL,verbose = FALSE,rank.responses = FALSE,
tvectors=tvectors)}

\arguments{
  \item{responses}{a character vector specifying multiple single words}
  \item{targets}{(optional:) a character vector specifying one or multiple single words}
  \item{split}{a character vector defining the character used to split the input strings into individual words (white space by default)}
  \item{unique.responses}{If \code{TRUE}, duplicated words in \code{responses} are discarded when computing the the centroid. \code{FALSE} by default, so multiple instances of the same word will be included.}
    \item{reference.list}{(optional:) A list of words in reference to which the neighborhood ranks are computed: Only entries in \code{reference.list} will be considered as possible neighbors. Only relevant when target words are provided in \code{target}. if \code{reference.list = NULL} (default), then \code{rownames(tvectors)} (all words in the semantic space) will be considered when computing ranks.}
    \item{verbose}{If \code{TRUE} (default: FALSE), a message will appear that specifies for which target the neighborhood ranks are currently being computed}
     \item{rank.responses}{If \code{FALSE} (default), responses themselves will not be considered for computing the neighborhood rank. }
  \item{tvectors}{the semantic space in which the computation is to be done (a numeric matrix where every row is a word vector)}



}


\value{An object of class \code{centroid_analysis}. This object is a list consisting of:


\item{\code{$centroid}}{The centroid of the response vectors}
\item{\code{$cosines}}{The cosine similarity between the response centroid and each target vector}
\item{\code{$ranks.target}}{The rank of the response centroid \emph{in the neighborhood of each target vector}, with reference to \code{reference.list}} 
\item{\code{$ranks.centroid}}{The rank of each target \emph{in the neighborhood of the response centroid}, with reference to \code{reference.list}} 

}

\author{
  Fritz Guenther, Aliona Petrenco
}
\seealso{
  \code{\link[lsa]{cosine}},
  \code{\link[LSAfun]{Cosine}},
  \code{\link[LSAfun]{neighbors}}
}

\references{Pugacheva, V., & Günther, F. (2024). Lexical choice and word formation in a taboo game paradigm. \emph{Journal of Memory and Language, 135,} 104477.

}


\examples{
data(wonderland)
centroid_analysis(responses=c("mouse","rabbit","cat","king","queen"),targets=c("alice","hare"),
          tvectors=wonderland)
}