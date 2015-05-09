\name{conSIM}
\alias{conSIM}
\title{Similarity in Context}
\encoding{latin1}

\description{Compute Similarity of a word with a set of two other test words, given a third context word}

\details{Following the example from Kintsch (2014): If one has to judge the similarity between \emph{France} one the one hand and the test words \emph{Germany} and \emph{Spain} on the other hand, this similarity judgement varies as a function of a fourth context word. If \emph{Portugal} is given as a context word, \emph{France} is considered to be more similar to \emph{Germany} than to \emph{Spain}, and vice versa for the context word \emph{Poland}. Kintsch (2014) proposed a context sensitive, asymmetrical similarity measure for cases like this, which is implemented here
}

\usage{conSIM(x,y,z,c,tvectors=tvectors,breakdown=TRUE)}

\arguments{
  \item{x}{The relevant word, given as a character of \code{length(x) = 1}}
  \item{y, z}{The two test words, given each as a character of \code{length(y) = 1}}
  \item{c}{The context word in respect to which the similarity of \code{x} to \code{y} and \code{z} is to be computed (a character of \code{length(y) = 1}) }
  \item{tvectors}{the semantic space in which the computation is to be done (a numeric matrix where every row is a word vector)}
  \item{breakdown}{if \code{TRUE}, the function \code{\link[LSAfun]{breakdown}} is applied to the input}

}
\value{A list of two similarity values:\cr
\code{SIM_XY_zc}: Similarity of \code{x} and \code{y}, given the alternative \code{z} and the context \code{c}\cr
\code{SIM_XZ_yc}: Similarity of \code{x} and \code{z}, given the alternative \code{y} and the context \code{c}}

\author{
  Fritz Günther
}
\seealso{
  \code{\link[LSAfun]{Cosine}}
  \code{\link[LSAfun]{asym}}
  
}

\references{Kintsch, W. (2015). Similarity as a Function of Semantic Distance and Amount of Knowledge. \emph{Psychological Review, 121,} 559-561.\cr

Tversky, A. (1977). Features of similarity. \emph{Psychological Review, 84,} 327-352.
}


\examples{data(wonderland)

conSIM(x="rabbit",y="alice",z="hatter",c="dormouse",tvectors=wonderland)}