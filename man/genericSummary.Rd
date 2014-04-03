\name{genericSummary}
\alias{genericSummary}
\title{Summarize a text}
\encoding{latin1}

\description{Selects sentences from a text that best describe its topic}

\details{Applies the method of Gong & Liu (2001) for generic text summarization of text document \emph{D} via Latent Semantic Analysis:
\enumerate{
  \item{Decompose the document \emph{D} into individual sentences,
and use these sentences to form the candidate sentence
set \emph{S}, and set \emph{k} = 1.}
  \item{Construct the terms by sentences matrix \emph{A} for the
document \emph{D}.}
  \item{Perform the SVD on A to obtain the singular value
matrix \eqn{\Sigma}, and the right singular vector matrix \emph{\eqn{V^t}}. In
the singular vector space, each sentence i is represented
by the column vector \eqn{\psi _i = [v_i1, v_i2, ... , v_ir]^t} of \emph{\eqn{V^t}}.}
  \item{Select the \emph{k}'th right singular vector from matrix \emph{\eqn{V^t}}.}
  \item{Select the sentence which has the largest index value
with the \emph{k}'th right singular vector, and include it in
the summary.}
  \item{If \emph{k} reaches the predefined number, terminate the op-
eration; otherwise, increment \emph{k} by one, and go to Step
4.}
}
(Cited directly from Gong & Liu, 2001, \emph{p.} 21)
}


\usage{genericSummary(text,k,split=c(".","!","?"),min=5,breakdown=TRUE,...)}

\arguments{
  \item{text}{A character vector of \code{length(text) = 1} specifiying the text to be summarized}
  \item{k}{The number of sentences to be used in the summary}
  \item{split}{A character vector specifying which symbols determine the end of a sentence in the document}
  \item{min}{The minimum amount of words a sentence must have to be included in the computations}
  \item{breakdown}{If \code{TRUE}, the function \code{\link[LSAfun]{breakdown}} is applied to the input}
  \item{...}{Further arguments to be passed on to \code{\link[lsa]{textmatrix}}}

}
\value{A character vector of the length \emph{k}}

\author{
  Fritz Günther
}
\seealso{
  \code{\link[lsa]{textmatrix}},
  \code{\link[lsa]{lsa}},
  \code{\link[base]{svd}}
  
}

\examples{D <- "This is just a test document. It is set up just to throw some random 
sentences in this example. So do not expect it to make much sense. Probably, even 
the summary won't be very meaningful. But this is mainly due to the document not being
meaningful at all. For test purposes, I will also include a sentence in this 
example that is not at all related to the rest of the document. Lions are larger than cats."

genericSummary(D,k=1)}