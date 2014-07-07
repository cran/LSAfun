\name{LSAfun-package}
\alias{LSAfun-package}
\title{Computations based on Latent Semantic Analysis}
\encoding{latin1}

\description{
LSAfun (short for Latent Semantic Analysis functions) is a package working with Latent Semantic Analysis.
Latent Semantic Analysis is an algorithm working on a corpus of text documents. It achieves a high-dimensional vector representation for word (and document) meanings. The exact algorithm is described in Martin & Berry (2007).\cr
Such a representation allows for the computation of word (and document) similarities, for example by computing cosine values of angles between two vectors.
}

\section{The focus of this package}{
This package is not designed to create LSA semantic spaces; this is the focus of the R-package \code{\link[lsa]{lsa}}. Instead, it provides functions to be applied on existing LSA semantic spaces, such as
\enumerate{
\item{Similarity Computations}
\item{neighborhood Computations}
\item{Applied Functions}
\item{Composition Methods}
}
}

\section{How to get an LSA space}{
\emph{LSAfun} comes with one example LSA space, the \link{wonderland} space.\cr\cr
This package can also directly use LSA semantic spaces created with the \code{\link[lsa]{lsa}}-package. Thus, it allows the user to use own LSA spaces. (Note that the function \code{\link[lsa]{lsa}} gives a list of three matrices. Of those, the term matrix \code{U} should be used.)\cr\cr
Another possibility is to use one of the LSA spaces provided at \url{http://www.lingexp.uni-tuebingen.de/z2/LSAspaces}. These are stored in the \code{.rda} format. To load one of these spaces into the \code{R} workspace, save them into a directory, set the working directory to that directory, and load the space using \code{load()}.
}

\author{
  Fritz Günther
}



