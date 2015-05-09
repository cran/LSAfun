\name{LSAfun-package}
\alias{LSAfun-package}
\title{Computations based on Latent Semantic Analysis}
\encoding{latin1}

\description{
Offers methods and functions for working with Vector Space Models of semantics, such as Latent Semantic Analysis (LSA).
Such models are created by algorithms working on a corpus of text documents. Those algorithms achieve a high-dimensional vector representation for word (and document) meanings. The exact LSA algorithm is described in Martin & Berry (2007).\cr
Such a representation allows for the computation of word (and document) similarities, for example by computing cosine values of angles between two vectors.
}

\section{The focus of this package}{
This package is not designed to create LSA semantic spaces. In R, this functionality is provided by the package \code{\link[lsa]{lsa}}. The focus of the package \emph{LSAfun} is to provide functions to be applied on existing LSA (or other) semantic spaces, such as
\enumerate{
\item{Similarity Computations}
\item{Neighborhood Computations}
\item{Applied Functions}
\item{Composition Methods}
}
}



\section{How to obtain a semantic space}{

\emph{LSAfun} comes with one example LSA space, the \link{wonderland} space.\cr
This package can also directly use LSA semantic spaces created with the \code{\link[lsa]{lsa}}-package. Thus, it allows the user to use own LSA spaces. 
(Note that the function \code{\link[lsa]{lsa}} gives a list of three matrices. Of those, the term matrix \code{U} should be used.)\cr




The \code{\link[lsa]{lsa}} package works with (very) small corpora, but gets difficulties in scaling up to larger corpora. In this case, it is recommended to use specialized software for creating semantic spaces, such as
\cr

\itemize{

\item{S-Space (Jurgens & Stevens, 2010), available \href{https://github.com/fozziethebeat/S-Space}{here}}

\item{SemanticVectors (Widdows & Ferraro, 2008), available \href{https://code.google.com/p/semanticvectors/}{here}}

\item{gensim (Rehurek & Sojka, 2010), available \href{https://radimrehurek.com/gensim/}{here}}

\item{DISSECT (Dinu, Pham, & Baroni, 2013), available \href{http://clic.cimec.unitn.it/composes/toolkit/}{here}}
}

Another possibility is to use one of the LSA spaces provided at \url{http://www.lingexp.uni-tuebingen.de/z2/LSAspaces}. These are stored in the \code{.rda} format. To load one of these spaces into the \code{R} workspace, save them into a directory, set the working directory to that directory, and load the space using \code{load()}.

}


\author{
  Fritz Günther
}



