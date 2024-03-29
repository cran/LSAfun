\name{LSAfun-package}
\alias{LSAfun-package}
\title{Computations based on Latent Semantic Analysis}
\encoding{latin1}

\description{
Offers methods and functions for working with Vector Space Models of semantics/distributional semantic models/word embeddings. The package was originally written for Latent Semantic Analysis (LSA), but can be used with all vector space models.
Such models are created by algorithms working on a corpus of text documents. Those algorithms achieve a high-dimensional vector representation for word (and document) meanings. The exact LSA algorithm is described in Martin & Berry (2007).


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


\section{Video Tutorials}{

A video tutorial for this package can be found here:
\url{https://youtu.be/IlwIZvM2kg8}

A video tutorial for using this package with vision-based representations from deep convolutional neural networks can be found here:
\url{https://youtu.be/0PNrXraWfzI}

}


\section{How to obtain a semantic space}{

\emph{LSAfun} comes with one example LSA space, the \link{wonderland} space.


This package can also directly use LSA semantic spaces created with the \code{\link[lsa]{lsa}}-package. Thus, it allows the user to use own LSA spaces. 
(Note that the function \code{\link[lsa]{lsa}} gives a list of three matrices. Of those, the term matrix \code{U} should be used.)





The \code{\link[lsa]{lsa}} package works with (very) small corpora, but gets difficulties in scaling up to larger corpora. In this case, it is recommended to use specialized software for creating semantic spaces, such as


\itemize{

\item{S-Space (Jurgens & Stevens, 2010), available \href{https://github.com/fozziethebeat/S-Space}{here}}

\item{SemanticVectors (Widdows & Ferraro, 2008), available \href{https://github.com/semanticvectors/semanticvectors}{here}}

\item{gensim (Rehurek & Sojka, 2010), available \href{https://radimrehurek.com/gensim/}{here}}

\item{DISSECT (Dinu, Pham, & Baroni, 2013), available \href{https://github.com/composes-toolkit/dissect}{here}}
}

\subsection{Downloading semantic spaces}{
Another possibility is to use one of the semantic spaces provided at \url{https://sites.google.com/site/fritzgntr/software-resources}. These are stored in the \code{.rda} format. To load one of these spaces into the \code{R} workspace, save them into a directory, set the working directory to that directory, and load the space using \code{load()}.
}
}


\author{
  Fritz Guenther
}



