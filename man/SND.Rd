\name{SND}
\alias{SND}
\title{Semantic neighborhood density}
\encoding{latin1}

\description{Returns semantic neighborhood with semantic neighborhood size and density}

\details{There are two principle approaches to determine the semantic neighborhood of a target word:

\itemize{
\item Set an a priori size of the semantic neighborhood to a fixed value \code{n} (e.g., Marelli & Baroni, 2015). The \code{n} closest words to the target word are counted as its semantic neighbors. The semantic neighborhood size is then necessarily \code{n}; the semantic neighborhood density is the mean similarity between these neighbors and the target word (see also \code{\link[LSAfun]{plausibility}})

\item Determine the semantic neighborhood based on a similarity threshold; all words whose similarity to the target word exceeds this threshold are counted as its semantic neighbors (e.g., Buchanan, Westbury, & Burgess, 2001). First, the similarity between the target word and all words in the semantic space is computed. These similarities are then transformed into \emph{z}-scores. Traditionally, the threshold is set to \emph{z = 3.5} (e.g., Buchanan, Westbury, & Burgess, 2001).
}

If a single target word is used as \code{x}, this  target word itself (which always has a similarity of 1 to itself) is excluded from these computations so that it cannot be counted as its own neighbor
}

\usage{SND(x,n=NA,threshold=3.5,tvectors=tvectors)}

\arguments{
  \item{x}{a character vector of \code{length(x) = 1} or a numeric of \code{length=ncol(tvectors)} vector with same dimensionality as the semantic space}
  \item{n}{if specified as a numeric, determines the size of the neighborhood as the \code{n} nearest words to \code{x}. If \code{n=NA} (default), the semantic neighborhood will be determined according to a similarity threshold (see \code{threshold})}
    \item{threshold}{specifies the similarity threshold that determines if a word is counted as a neighbor for \code{x}, following the method by Buchanan et al. (2011) (see \code{Description} below)}
  \item{tvectors}{the semantic space in which the computation is to be done (a numeric matrix where every row is a word vector)}

 
}
\value{A list of three elements:

\itemize{
\item{neighbors:} A names numeric vector of all identified neighbors, with the names being these neighbors and the values their similarity to \code{x}
\item{n_size:} The number of neighbors as a numeric
\item{SND:} The semantic neighborhood density (SND) as a numeric
}

}

\author{
  Fritz Guenther
}
\seealso{
  \code{\link[lsa]{cosine}},
  \code{\link[LSAfun]{plot_neighbors}},
  \code{\link[LSAfun]{compose}}
  
}

\references{
Buchanan, L., Westbury, C., & Burgess, C. (2001). Characterizing semantic space: Neighborhood effects in word recognition. \emph{Psychonomic Bulletin & Review, 8,} 531-544.

Marelli, M., & Baroni, M. (2015). Affixation in semantic space: Modeling morpheme meanings with compositional distributional semantics. \emph{Psychological Review, 122,} 485-515.


}

\examples{data(wonderland)

SND("cheshire",n=20,tvectors=wonderland)

SND("alice",threshold=2,tvectors=wonderland)
}