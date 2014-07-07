\name{compose}
\alias{compose}
\title{Two-Word Composition}
\encoding{latin1}

\description{Computes the vector of a complex expression p consisting of two single words u and v,
following the methods examined in Mitchell & Lapata (2008) (see \emph{Details}).}

\details{
Let \eqn{p} be the vector with entries \eqn{p_i} for the two-word phrase consisiting of \eqn{u} with entries \eqn{u_i} and \eqn{v} with entries \eqn{v_i}.
The different composition methods as described by Mitchell & Lapata (2008) are as follows:
\itemize{
  \item Additive Model (\code{method = "Add"})    \deqn{p_i = u_i + v_i}
  \item Weighted Additive Model (\code{method = "WeightAdd"}) \deqn{p_i = a*u_i + b*v_i}
  \item Multiplicative Model (\code{method = "Multiply"}) \deqn{p_i = u_i * v_i}
  \item Combined Model (\code{method = "Combined"}) \deqn{p_i = a*u_i + b*v_i + c*u_i*v_i }
  \item Predication (\code{method = "Predication"}) 
  \cr(see \code{\link[LSAfun]{Predication}})}


If \code{method="Predication"} is used, \code{x} will be taken as Predicate and \code{y} will be taken as Argument of the phrase (see \emph{Examples})
}

\usage{
## Default 
compose(x,y,method="Add", a=1,b=1,c=1,m,k,tvectors=tvectors,
  breakdown=TRUE, norm="none")}

\arguments{
  \item{x}{a single word (character vector with \code{length(x) = 1)}}
  \item{y}{a single word (character vector with \code{length(y) = 1)}}
  \item{a,b,c}{weighting parameters, see \emph{Details}}
  \item{m}{number of nearest words to the Predicate that are initially activated (see \code{\link[LSAfun]{Predication}})}
  \item{k}{size of the \code{k}-neighborhood; \code{k} \eqn{\le} \code{m} (see \code{\link[LSAfun]{Predication}})}
  \item{method}{the composition method to be used (see \emph{Details})}
  \item{norm}{whether to \code{\link[LSAfun]{normalize}} the single word vectors before applying a composition function. Setting \code{norm = "none"} will not perform any normalizations, setting \code{norm = "all"} will normalize every involved word vector. Setting \code{norm = "block"} is only valid for the \code{\link[LSAfun]{Predication}} method}
  \item{tvectors}{the semantic space in which the computation is to be done (a numeric matrix where every row is a word vector)}
  \item{breakdown}{if \code{TRUE}, the function \code{\link[LSAfun]{breakdown}} is applied to the input}  
  }
 

\value{The phrase vector as numeric
}

\author{
  Fritz Günther
}

\seealso{
  \code{\link[LSAfun]{Predication}}
   
}

\references{
Kintsch, W. (2001). Predication. \emph{Cognitive science, 25,} 173-202.

Mitchell, J. & Lapata, M. (2008). Vector-based Models of Semantic
Composition. In \emph{Proceedings of ACL-08: HLT} (pp. 236-244).
Columbus, Ohio.
}
\examples{data(wonderland)

compose(x="mad",y="hatter",method="Add",tvectors=wonderland)

compose(x="mad",y="hatter",method="Combined",a=1,b=2,c=3,
tvectors=wonderland)

compose(x="mad",y="hatter",method="Predication",m=20,k=3,
tvectors=wonderland)}