\name{asym}
\alias{asym}
\title{Asymmetric Similarity functions}
\encoding{latin1}

\description{Compute various asymmetric similarities between words}

\details{Asymmetric (or directional) similarities can be useful e.g. for examining \emph{hypernymy} (category inclusion), for example the relation between \emph{dog} and \emph{animal} should be asymmetrical. The general idea is that, if one word is a hypernym of another (i.e. it is semantically narrower), then a significant number of dimensions that are salient in this word should also be salient in the semantically broader term (Lenci & Benotto, 2012).\cr\cr
In the formulas below, \eqn{w_x(f)} denotes the value of vector \eqn{x} on dimension \eqn{f}. Furthermore, \eqn{F_x} is the set of \emph{active} dimensions of vector \eqn{x}. A dimension \eqn{f} is considered active if
\eqn{w_x(f) > t}, with \eqn{t} being a pre-defined, free parameter.\cr
The options for \code{method} are defined as follows (see Kotlerman et al., 2010) (1)):\cr\cr
\itemize{

  \item{\code{method = "weedsprec"}} \deqn{weedsprec(u,v) = \frac{\sum\nolimits_{f \in F_u \cap F_v}w_u(f)}{\sum\nolimits_{f \in F_u}w_u(f)}}      
  

\item{\code{method = "cosweeds"}} \deqn{cosweeds(u,v) = \sqrt{weedsprec(u,v) \times cosine(u,v)}}    

  \item{\code{method = "clarkede"}} \deqn{clarkede(u,v) = \frac{\sum\nolimits_{f \in F_u \cap F_v}min(w_u(f),w_v(f))}{\sum\nolimits_{f \in F_u}w_u(f)}}     

  \item{\code{method = "invcl"}} \deqn{invcl(u,v) = \sqrt{clarkede(u,v)\times(1-clarkede(u,v)})}      

\item{\code{method = "kintsch"}}\cr\cr
Unlike the other  methods, this one is not derived from the logic of hypernymy, but rather from asymmetrical similarities between words due to different amounts of knowledge about them. Here, asymmteric similarities between two words are computed by taking into account the vector length (i.e. the amount of information about those words). This is done by projecting one vector onto the other, and normalizing this resulting vector by dividing its length by the length of the longer of the two vectors (Details in Kintsch, 2014, see References). 

}
}








\usage{asym(x,y,method,t=0,tvectors,breakdown=TRUE)}

\arguments{
  \item{x}{A single word, given as a character of \code{length(x) = 1}}
  \item{y}{A single word, given as a character of \code{length(y) = 1}}
  \item{method}{Specifying the formula to use for asymmetric similarity computation}
  \item{t}{A numeric threshold a dimension value of the vectors has to exceed so that the dimension is considered \emph{active}; not needed for the \code{kintsch} method}
  \item{tvectors}{the semantic space in which the computation is to be done (a numeric matrix where every row is a word vector)}
  \item{breakdown}{if \code{TRUE}, the function \code{\link[LSAfun]{breakdown}} is applied to the input}

}
\value{A numeric giving the asymmetric similarity between \code{x} and \code{y}}

\author{
  Fritz Günther
}
\seealso{
  \code{\link[LSAfun]{Cosine}}
  \code{\link[LSAfun]{conSIM}}
  
}

\references{
Kintsch, W. (2015). Similarity as a Function of Semantic Distance and Amount of Knowledge. \emph{Psychological Review, 121,} 559-561.

Kotlerman, L., Dagan, I., Szpektor, I., & Zhitomirsky-Geffet, M (2010). Directional distributional
similarity for lexical inference. \emph{Natural Language Engineering, 16,} 359-389.

Lenci, A., & Benotto, G. (2012). Identifying hypernyms in distributional semantic spaces. In \emph{Proceedings of *SEM} (pp. 75-79), Montreal, Canada.

}


\examples{data(wonderland)

asym("alice","girl",method="cosweeds",t=0,tvectors=wonderland)
asym("alice","rabbit",method="cosweeds",tvectors=wonderland)}