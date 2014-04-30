\name{normalize}
\alias{normalize}
\title{Normalize a vector}
\encoding{latin1}

\description{Normalizes a character vector to a unit vector}

\details{The (euclidean) norm of a vector \eqn{x} is defined as 

\deqn{||x|| =  \sqrt{\Sigma(x^2)}}

To normalize a vector to a unit vector \eqn{u} with \eqn{||u|| = 1}, the following equation is applied:

\deqn{x' = x/ ||x||}

}

\usage{normalize(x)}

\arguments{ 

\item{x}{a numeric or integer vector}
}


\value{The normalized vector as a numeric}

\author{
  Fritz Günther
}

  

\examples{normalize(1:2)


## check vector norms:

x <- 1:2

sqrt(sum(x^2))              ## vector norm
sqrt(sum(normalize(x)^2))   ## norm = 1

}