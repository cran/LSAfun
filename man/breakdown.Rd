\name{breakdown}
\alias{breakdown}
\title{Clean up special characters}
\encoding{latin1}

\description{Replaces special characters in character vectors}

\details{
Applies the following functions to a character vector

\itemize{
\item{sets all letters to lower case}
\item{replaces umlauts (for example ä replaced by ae)}
\item{removes accents from letters (for example é replaced by e)}
\item{replaces ß by ss}

Also removes other special characters, like punctuation signs, numbers and breaks
}
}

\usage{breakdown(x)}

\arguments{
  \item{x}{a character vector}  
}

\value{A character vector}

\author{
  Fritz Günther
}
\seealso{
  \code{\link[base]{gsub}}
}


            
\examples{breakdown("Märchen")

breakdown("I was visiting Orléans last week. 
           It was nice, though!")}