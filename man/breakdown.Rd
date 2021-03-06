\name{breakdown}
\alias{breakdown}
\title{Clean up special characters}
\encoding{latin1}

\description{Replaces special characters in character vectors}

\details{
Applies the following functions to a character vector

\itemize{
\item{sets all letters to lower case}
\item{replaces umlauts (for example � replaced by ae)}
\item{removes accents from letters (for example � replaced by e)}
\item{replaces � by ss}

Also removes other special characters, like punctuation signs, numbers and breaks
}
}

\usage{breakdown(x)}

\arguments{
  \item{x}{a character vector}  
}

\value{A character vector}

\author{
  Fritz G�nther
}
\seealso{
  \code{\link[base]{gsub}}
}


            
\examples{breakdown("M�rchen")

breakdown("I was visiting Orl�ans last week. 
           It was nice, though!")}