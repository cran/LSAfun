\name{wonderland}
\docType{data}
\alias{wonderland}
\title{LSA Space: Alice's Adventures in Wonderland}
\description{
  This data set is a 50-dimensional LSA space derived from Lewis Carrol's book "Alice's Adventures in Wonderland". The book was split into 791 paragraphs which served as documents for the LSA algorithm (Landauer, Foltz & Laham, 1998). Only words that appeared in at least two documents were used for building the LSA space.\cr
  This LSA space contains 1123 different terms, all in lower case letters, and was created using the \code{\link[lsa]{lsa}}-package. It can be used as \code{tvectors} for all the functions in the \code{LSAfun}-package.
}
\usage{data(wonderland)}
\format{A 1123x50 matrix with terms as rownames.}
\source{\href{http://www.gutenberg.org/cache/epub/11/pg11.txt}{Alice in Wonderland from Project Gutenberg}}
\references{
  Landauer, T., Foltz, P., and Laham, D. (1998) \emph{Introduction to Latent Semantic Analysis}. In: Discourse Processes 25, pp. 259-284.
  
  Carroll, L. (1865). \emph{Alice's Adventures in Wonderland}. New York: MacMillan.

}
\keyword{LSA space}