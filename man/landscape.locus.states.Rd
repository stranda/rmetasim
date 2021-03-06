\name{landscape.locus.states}
\alias{landscape.locus.states}

\title{return a matrix containing actual allelic states and their indices}

\description{Convenience function to return a matrix containing the states of the alleles and their indicesfor a particular locus
}
\usage{
  landscape.locus.states(Rland,lnum=1,do.check=T)
}
\arguments{
  \item{lnum}{the locus to return}
  \item{Rland}{the Rmetasim landscape object}
  \item{do.check}{flag to call 'is.landscape' on input}
}


\value{
  matrix
}


\seealso{
  landscape.locus, landscape.states
  }


\keyword{misc}
