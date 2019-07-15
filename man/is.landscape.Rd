\name{is.landscape}
\alias{is.landscape}
\title{Test whether an object is a (fairly) legitimate landscape}

\description{
  Test whether a genuine landscape}
\usage{
  is.landscape(Rland = NULL, verb = TRUE, exact = FALSE, do.check = TRUE)
}
\arguments{
  \item{Rland}{the Rmetasim landscape object}
  \item{verb}{print why not a landscape}
  \item{exact}{more strict}
  \item{do.check}{actually do the check (function returns TRUE if set to FALSE)}
}

\examples{
  exampleland <- landscape.new.example()
  is.landscape(exampleland)
  rm(exampleland)
}
\keyword{misc}
