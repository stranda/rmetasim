\name{landscape.theta.h}
\alias{landscape.theta.h}
\title{Calculate theta using heterozygosity}
\description{Calculate theta from a landscape based upon
  heterozygosity.}
\usage{
  landscape.theta.h(rland)
}
\arguments{
  \item{rland}{the Rmetasim landscape object}
}
\details{
Uses \code{\link[pegas]{theta.h}} with the sampled allele copies.
Singleton populations retain matrix dimensions. Empty populations,
samples with fewer than two copies, and monomorphic samples return
\code{NA}; monomorphic behaviour is unchanged from earlier versions.
}
  
\value{
A matrix with num loci columns and num populations rows.  Each element
reflects the estimated theta for that population x locus combination  }
\seealso{
landscape.theta.k, landscape.theta.s
}
\examples{
  exampleland <- landscape.new.example()
  exampleland <- landscape.simulate(exampleland, 4)
  theta.h.mat <- landscape.theta.h(exampleland)
  theta.h.mat
}
\keyword{misc}
