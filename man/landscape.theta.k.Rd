\name{landscape.theta.k}
\alias{landscape.theta.k}
\title{Calculate theta using the number of alleles}
\description{
  Calculate theta using number of alleles from a landscape.}
\usage{
 landscape.theta.k(rland)
}
\arguments{
  \item{rland}{the Rmetasim landscape object}
}
\details{
Uses \code{\link[pegas]{theta.k}} with the sampled allele copies.
Singleton populations retain matrix dimensions. Empty populations,
samples with fewer than two copies, and monomorphic samples return
\code{NA}; monomorphic behaviour is unchanged from earlier versions.
When every sampled copy has a distinct allele, no finite theta estimate
exists; this case also returns \code{NA} instead of a root-search error.
}
\value{
A matrix with num loci columns and num populations rows.  Each element
reflects the estimated theta for that population x locus combination  }
\seealso{
landscape.theta.h, landscape.theta.s
}
\examples{
  exampleland <- landscape.new.example()
  exampleland <- landscape.simulate(exampleland, 4)
  theta.k.mat <- landscape.theta.k(exampleland)
  theta.k.mat
}
\keyword{misc}
