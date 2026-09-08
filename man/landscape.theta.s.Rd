\name{landscape.theta.s}
\alias{landscape.theta.s}
\title{Calculate Watterson's theta from segregating sites}
\description{Calculate Watterson's theta for sequence loci in each population.}
\usage{landscape.theta.s(rland)}
\arguments{
  \item{rland}{The rmetasim landscape object.}
}
\details{
Uses \code{\link[pegas]{theta.s}} with the number of segregating sites and
the number of sampled gene copies, not sequence length or the number of
unique haplotypes. A diploid individual contributes two sequence copies.
The estimate is per locus, not per nucleotide: it is the number of
segregating sites divided by the harmonic sum from 1 to \eqn{n-1}, where
\eqn{n} is the number of sampled copies.
Sequence loci must contain equal-length, non-empty A/C/G/T sequences.
Ambiguous bases and gaps are rejected rather than silently choosing a
missing-site policy. Monomorphic sequence loci return zero when at least
two copies are available. Non-sequence loci and populations with fewer than
two copies return \code{NA}.
}
\value{
A matrix with one column per locus and one row per population.
}
\references{
Watterson, G. A. (1975). On the number of segregating sites in genetical
models without recombination. Theoretical Population Biology, 7, 256--276.
Tajima, F. (1989). Statistical method for testing the neutral mutation
hypothesis by DNA polymorphism. Genetics, 123, 585--595.
}
\seealso{\code{\link{landscape.theta.k}}, \code{\link{landscape.theta.h}}}
\examples{
  exampleland <- landscape.new.example()
  theta.s.mat <- landscape.theta.s(exampleland)
  theta.s.mat
}
\keyword{misc}
