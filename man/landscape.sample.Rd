\name{landscape.sample}
\alias{landscape.sample}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{simulates sampling for genetics on the landscape}
\description{
Randomly pulls a max of \code{ns} individuals from a max of \code{np}
populations and returns a landscape object that could be used for
further simulation, but is usually used for analyses and summary
statistics calculatiuons.  If one needs a sample of specific
populations/habitats, then these should be given in the vector \code{pvec}
}
\usage{
landscape.sample(Rland, np = NULL, ns=NULL, pvec=NULL,svec = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Rland}{ landscape object }
  \item{ns}{Maximum individuals per selected population: a non-negative integer,
or a vector in the order of the distinct populations in \code{pvec}.
Stage sampling accepts a single size applied to each selected stage.}
  \item{np}{number of populations to sample}
  \item{pvec}{a vector of populations to sample}
  \item{svec}{A vector of the stages sampled from a landscape. This
    provides good control over which populations and which stages within
  populations can be sampled.}
}
\value{
landscape object
}
\details{
This function is intended to provide a lot of flexibility in sampling
individuals from a rmetasim landscape. Sampling is without replacement.
Groups smaller than their requested size contribute all available individuals.
Singleton and empty selections retain the individuals matrix dimensions.
An empty sample is not a populated landscape suitable for further simulation.
Incompatible sample-size vector lengths and simultaneous stage/population
requests are rejected.
}
\examples{
	l <- landscape.new.example()
	l <- landscape.simulate(l,1)
	l.samp <- landscape.sample(l,np=1,ns=24)
	landscape.amova.pairwise(l.samp)
        l.samp2 <- landscape.sample(l,ns=24,pvec=c(2))
	landscape.amova.pairwise(l.samp2) 
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ misc }% __ONLY ONE__ keyword per line
