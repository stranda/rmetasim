\name{landscape.setallelefreq}
\alias{landscape.setallelefreq}
\title{Set Allele frequencies in (a) specific stage(s)}

\description{
  This function takes a landscape and changes the allele frequencies in
  different stages based on the list 'af'}
\usage{
## must be called AFTER landscape has been created and populated
landscape.setallelefreq(rland,af=NULL,states=TRUE)
}
\arguments{
  \item{rland}{landscape object, required}
  \item{af}{a named list of named lists that specifies states, loci
  and allele frequencies, see details}
  \item{states}{(default=TRUE) are the alleles specified by state or by
  allele index (allele indices are stored in the 'individuals'
  sub-object}
}

\details{
  The individual initilization function uses a global table of allele
  frequencies (specified with landscape.new.locus()).  This function
  re-writes the genotypes for specific demographic states and loci with new
  values.  This way it is possible to create demographic state-specifc allele
  frequency variation.

  First the landscape passed to landscape.setpopfreq() must be
  populated, usually by a call to landscape.new.individuals().  This
  function cannot be used to add new loci or new alleles to the global
  system.  

  This function depends on the data structure 'af' which is a
  hierarchical set of lists.  The highest level in the hierarchy is a
  named list of demographic state (the first column of the 'individuals'
  object).  Legal values for the names are the integers 0:(number of
  states-1) then converted to characters.  Each element of this list is
  itself a named list of loci.  Legal values for the names are the integers 1:number of
  loci then converted to characters.  Each element of the loci list is a
  named vector of allele frequencies.  The names for these vectors
  correspond to either: allele states (including DNA sequences) or
  allele indices (the numbers entered in the 'individuals' subobject as
  genotypes).  The parameter states=TRUE results in the former behavior
  and states=FALSE, the latter.
  }

\examples{
  exampleland <- landscape.new.example()
  af <- list('0'=list('1'=c('1'=0.5,'2'=0.25,'5'=0.25),
                      '2'=c('1'=0.5,'2'=0.25,'3'=0.25)),
             '2'=list('1'=c('1'=0.5,'2'=0.35,'5'=0.15),
                      '2'=c('1'=0.10,'2'=0.15,'3'=0.75)))

  landscape.allelefreq(exampleland)
  landscape.allelefreq(landscape.setallelefreq(exampleland, af))
}
\keyword{misc}
