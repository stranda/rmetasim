#Create genind object from landscape
#ignores the haploid loci
landscape.make.genind <- function(Rland) {
  tab <- landscape.ind.freq(Rland) * 2
  dimnames(tab) <- list(
    rownames = 1:dim(tab)[1],
    colnames = landscape.freq.locnames(Rland)
  )
  gi <- adegenet::genind(
    tab,
    pop = as.factor(landscape.populations(Rland)),
    ploidy = 2
  )
  gi[, loc = which(landscape.ploidy(Rland) > 1)]
}
