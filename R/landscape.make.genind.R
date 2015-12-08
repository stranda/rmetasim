#Create genind object from landscape

landscape.make.genind <- function(Rland)
  {
      tab <- 2*landscape.ind.freq(Rland)
      dimnames(tab) <- list(rownames=1:dim(tab)[1],colnames=landscape.freq.locnames(Rland))
      populations <- landscape.populations(Rland)
      genind(tab,pop=as.factor(populations),ploidy=2)
  }
