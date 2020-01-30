
landscape.ind.freq <- function(Rland,include.states=TRUE) {
  l <- Rland
  ploidy <- landscape.ploidy(l)
  aml <- vector("list", length(ploidy))
  for (loc in 1:length(aml)) {
    genos <- landscape.locus(l, loc)[, -(1:landscape.democol())]
    loc.ploidy <- ploidy[loc]
    if (l$loci[[loc]]$type != 253) {
      lst <- landscape.locus.states(l, loc)
      names(lst$state) <- lst$aindex
      if (loc.ploidy == 2) {
        genos[, 1] <- unname(lst$state[as.character(genos[, 1])])
        genos[, 2] <- unname(lst$state[as.character(genos[, 2])])
      } else {
        genos <- unname(lst$state[as.character(genos)])
      }
    }
    unique.genos <- sort(as.character(unique(as.vector(genos))))
    aml[[loc]] <- sapply(unique.genos, function(x) {
      if (loc.ploidy == 2) {
        (as.character(genos[, 1]) == x) + (as.character(genos[, 2]) == x)
      } else {
        as.character(genos) == x
      }
    }) / loc.ploidy
  }
  do.call(cbind, aml)
}
