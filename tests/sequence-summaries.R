library(rmetasim)
tajima <- getFromNamespace("landscape.tajima.d", "rmetasim")
set.seed(123)
g <- landscape.new.example()
for (i in seq_along(g$loci[[3]]$alleles)) {
    g$loci[[3]]$alleles[[i]]$state <- if (i == 1L) "AAAA" else "AAAT"
}
cols <- which(landscape.locusvec(g) == 3L) + landscape.democol()
g$individuals[, cols] <- 1L
g$individuals[1, cols[1]] <- 2L
g$individuals[51, cols[1]] <- 2L
stopifnot(is.landscape(g))
expected.theta <- 1 / sum(1 / seq_len(99))
stopifnot(isTRUE(all.equal(landscape.theta.s(g)[, 3], rep(expected.theta, 2))))
dna <- ape::as.DNAbin(do.call(rbind, strsplit(c("AAAT", rep("AAAA", 99)), "")))
expected.D <- pegas::tajima.test(dna)$D
stopifnot(isTRUE(all.equal(tajima(g)[, 3], rep(expected.D, 2))))
stopifnot(all(is.na(landscape.theta.s(g)[, 1:2])))
# Constant padding changes sequence length but not the per-locus statistics.
padded <- g
for (i in seq_along(padded$loci[[3]]$alleles)) {
    padded$loci[[3]]$alleles[[i]]$state <- paste0(padded$loci[[3]]$alleles[[i]]$state,"AAAA")
}
stopifnot(isTRUE(all.equal(landscape.theta.s(padded),landscape.theta.s(g))))
stopifnot(isTRUE(all.equal(tajima(padded),tajima(g))))
# Retain multiplicities: changing frequencies changes Tajima's D.
balanced <- g
balanced$individuals[, cols[1]] <- 2L
stopifnot(!isTRUE(all.equal(tajima(balanced),tajima(g))))
mono <- g
mono$individuals[, cols] <- 1L
stopifnot(all(landscape.theta.s(mono)[, 3] == 0),all(is.na(tajima(mono)[, 3])))
# One diploid individual supplies two sequences; the other population is empty.
small <- g
small$individuals <- small$individuals[1, , drop=FALSE]
stopifnot(is.landscape(small))
stopifnot(landscape.theta.s(small)[1,3] == 1,
          is.na(landscape.theta.s(small)[2,3]),all(is.na(tajima(small))))
bad <- g
bad$loci[[3]]$alleles[[1]]$state <- "AAAN"
stopifnot(inherits(try(landscape.theta.s(bad),silent=TRUE),"try-error"))
bad$loci[[3]]$alleles[[1]]$state <- "AAA"
stopifnot(inherits(try(landscape.theta.s(bad),silent=TRUE),"try-error"))
