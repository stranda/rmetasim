library(rmetasim)
set.seed(123)
g <- landscape.new.example()
for (p in 1:2) for (j in seq_along(g$loci)) {
    cols <- which(landscape.locusvec(g)==j)+landscape.democol()
    copies <- as.vector(g$individuals[landscape.populations(g)==p,cols,drop=FALSE])
    stopifnot(isTRUE(all.equal(landscape.theta.h(g)[p,j],pegas::theta.h(factor(copies)))))
    stopifnot(isTRUE(all.equal(landscape.theta.k(g)[p,j],pegas::theta.k(factor(copies)))))
}
small <- landscape.new.individuals(g,c(1,0,1,0))
cols <- which(landscape.locusvec(small)==1)+landscape.democol()
small$individuals[,cols] <- matrix(rep(c(1L,2L),2),nrow=2,byrow=TRUE)
stopifnot(is.landscape(small))
stopifnot(all(landscape.theta.h(small)[,1]==pegas::theta.h(factor(c(1,2)))))
stopifnot(all(is.na(landscape.theta.k(small)[,1])))
small$individuals <- small$individuals[1,,drop=FALSE]
stopifnot(all(is.na(landscape.theta.h(small)[2,])),all(is.na(landscape.theta.k(small)[2,])))
small$individuals[,cols] <- 1L
stopifnot(is.na(landscape.theta.h(small)[1,1]),is.na(landscape.theta.k(small)[1,1]))
