library(rmetasim)
set.seed(123)
g <- landscape.new.example()
stopifnot(is.landscape(g))
z <- landscape.sample(g, pvec = 2, ns = 2)
stopifnot(nrow(z$individuals) == 2, all(landscape.populations(z) == 2))
z <- landscape.sample(g, pvec = c(2, 1), ns = c(2, 3))
stopifnot(identical(as.integer(table(landscape.populations(z))), c(3L, 2L)))
small <- landscape.new.individuals(g, c(1, 0, 1, 0))
stopifnot(is.landscape(small))
for (z in list(landscape.sample(small, pvec = c(1, 2), ns = 1),
               landscape.sample(small, svec = c(0, 2), ns = 1))) {
    stopifnot(is.matrix(z$individuals), nrow(z$individuals) == 2,
              isTRUE(all.equal(z$individuals, small$individuals)), is.landscape(z))
}
z <- landscape.sample(small, pvec = 2)
stopifnot(is.matrix(z$individuals), nrow(z$individuals) == 1)
z <- landscape.sample(g, svec = 1, ns = 2) # unoccupied stage
stopifnot(is.matrix(z$individuals), nrow(z$individuals) == 0)
z <- landscape.sample(g, ns = 0)
stopifnot(is.matrix(z$individuals), nrow(z$individuals) == 0,
          ncol(z$individuals) == ncol(g$individuals))
z <- landscape.sample(g, pvec = 1, ns = 1000)
stopifnot(nrow(z$individuals) == 50)
stopifnot(inherits(try(landscape.sample(g, ns = -1), silent = TRUE), "try-error"))
stopifnot(inherits(try(landscape.sample(g, pvec = c(1,2), ns = c(1,2,3)), silent = TRUE), "try-error"))
