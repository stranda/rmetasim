library(rmetasim)
# Run after installing the proposed patch. These are real native landscapes.
set.seed(123)
g <- landscape.new.example()
stopifnot(is.landscape(g))
democol <- landscape.democol()
cols <- which(landscape.locusvec(g)==1)+democol
rows <- g$individuals[,1]==0
for (prob in list(c('2'=1,'1'=0),c('1'=0,'2'=1),c('2'=1))) {
  z <- landscape.setallelefreq(g,list('0'=list('1'=prob)),states=TRUE)
  stopifnot(all(z$individuals[rows,cols]==2))
  stopifnot(isTRUE(all.equal(z$individuals[!rows,,drop=FALSE],g$individuals[!rows,,drop=FALSE])))
  stopifnot(is.landscape(z))
}
# Same named probabilities produce the same draws regardless of input order
# when one allele has all probability mass. A fixed allele-index request also works.
z <- landscape.setallelefreq(g,list('0'=list('1'=c('2'=1))),states=FALSE)
stopifnot(all(z$individuals[rows,cols]==2))
err <- try(landscape.setallelefreq(g,list('0'=list('1'=c('unknown'=1)))),silent=TRUE)
stopifnot(inherits(err,"try-error"))
