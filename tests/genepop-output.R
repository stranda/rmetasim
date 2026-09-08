library(rmetasim)
writer <- getFromNamespace("landscape.write.genepop", "rmetasim")
set.seed(123)
g <- landscape.new.example()
check_output <- function(g) {
    f <- tempfile()
    on.exit(unlink(f))
    writer(g, f)
    lines <- readLines(f)
    stopifnot(identical(lines[2:3], c("locus-1","locus-3")))
    rows <- lines[grepl(",", lines, fixed=TRUE)]
    stopifnot(length(rows)==nrow(g$individuals), !anyDuplicated(sub(",.*","",rows)))
    tokens <- strsplit(sub("^.*, ","",rows)," ",fixed=TRUE)
    stopifnot(all(lengths(tokens)==2L),all(grepl("^[0-9]{6}$",unlist(tokens))))
    # Recover source row numbers rather than assuming population output order.
    source.rows <- as.integer(sub("^Row-([0-9]+).*","\\1",rows))
    for (k in 1:2) {
        j <- c(1L,3L)[k]
        cols <- which(landscape.locusvec(g)==j)+landscape.democol()
        expected <- g$individuals[source.rows,cols,drop=FALSE]
        if(j==1L) {
            a <- g$loci[[j]]$alleles
            lookup <- setNames(vapply(a,function(x)x$state,numeric(1)),
                               vapply(a,function(x)as.character(x$aindex),character(1)))
            expected[] <- lookup[as.character(expected)]
        }
        expected <- expected+1
        codes <- vapply(tokens, function(x)x[k], character(1))
        stopifnot(all(as.integer(substr(codes,1,3))==expected[,1]),
                  all(as.integer(substr(codes,4,6))==expected[,2]))
    }
}
check_output(g)
check_output(landscape.new.individuals(g,c(1,0,0,0)))
bad <- g
for(i in seq_along(bad$loci[[1]]$alleles)) bad$loci[[1]]$alleles[[i]]$state <- as.integer(1000+i)
f <- tempfile()
writeLines("existing file",f)
stopifnot(inherits(try(writer(bad,f),silent=TRUE),"try-error"),
          identical(readLines(f),"existing file"))
unlink(f)
stopifnot(inherits(try(writer(bad,f),silent=TRUE),"try-error"),!file.exists(f))
