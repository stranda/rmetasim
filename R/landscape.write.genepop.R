# Internal diploid-only GENEPOP writer. Haploid loci are intentionally omitted.
# Retain the legacy coding: numeric states + 1, or sequence allele indices + 1.
# Codes must be integers 001..999; no automatic recoding of oversized values.
# Validate and format everything before opening the output file.
# Labels include original row number, individual ID, class and generation.
landscape.write.genepop <- function(Rland, fn = "genepop.out",
                                   title = "rmetasim landscape output") {
    if (!is.landscape(Rland)) stop("Invalid landscape", call. = FALSE)
    if (!is.character(fn) || length(fn) != 1L || is.na(fn) || !nzchar(fn)) {
        stop("fn must be a non-empty filename", call. = FALSE)
    }
    if (!is.character(title) || length(title) != 1L || is.na(title) ||
        grepl("[\r\n]", title)) stop("title must be a single line", call. = FALSE)
    diploid <- which(landscape.ploidy(Rland) == 2L)
    if (!length(diploid)) stop("No diploid loci to export", call. = FALSE)
    individuals <- Rland$individuals
    locusvec <- landscape.locusvec(Rland)
    genotypes <- matrix("", nrow(individuals), length(diploid))
    for (k in seq_along(diploid)) {
        j <- diploid[k]
        cols <- which(locusvec == j) + landscape.democol()
        values <- individuals[, cols, drop = FALSE]
        if (Rland$loci[[j]]$type != 253) {
            alleles <- Rland$loci[[j]]$alleles
            indices <- vapply(alleles, function(a) as.character(a$aindex), character(1))
            states <- vapply(alleles, function(a) as.numeric(a$state), numeric(1))
            values[] <- states[match(as.character(values), indices)]
        }
        codes <- values + 1
        if (anyNA(codes) || any(!is.finite(codes) | codes != floor(codes) |
                               codes < 1 | codes > 999)) {
            stop("Locus ", j, " cannot be encoded with three-digit GENEPOP alleles",
                 call. = FALSE)
        }
        genotypes[, k] <- sprintf("%03d%03d", as.integer(codes[, 1]),
                                  as.integer(codes[, 2]))
    }
    labels <- paste0("Row-", seq_len(nrow(individuals)),
                     " ID-", individuals[, 4], " Class-", individuals[, 1],
                     " Gen-", individuals[, 3])
    rows <- paste0(labels, ", ", apply(genotypes, 1, paste, collapse = " "))
    populations <- individuals[, 1] %/% Rland$intparam$stages + 1L
    lines <- c(paste0(date(), ": ", title), paste0("locus-", diploid))
    for (p in unique(populations)) lines <- c(lines, "POP", rows[populations == p])
    writeLines(lines, fn)
    invisible(NULL)
}
