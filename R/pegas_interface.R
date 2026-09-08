# Allele-based summaries retain the pegas estimators for ordinary samples.
.landscape.allele.theta <- function(rland, method = c("h", "k")) {
    method <- match.arg(method)
    retval <- matrix(NA_real_, nrow = rland$intparam$habitats,
                     ncol = length(rland$loci))
    populations <- rland$individuals[, 1] %/% rland$intparam$stages + 1L
    locusvec <- landscape.locusvec(rland)
    for (i in seq_len(rland$intparam$habitats)) {
        for (j in seq_along(rland$loci)) {
            cols <- which(locusvec == j) + landscape.democol()
            copies <- as.vector(rland$individuals[populations == i, cols, drop = FALSE])
            n <- length(copies)
            k <- length(unique(copies))
            # Preserve existing NA behaviour for monomorphic samples.
            if (n < 2L || k < 2L) next
            # All copies distinct: theta.k has no finite root.
            if (method == "k" && k == n) next
            retval[i, j] <- if (method == "h") pegas::theta.h(factor(copies)) else
                pegas::theta.k(factor(copies))
        }
    }
    retval
}

landscape.theta.h <- function(rland) {
    .landscape.allele.theta(rland, "h")
}

landscape.theta.k <- function(rland) {
    .landscape.allele.theta(rland, "k")
}

# Extract every sampled gene copy, not just unique allele states.
.landscape.sequence.summary <- function(rland, tajima = FALSE) {
    retval <- matrix(NA_real_, nrow = rland$intparam$habitats,
                     ncol = length(rland$loci))
    populations <- rland$individuals[, 1] %/% rland$intparam$stages + 1L
    locusvec <- landscape.locusvec(rland)
    for (j in seq_along(rland$loci)) {
        if (rland$loci[[j]]$type != 253) next
        alleles <- rland$loci[[j]]$alleles
        indices <- vapply(alleles, function(a) as.character(a$aindex), character(1))
        states <- vapply(alleles, function(a) as.character(a$state), character(1))
        cols <- which(locusvec == j) + landscape.democol()
        for (i in seq_len(rland$intparam$habitats)) {
            sampled <- as.vector(rland$individuals[populations == i, cols, drop = FALSE])
            n <- length(sampled)
            if (n < 2L) next
            matched <- match(as.character(sampled), indices)
            if (anyNA(matched)) stop("Sampled sequence allele index is not in the allele table")
            sequences <- toupper(states[matched])
            widths <- nchar(sequences)
            if (anyNA(sequences) || length(unique(widths)) != 1L ||
                any(widths == 0L) || any(grepl("[^ACGT]", sequences))) {
                stop("Sequence summaries require equal-length, non-empty A/C/G/T sequences")
            }
            dna <- do.call(rbind, strsplit(sequences, "", fixed = TRUE))
            S <- sum(apply(dna, 2L, function(site) length(unique(site)) > 1L))
            if (!tajima) {
                # Watterson's theta per locus: n is the number of gene copies.
                retval[i, j] <- pegas::theta.s(S, n)
            } else if (n >= 4L && S > 0L) {
                # Standard Tajima's D; undefined small/monomorphic cases stay NA.
                D <- pegas::tajima.test(ape::as.DNAbin(dna))$D
                if (is.finite(D)) retval[i, j] <- D
            }
        }
    }
    retval
}

landscape.theta.s <- function(rland) {
    .landscape.sequence.summary(rland)
}

# Internal: standard Tajima's D, retaining sampled haplotype multiplicities.
landscape.tajima.d <- function(rland) {
    .landscape.sequence.summary(rland, tajima = TRUE)
}
