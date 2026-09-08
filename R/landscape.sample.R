.landscape.sample.groups <- function(Rland, groups, selected, ns = NULL) {
    if (is.null(ns)) {
        keep <- which(groups %in% selected)
    } else {
        if (!is.numeric(ns) || !length(ns) || anyNA(ns) ||
            any(!is.finite(ns) | ns < 0 | ns != floor(ns)) ||
            !(length(ns) %in% c(1L, length(selected)))) {
            stop("ns must be a non-negative integer or one size per selected group")
        }
        sizes <- rep(ns, length.out = length(selected))
        keep <- unlist(lapply(seq_along(selected), function(i) {
            rows <- which(groups == selected[i])
            if (length(rows) < sizes[i]) return(rows)
            rows[sample.int(length(rows), sizes[i], replace = FALSE)]
        }), use.names = FALSE)
        if (is.null(keep)) keep <- integer()
    }
    Rland$individuals <- Rland$individuals[keep, , drop = FALSE]
    if (!is.null(ns)) {
        Rland$individuals <- Rland$individuals[
            order(Rland$individuals[, 1], Rland$individuals[, 4]), , drop = FALSE]
    }
    Rland
}

landscape.sample.stages <- function(Rland, ns = NULL, svec = NULL) {
    if (is.null(svec)) return(Rland)
    stages <- seq_len(Rland$intparam$stages * Rland$intparam$habitats) - 1L
    if (anyNA(svec) || any(!svec %in% stages)) {
        stop("you have specified demographic stages that do not occur in this landscape")
    }
    if (!is.null(ns) && length(ns) != 1L) stop("stage sampling requires a single ns")
    .landscape.sample.groups(Rland, Rland$individuals[, 1], unique(svec), ns)
}

landscape.sample.pops <- function(Rland, ns = NULL, pvec = NULL) {
    if (is.null(pvec)) return(Rland)
    if (anyNA(pvec) || any(!pvec %in% seq_len(Rland$intparam$habitats))) {
        stop("you have specified populations that cannot occur in this landscape")
    }
    groups <- landscape.populations(Rland)
    .landscape.sample.groups(Rland, groups, unique(pvec), ns)
}

landscape.sample <- function(Rland, np = NULL, ns = NULL, pvec = NULL, svec = NULL) {
    if (!is.null(svec) && (!is.null(pvec) || !is.null(np))) {
        stop("specify either stages or populations, not both")
    }
    if (!is.null(np)) {
        if (!is.numeric(np) || length(np) != 1L || is.na(np) ||
            !is.finite(np) || np < 0 || np != floor(np) || np > Rland$intparam$habitats) {
            stop("np must be an integer between zero and the number of populations")
        }
    }
    if (!is.null(np) || !is.null(pvec)) {
        if (is.null(pvec)) pvec <- sample.int(Rland$intparam$habitats, np, replace = FALSE)
        pvec <- unique(pvec)
        if (!is.null(np) && np != length(pvec)) {
            stop("if both np and pvec are specified, length(pvec) must equal np")
        }
        return(landscape.sample.pops(Rland, ns, pvec))
    }
    if (!is.null(svec)) return(landscape.sample.stages(Rland, ns, svec))
    if (!is.null(ns)) return(landscape.sample.pops(Rland, ns, seq_len(Rland$intparam$habitats)))
    Rland
}
