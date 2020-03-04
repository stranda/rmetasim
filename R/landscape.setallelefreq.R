
##
## function to specify allele frequencies on a per stage/per locus level
##
## rland is a landscape object
## af is a heirarchical list with population/habitat as the top level
## and locus as intermediate.  This object needs to have names in the top
## level corresponding to stages and names on the next level corresponding
## loci
## all this function does is expand the populations into thier consitutent
## stages and then calls landscape.setallelefreq with an updated "af"

landscape.setpopfreq <- function(rland,af=NULL,states=TRUE)
{
    if (FALSE)
    {
        af <- list('1'=list('1'=c('1'=0.5,'2'=0.25,'5'=0.25),   #population 1 locus 1
                            '2'=c('1'=0.5,'2'=0.25,'3'=0.25)),  #population 1 locus 2
                   '2'=list('1'=c('3'=0.5,'2'=0.35,'5'=0.15),   #population 2 locus 1
                            '2'=c('2'=0.25,'3'=0.75)))          ##population 2 locus 2
    }
    posspops <- sort(unique(landscape.populations(rland)))
    s = rland$intparam$stages
    h = rland$intparam$habitats
    newaf <- lapply(names(af), function(p) {
      pnum <- as.numeric(p)
      stages <- ((pnum - 1) * s):((pnum - 1) * s + s - 1)
      lapply(stages, function(s) stats::setNames(list(af[[p]]), s))
    })
    newaf <- unlist(newaf, recursive = F)
    landscape.setallelefreq(rland, newaf, states)
}




##
## function to specify allele frequencies on a per stage/per locus level
##
## rland is a landscape object
## af is a heirarchical list with stage as the top level

## and locus as intermediate.  This object needs to have names in the top
## level corresponding to stages and names on the next level corresponding
## loci

landscape.setallelefreq <- function(rland,af=NULL,states=TRUE)
{
    if (FALSE)
    {
        af <- list('0'=list('1'=c('1'=0.5,'2'=0.25,'5'=0.25),'2'=c('1'=0.5,'2'=0.25,'3'=0.25)),
                   '2'=list('1'=c('1'=0.5,'2'=0.35,'5'=0.15),'2'=c('1'=0.10,'2'=0.15,'3'=0.75)))
    }

  if (is.null(af)) {
    cat("specify at least some allele frequencies\n")
  } else {
    locposition <- as.character(landscape.locusvec(rland))
    possLoc <- as.character(1:length(rland$loci))
    possStgs <- as.character(0:((rland$intparam$habitats * rland$intparam$stages) - 1))
    democol <- landscape.democol()
    if (length(which(!(names(af) %in% possStgs))) > 0) {
      stop(paste("some stages in af do not exist in landscape"))
    }
    else {
      for (s in names(af)) {
        s.inds <- rland$individuals[, 1] == as.numeric(s)
        inds <- rland$individuals[s.inds, ]
        n <- dim(inds)[1]
        if(!all(names(af[[s]]) %in% possLoc)) {
          stop("a locus name is not found in the landscape")
        }
        for (l in names(af[[s]])) {
          av <- af[[s]][[l]]
          if (sum(av) > 1) stop("allele freq vector sums to > 1")
          aindex <- if (states) {
            ai <- landscape.locus.states(rland, as.numeric(l), do.check = F)
            ai$aindex[ai$state %in% names(av)]
          } else names(av)
          lcols <- which(locposition == l) + democol
          inds[, lcols] <- as.numeric(
            sample(aindex, n * length(lcols), replace = T, prob = av)
          )
        }
        rland$individuals[s.inds, ] <- inds
      }
    }
  }
  rland
}
