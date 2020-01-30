
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
    s=rland$intparam$stages
    h=rland$intparam$habitats
    newaf <- NULL
    for (p in names(af))
    {
        pnum <- as.numeric(p)
        stages <- ((pnum-1)*s):((pnum-1)*s + (s) - 1)
        a <- af[[p]]
        newaf <- c(newaf,lapply(stages,function(s)
        {
            lst <- list(a)
            names(lst) <- s
            lst
        })
        )
    }
    newaf <- unlist(newaf,recursive=F)
    landscape.setallelefreq(rland,newaf,states)
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
    
    if (is.null(af))
    {
        print("specify at least some allele frequencies")
    } else #there was an attempt to specify allelefreqs
    {
        locposition <- landscape.locusvec(rland) 
        possLoc <- as.character(1:length(rland$loci))
        possStgs <- as.character(0:((rland$intparam$habitats*rland$intparam$stages)-1))
        ##check to see if stage names in af correspond to possStgs
        if (length(which(!(names(af) %in% possStgs)))>0)
        {
            stop(paste("some stages in af do not exist in landscape"))
        } else { #looks like we can truck through the stages
            for (s in names(af)) #each s is a stage
            {
                inds <- rland$individuals[rland$individuals[,1]==as.numeric(s),]
                for (l in names(af[[s]])) #cruise through the loci
                    if (length(which(!(l%in%possLoc)))>0)
                    {
                        stop(paste("a locus name is not found in the landscape"))
                    } else { #this locus is legal, lets make the changes
                        av <- af[[s]][[l]] #this should be the allele frequency vector for l/pop
                        if (sum(av)>1) {stop("allele freq vector sums to > 1")}
                        lcols <- (which(as.character(locposition)==l)+landscape.democol())
                        n=dim(inds)[1]

                        if (states)
                            {
                                ai <- landscape.locus.states(rland,as.numeric(l),do.check=F)
                                aindex <- ai$aindex[ai$state %in% names(av)]
                            } else aindex <- names(av)
                        for (a in lcols)
                            inds[,a] <- as.numeric(sample(aindex,n,replace=T,prob=av))
                    }
                rland$individuals[s==as.character(rland$individuals[,1]),] <- inds
            }
            
        }
    }
    rland
}
