landscape.sample.stages <- function(rland,ns=NULL,svec=NULL)
    {
        if (is.null(svec))
            {
                rland
            } else { #there are stages to check out
                stgs <- 0:((rland$intparam$stages * rland$intparam$habitats)-1)
                if (sum(!(svec%in%stgs))>0) stop ("you have specified demographic stages that do no occur in this landscape")
                rland$individuals <- rland$individuals[rland$individuals[,1]%in%svec,]
                if (!is.null(ns))
                    {
                        for (s in svec)
                            {
                                inds <- rland$individuals[rland$individuals[,1]==s,]
                                rland$individuals <-  rland$individuals[rland$individuals[,1]!=s,]
                                inddim <- dim(inds)[1]
                                if (inddim>0)
                                    if (inddim<ns)
                                        rland$individuals <- rbind(rland$individuals,inds)
                                    else
                                        rland$individuals <- rbind(rland$individuals,inds[sample(1:inddim,ns,replace=F),])
                            }
                        rland$individuals <- rland$individuals[order(rland$individuals[,1],rland$individuals[,4]),]
                    }
                rland
            }
    }

landscape.sample.pops <- function(rland,ns=NULL,pvec=NULL)
    {
        if (is.null(pvec))
            {
                rland
            } else { #there are pops
                pops <- 1:rland$intparam$habitats
                if (sum(!(pvec%in%pops))>0) stop ("you have specified populations that can not occur in this landscape")
                rland$individuals <- rland$individuals[landscape.populations(rland)%in%pvec,]
                if (!is.null(ns))
                    {
                        for (p in pvec)
                            {
                                inds <- rland$individuals[landscape.populations(rland)==p,]
                                rland$individuals <-  rland$individuals[landscape.populations(rland)!=p,]
                                inddim <- dim(inds)[1]
                                if (inddim<ns)
                                    rland$individuals <- rbind(rland$individuals,inds)
                                else
                                    rland$individuals <- rbind(rland$individuals,inds[sample(1:inddim,ns,replace=F),])
                            }
                        rland$individuals <- rland$individuals[order(rland$individuals[,1],rland$individuals[,4]),]
                    }
                rland
            }
    }



landscape.sample <- function (rland, np = NULL, ns = NULL, pvec = NULL, svec=NULL) 
{

    if ((!is.null(svec))&(!is.null(pvec))) stop ("either pvec or svec should be specified, not both")
    if ((!is.null(np))|(!is.null(pvec)))
        {
            if (is.null(pvec))
                {
                    pvec <- sample(1:rland$intparam$habitats,np,F)
                }
            if ((!is.null(np))&(!is.null(pvec)))
                if (np!=length(pvec)) stop ("if you specify both np and pvec, the length of pvec has to equal np")
            retland <- landscape.sample.pops(rland,ns=ns,pvec=pvec)
        } else 
            if (!is.null(svec))
                {
                    retland <- landscape.sample.stages(rland,ns=ns,svec=svec)
                } else
                    if (!is.null(ns))
                        {
                            retland <- landscape.sample.pops(rland,ns=ns,pvec=1:rland$intparam$habitats)
                        } else retland <- rland
    retland
}
