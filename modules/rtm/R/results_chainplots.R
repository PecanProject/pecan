##' @name chain.plots
##' @title Plot individual chains
##' @details {
##' Interactive chain by chain plot of mcmc.list objects. Individual chains can be selected for removal as they appear.
##' For each question, enter '1' for 'Yes' and '0' for 'No'.
##' }

##' @param mcmclist 'mcmc.list' object from which chains will be extracted.
##' @return Returns 'mcmc.list' object with 'bad' chains removed.
##' @export
##' 
##' @author Alexey Shiklomanov

library(coda)

chain.plots <- function(mcmclist){
        plot(mcmclist)
        t1 <- readline("View details? : ")
        if(t1 == 1){
                l.m <- length(mcmclist)
                out <- NULL
                for (i in 1:l.m){
                        plot(mcmclist[[i]])
                        t2 <- readline("Discard? ")
                        if(t2 == 1){
                                out <- c(out, i)
                        }
                }
                if(length(out) > 0) {
                        return(mcmclist[-out])
                } else {
                        return(mcmclist)
                }
        } else {
                return(mcmclist)
        }
        
}

