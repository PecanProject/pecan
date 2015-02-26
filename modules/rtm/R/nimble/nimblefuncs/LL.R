### Custom log-likelihood function for NIMBLE
### Metropolis-Hastings algorithm

prospect_LL <- nimbleFunction(
        setup = function(model, constants) {
                ### Load constants
                observed <- constants$observed
                nspec <- constants$nspec
                wl <- constants$wl

                ### Load specerror function
                getrefl <- prospect_refl(model, constants)

        },
        run = function(){
                declare(specerror, double(2, c(wl, nspec)))
                Refl <- getrefl$run()
                for(i in 1:nspec){
                        specerror[,i] <- Refl - observed[,i]
                }
                logL <- 0.0
                for(i in 1:wl){
                        for(j in 1:nspec) {
                                logL <- logL + dnorm(specerror[i,j], 0, model$resp, 1)
                        }
                }
                returnType(double(0))
                return(logL)
        })
