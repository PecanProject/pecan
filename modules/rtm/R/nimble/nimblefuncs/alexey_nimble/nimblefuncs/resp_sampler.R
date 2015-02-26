sampler_resp <- nimbleFunction(
        contains = sampler_BASE,
        setup = function(model, mvSaved, control) {
                targetNode <- control$targetNode
                calcNodes <- model$getDependencies(targetNode)
                getrefl <- prospect_refl(model, prospectConstants)
        },
        run = function(){
                ### Initializing variables
                declare(specerror, double(2, c(wl, nspec)))
                Refl <- getrefl$run()
                for (i in 1:nspec){
                        specerror[,i] <- Refl - observed[,i]
                }
                rp1 <- nspec * wl / 2
                rp2 <- (nspec * wl - 1) * var(specerror)
                rp <- rgamma(1, rp1, rp2)

                model[[targetNode]] <<- rp
                calculate(model, calcNodes)
                copy(from = model, to = mvSaved, nodes = calcNodes, row = 1, logProb = TRUE)
                returnType(integer(0))
                return(1)
        })