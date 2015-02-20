### Custom Gibbs sampler for the residual error precision ("resp")
sampler_resp <- nimbleFunction(
        contains = sampler_BASE,
        setup = function(model, mvSaved, control) {
                targetNode <- control$targetNode
                calcNodes <- model$getDependencies(targetNode)

                constants <- prospectConstants      # Workaround. I couldn't pass constants to the sampler.
                getrefl <- prospect_refl(model, constants)
                observed <- constants$observed
                nspec <- constants$nspec
                abs <- constants$Cab_abs
                wl <- length(abs)

        },
        run = function(){
                ### Initializing variables
                declare(specerror, double(2, c(wl, nspec)))
                declare(Refl, double(1, wl))
                declare(observed, double(2, c(wl, nspec)))
                declare(nspec, double(0))
                declare(wl, double(0))

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

        },

        methods = list(
                reset = function() {})
)
