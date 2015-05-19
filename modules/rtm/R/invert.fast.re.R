invert.fast.re <- function(modname, observed, inits, rand, cons, 
                   pmu, psd, plog, minp, ngibbs){
# Get model code number
    data(model.list)
    setkey(model.list, modname)
    model.set <- model.list[modname]
    if(all(is.na(model.set[,-1,with=FALSE]))){
        stop(sprintf("Error: Model '%s' not found", modname))
    }
    modcode <- as.integer(model.set$modcode)
    print(sprintf("Model: %s; Code: %d", model.set$fullname, modcode))

# Setup initial conditions and constants
    names.all <- unlist(strsplit(model.set$par.names, " "))
    names.inits <- names(inits)
    stopifnot(!is.null(names.inits))
    npars <- length(inits)
    ipars <- match(names.inits, names.all)
    if(length(cons) > 0){
        names.cons <- names(cons)
        stopifnot(!is.null(names.cons))
        ncons <- length(cons)
        icons <- match(names.cons, names.all)
    } else {
        cons <- numeric(0)
        ncons <- as.integer(0)
        icons <- numeric(0)
    }

# Setup random effects
    names.rand <- rownames(rand)
    stopifnot(!is.null(names.rand) && 
              length(names.rand) == length(inits))
    ord.rand <- match(names.rand, names.inits)
    rand <- rand[ord.rand,]

# Force correct types for other parameters
    observed <- as.matrix(observed)
    nspec <- ncol(observed)
    ngibbs <- as.integer(ngibbs)
    results <- matrix(0, ngibbs, npars*(nspec+2)+1)
    seed <- round(1e8 * runif(100))
    seed <- as.integer(seed)

# Group parameters and execute
    in.list <- list("invert_re", observed, nspec, modcode,
                    inits, npars, ipars, rand, 
                    cons, ncons, icons,
                    pmu, psd, plog, minp, ngibbs, results, seed)
    t1 <- proc.time()
    out.list <- do.call(.Fortran, in.list)
    t2 <- proc.time()
    print(t2 - t1)
    return(out.list[[length(out.list)-1]])
}
