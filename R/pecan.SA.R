pecan.SA <- function(M, yr0, yrf, date) {
  ## PECAn Sensitivity Analysis

  ## solving var(f) = sum((df/dtheta)^2*var(theta)) + O3
  ## theta are the n trait inputs defined in 'trait' dataframe
  ## f are the model outputs at different quantiles of theta

  ## contents:
  ##   log:   error log
  ##   trait: trait defs (id, fileid, figid)
  ##   tr.n:  number of traits
  ##   pft:   plant fn. type (string), e.g. 'c4crop', 'temphw'
  ##   post.dtheta.q, prior.dtheta.q: prior and posterior statistics (lcl, median,ucl, mean, var, cv)
  ##   samps: prior and posterior samples (n=5000) for each trait
  ##   priors:prior parameters for each trait 

####Import data from ED2 output
  outdir <- paste("/home/scratch/dlebauer/output/grassSA/out",date,sep="")
  trait.defs <- trait.dictionary()
  run.id <- c('post', 'prior')
  var.id <- c('agb', 'ssc')

  ## .f.yr is a list of files with annual output
  .f <- dir(outdir, full.names=TRUE)      ## grab all files 
  .f.yr <- .f[grep("-Y-", .f)]            ## select annual output
  t.range <- c(yr0, yrf)
  dt <- yrf - yr0 + 1
  saruns <- saruntype <- character()
  .i <- 1
  for(.p in run.id) {
    for(.cl in c('lcl', 'ucl')) {
      for (.id in as.character(trait.defs$fileid)) {
        saruns[.i] <- paste(.p, .cl, '.', .id, '-', sep = "")
        saruntype[.i] <- paste(.p, .cl, sep = "")
        .i <- .i+1
      }
    }
  }


  ## first create a vector from 1:M with leading zeros
  .ens.id <- paste('x', as.character(seq(1,M)+1000), sep = '')
  ens.id <- gsub('x1','0',.ens.id)

  runnames <- c(paste("postsamp", ens.id,'-', sep=""),
                paste ("priorsamp", ens.id, '-', sep=""),
                saruns, 'postmeans', 'priormeans')
  runtype <- c(rep("postsamp", M),
               rep("priorsamp", M),
               saruntype, 'postmeans', 'priormeans')
  .list <- list('output' = matrix(nrow = length(runnames), ncol = dt),
                prior.mean.f = numeric(),
                prior.var.f = numeric(),
                post.mean.f = numeric(),
                post.var.f = numeric())
  dat <- list(runtype = runtype, # specifies prior, posterior, or SA quantile run
              'agb' = .list,
              'ssc' = .list) 

  for(i in names(dat)[-1]) {
    rownames(dat[[i]][['output']]) <- runnames
    colnames(dat[[i]][['output']]) <- seq(yr0, yrf)
  }


  
  for (.iyr in seq(yr0, yrf)) {
    .f.iyr <- .f.yr[grep(.iyr,.f.yr)]
    for (.i.run in seq(runnames)) {
      .f.iyr.irun <- .f.iyr[grep(runnames[.i.run], .f.iyr)]
      if(!identical(.f.iyr.irun, character(0))) {
        .a<- hdf5load(.f.iyr.irun, load=FALSE)[c("AGB_CO", "SLOW_SOIL_C", "NPLANT")]
        dat[['agb']][['output']][runnames[.i.run], as.character(.iyr)] <- sum(.a$AGB_CO * .a$NPLANT) * 20
                                        #*20 converts from g/m^2 to Mt/ha
        dat[['ssc']][['output']][runnames[.i.run], as.character(.iyr)] <- .a$SLOW_SOIL_C
      }
    }
  }

  satables <- list()
  for (.r in c('post', 'prior')) {
    satables[[.r]] <- list()
    for (.i in c('agb', 'ssc')) {
      satables[[.r]][[.i]] <- pecan.SAcalcs(.r,  .i, dat, traits = prvec, trait.samps)
    }
  }

  save(satables, file = "pecan.SA.Rdata")
}
