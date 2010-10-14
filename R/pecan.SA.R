pecan.SA <- function(M, yr0, yrf, date, outdir) {
  ## PECAn Sensitivity Analysis

  ## solving var(f) = sum((df/dtheta)^2*var(theta)) + O3
  ## theta are the n trait inputs defined in 'trait' dataframe
  ## f are the model outputs at different quantiles of theta

  ##Import filenames from ED2 output 
  trait.defs <- trait.dictionary()

  ## .f.yr is a list of files with annual output
  .f <- dir(outdir, full.names=TRUE)      ## grab all files in outdir
  .f.yr <- .f[grep("-Y-", .f)]            ## select annual output
  t.range <- c(yr0, yrf)


  ### Creating list 'dat' for data storage
  
  ## create vectors sarun, saruntype
  ## sarun - run ids
  ## saruntype - e.g.: post, prior
  saruns <- saruntype <- character()
  run.id <- c('post', 'prior')
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

  ## create a vector from 1:M with leading zeros
  .ens.id <- paste('x', as.character(seq(1,M)+1000), sep = '')
  ens.id <- gsub('x1','0',.ens.id)

  runnames <- c(paste("postsamp", ens.id,'-', sep=""),
                paste ("priorsamp", ens.id, '-', sep=""),
                saruns, 'postmeans', 'priormeans')
  runtype <- c(rep("postsamp", M),
               rep("priorsamp", M),
               saruntype, 'postmeans', 'priormeans')
  outlist <- list('output' = matrix(nrow = length(runnames), ncol = yrf - yr0 + 1),
                prior.mean.f = numeric(),
                prior.var.f = numeric(),
                post.mean.f = numeric(),
                post.var.f = numeric())
  dat <- list(runtype = runtype, # specifies prior, posterior, or SA quantile run
              'agb' = outlist,
              'ssc' = outlist) 

  for(i in names(dat)[-1]) {
    rownames(dat[[i]][['output']]) <- runnames
    colnames(dat[[i]][['output']]) <- seq(yr0, yrf)
  }


  ## extract data from hdf5 output files to dat
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

  ## create tables with data for sensitivity analysis
  satables <- list()
  for (.r in c('post', 'prior')) {
    satables[[.r]] <- list()
    for (.i in c('agb', 'ssc')) {
      satables[[.r]][[.i]] <- pecan.SAcalcs(.r,  .i, dat, trait.defs, trait.samps)
    }
  }

  return(list(satables=satables,
              transformed.samps = dat))
}
