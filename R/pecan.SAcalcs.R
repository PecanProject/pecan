pecan.SAcalcs <- function(runname, outvar, dat, dtheta.q, trait.defs, trait.samps) {

  ## Values of Theta
  ## #######################
  ## Load dtheta values used in sensitivity analysis
  ## dtheta.q has cbind(lcl, ucl, mean, var, cv)
  dq <- as.data.frame(signif(dtheta.q[[runname]], 4))
    
  colnames(dq) <- c('lcl.theta', 'ucl.theta', 'mean.theta', 'var.theta', 'cv.theta') 
  dq$id <- rownames(dq)

  ##  Transform degrees C to K fof Vm_low temp
  .vmlt <- dq['Vm_low_temp', c('lcl.theta', 'ucl.theta', 'mean.theta') ]
  dq['Vm_low_temp', c('lcl.theta', 'ucl.theta', 'mean.theta')] <- c(.vmlt+273.15)

  ## Calculate Values of 'f'
  ## f is the model output variable, either 'agb' or 'ssc' for now
  
  ##  define 'f'
  if(runname == 'post') {
    f <- dat[[outvar]][['output']][which(dat[["runtype"]] == "postsamp"),]
  } else if (runname == 'prior') {
    f <- dat[[outvar]][['output']][which(dat[["runtype"]] == "priorsamp"),]
  } else { print ('runname id not valid')
         }      
  
  ## Calculate overall mean and variance
  ## mean: average each run across years and ensemble runs
  mean.f <- mean(rowMeans(f, na.rm=TRUE),  na.rm = TRUE)
  ## var(f) from ensemble runs
  var.f  <- var(rowMeans(f, na.rm=TRUE),  na.rm = TRUE)

  #identify row in f with mean data for the runname = 'post' or 'prior'
  mean.rowname <- paste(runname, 'means', sep = "") 

  ## Make table of values used in Sensitivity Analysis
  satable <-  merge(dq, trait.defs, by = 'id')
  rownames(satable) <- satable$id
  
  ## all mean "f's" are the same
  satable$mean.f <- mean(dat[[outvar]][['output']][mean.rowname,],na.rm=T)
  ## calculate the values of f +/- df
  for (.j in seq(satable$id)) {
    for (.k in c('lcl', 'ucl')) {
      .j1 <- as.character(satable$fileid[.j])
      .j2 <- as.character(satable$id[.j])
      sa.rowname <- paste(runname, .k, '.', .j1, '-', sep="")
      if (sa.rowname %in% rownames(dat[[outvar]][['output']])){
        satable[.j2, paste(.k, '.f', sep = '')] <- mean(dat[[outvar]][['output']][sa.rowname, ], na.rm=T)
      }
    }
  }
   
  ##~~~~~~~~~~~~~~~~~~~~~~~
  ## Data Organization Done
  ##~~~~~~~~~~~~~~~~~~~~~~~

  ##~~~~~~~~~~~~~~~~~~~~~~~
  ## Begin SA Calculations
  ## required inputs: sa.df, var.f, dtheta, dfdth()
  ##~~~~~~~~~~~~~~~~~~~~~~~
 
  pdf(paste('out/', runname, outvar, 'dfdth.SA.pdf', sep = ""))
  ymin <- min(satable[,c('mean.f', 'lcl.f', 'ucl.f')])*0.95
  ymax <- max(satable[,c('mean.f', 'lcl.f', 'ucl.f')])*1.05
  if(is.na(ymin)) ymin <- min(0, mean.f - sqrt(var.f))
  if(is.na(ymax)) ymax <- mean.f + sqrt(var.f)

  dfdth.terms <- sapply(satable$id,
                        function(x) {
                          dfdth(f = satable[x, c('lcl.f', 'mean.f', 'ucl.f')],
                                th = satable[x, c('lcl.theta', 'mean.theta', 'ucl.theta')],
                                yrange = c(ymin, ymax),
                                name = x)})
  dev.off()
  rownames(dfdth.terms) <- c('df', 'd2f')

  satable <- cbind(satable, t(dfdth.terms))
  satable$df <- as.numeric(satable$df)
  satable$d2f <- as.numeric(satable$d2f)
  
  ## elasticity = (df/dth)/(mean.f/mean.theta)
  satable$elast <- with(satable,
                        df/(mean.f/mean.theta))

  ## var.f = (df)^2 * var.theta + 
  for (.jid in seq(satable$id)) {
    x <- trait.samps[[paste(runname,'.samps',sep='')]][,.jid]
    a <- mean(x)
    df <- satable$df[.jid]
    d2f<- satable$d2f[.jid]
    o1 <- var(df*x)#var to first order taylor series expansion 
    o2 <- var(df*x + 0.5 * d2f * (x-a)^2)#var to second order\
    print(satable$id[.jid])
    print(cbind(c('n',length(x)), c('O1 term',signif(o1,2)),c('O2 term',signif(o2,2)), c('ratio O1:O2',signif(o1/o2,2))))
    satable$o1[.jid] <- o1
    satable$o2[.jid] <- o2
  }
  sum.rhs <- sum(satable$o1)
  
  for (.jid in seq(satable$id)) {
    ## Var explained by param i 
    ## percent var: var(f) as denominator
    satable$per.var[.jid] <- satable$o1[.jid]/var.f
    ## rel.var: var(f) - higher order closure terms as denominator
    satable$rel.var[.jid] <- satable$o1[.jid]/sum.rhs
  }
  var.explained <- sum(satable$per.var)
  satable$null <- rep(0, length(satable$id)) #dummy for plotting probably obsolete
  # satable$rel.var[which(satable$rel.var < 0.00001)] <- 0
  return(satable)
}
