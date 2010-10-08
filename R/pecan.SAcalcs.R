pecan.SAcalcs <- function(.irun, .ivar, dat, traits, trait.samps) {
                                        # .irun <- c('post', 'prior')
                                        # .ivar <- c('agb', 'ssc')

  ## Values of Theta
  ## #######################
  ## Load dtheta values used in sensitivity analysis
  ## dtheta.q has cbind(lcl, ucl, mean, var, cv)
                                        #!!need to fix error?
  if(.irun == 'post') {
    f <- dat[[.ivar]][['output']][which(dat[["runtype"]] == "postsamp"),]
  } else if (.irun == 'prior') {
    f <- dat[[.ivar]][['output']][which(dat[["runtype"]] == "priorsamp"),]
  } else { print ('.irun id not valid')
         }      
 
  ## Calculate overall mean and variance
  ## average each run across years before calculations 
  mean.f <- mean(rowMeans(f, na.rm=TRUE),  na.rm = TRUE)
  var.f  <- var(rowMeans(f, na.rm=TRUE),  na.rm = TRUE)

  mean.rowname <- paste(.irun, 'means', sep = "") #identify row in f with mean data

  ## convert the dtheta.q to dataframe
  .dq <- as.data.frame(eval( parse ( text = paste(.irun, '.dtheta.q', sep = ""))))
  
  
  colnames(.dq) <- c('lcl.theta', 'ucl.theta', 'mean.theta', 'var.theta', 'cv.theta') 
  .dq$id <- rownames(.dq)
  traits<-trait.dictionary()

  ##  Transform degrees C to K fof Vm_low temp
  .vmlt <- .dq['Vm_low_temp', c('lcl.theta', 'ucl.theta', 'mean.theta') ]
  .dq['Vm_low_temp', c('lcl.theta', 'ucl.theta', 'mean.theta')] <- c(.vmlt+273.15)

  ## Start making table for Sensitivity Analysis
  satable <-  merge(.dq, traits, by = 'id')
  rownames(satable) <- satable$id
  
  ## Values of f
  ## #################
  ## df from SA for change in f at mean
  ## var(f) from ensemble runs
  
  ## all mean "f's" are from the same run
  satable$mean.f <- mean(dat[[.ivar]][['output']][mean.rowname,],na.rm=T)
  for (.j in seq(satable$id)) {
    for (.k in c('lcl', 'ucl')) {
      .j1 <- as.character(satable$fileid[.j])
      .j2 <- as.character(satable$id[.j])
      sa.rowname <- paste(.irun, .k, '.', .j1, '-', sep="")
      if (sa.rowname %in% rownames(dat[[.ivar]][['output']])){
        satable[.j2, paste(.k, '.f', sep = '')]<- mean(dat[[.ivar]][['output']][sa.rowname, ],na.rm=T)
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
 
  pdf(paste(.irun, .ivar, 'dfdth.SA.pdf', sep = ""))
  ymin <- min(satable[,c('mean.f', 'lcl.f', 'ucl.f')])*0.95
  ymax <- max(satable[,c('mean.f', 'lcl.f', 'ucl.f')])*1.05
  if(is.na(ymin)) ymin <- min(0, mean.f - sqrt(var.f))
  if(is.na(ymax)) ymax <- mean.f + sqrt(var.f)
  for (.jid in which(!is.na(satable$lcl.f))) {
    fig.title <- as.character(satable$figid[.jid])
    id <- satable$id[.jid]
    dtheta.j <- as.numeric(satable[.jid, c('lcl.theta', 'mean.theta', 'ucl.theta')])
    df.ij <- as.numeric(satable[.jid, c('lcl.f', 'mean.f', 'ucl.f')])
    dfdth.terms <- dfdth(f = df.ij, th = dtheta.j, yrange = c(ymin, ymax), name = fig.title)
    satable$df[.jid]  <- dfdth.terms$dfdth1
    satable$d2f[.jid] <- dfdth.terms$dfdth2
    satable$elast[.jid] <- satable$d2f[.jid]*dtheta.j[2]/df.ij[2]
  }
  dev.off()

  sum.var <- sum(satable$var.theta)

  ## var.f = (df)^2 * var.theta + 
  for (.jid in which(!is.na(satable$id))) {
    x <- trait.samps[[.irun]][[satable$id[.jid]]]
    a <- mean(x)
    df <- satable$df[.jid]
    d2f<- satable$d2f[.jid]
    o1 <- var(df*x)#var to first order taylor series expansion 
    o2 <- var(df*x + 0.5 * d2f * (x-a)^2)#var to second order\
    print(satable$id[.jid])
    print(cbind(length(x), o1,o2, o1/o2))

    satable$o1[.jid] <- o1
    satable$o2[.jid] <- o2
  }
  sum.rhs <- sum(satable$o2)
  
  for (.jid in seq(satable$id)) {
    satable$per.var[.jid] <- satable$o2[.jid]/var.f
    satable$rel.var[.jid] <- satable$o2[.jid]/sum.rhs
  }
  var.explained <- sum(satable$per.var)
  satable$null <- rep(0, length(satable$id))
  satable$rel.var[which(satable$rel.var < 0.00001)] <- 0
  return(satable)
}
