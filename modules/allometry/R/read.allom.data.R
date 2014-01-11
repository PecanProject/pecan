#' @param pft.data   PFT dataframe
#' \itemize{
#'   \item{acronym}{USDA species acronym, used with FIELD data}
#'   \item{spcd}{USFS species code, use with TALLY data}
#' }
#' @param component  allometry ID, Jenkins table 5
#' @param field      raw field data
#' @param parm       allometry equation file, Jenkins table 3
#' @return \item{name field}
#'         \item{name parm}
read.allom.data <- function(pft.data, component, field, parm) {

  allom <- list(parm=NULL,field=NULL)
 
  ## make sure some data was found
  #####################################################
  if(is.null(field) & is.null(parm)){
    print("READ.ALLOM.DATA: Neither field data nor tally data was found")
    return(allom)
  }
  
  ## FIELD DATA ------------------------------------------------
  
  if(!is.null(field)){
    allom$field = list()        
    for(i in 1:length(field)){
      
      ## load data
      dat <- read.csv(field[i])
      
      ## grab the response component
      y = switch(as.character(component),
                 '40' = dat$Ht,
                 '43' = dat$Ca
      )
      
      ## if it exists, grab the other columns
      if(!is.null(y)){
        spp = dat$spp
        x = dat$Dia
        entry = data.frame(x,y,spp)
        
        ## match spp to PFT
        spp2pft <- match(dat$spp,pft.data$acronym)
        entry<- entry[!is.na(spp2pft),]
        
        ## insert each species separately
        for(s in unique(entry$spp)){
          sel = which(entry$spp == s)
          allom$field[[length(allom$field)+1]] <- entry[sel,]
        }
      } ## end Y data exists
      
    } ## end loop over FIELD files
  }   ## end field files exist
  
  
  ## EQUATION TALLY --------------------------------------------------------
  
  
  if(!is.null(parm)){
    allom$parm = read.csv(parm,skip=2,as.is=TRUE)

    ## debugging hack
  ##allom$parm <- read.csv("/home/mdietze/stats/AllomAve/Table3_GTR-NE-319.csv",skip=2)
  
    ## Match TALLY data to PFT
    ######################################################
    allompft <- rep(NA,nrow(allom$parm))
    for(i in 1:nrow(allom$parm)){
      sel <- which(pft.data$spcd == allom$parm$Species[i]) ## match on USFS spp code
      if(length(sel) > 0){
        allompft[i] <- pft.data$spcd[sel[1]]
      } 
    }
    
    ## Select the entries that match the component we want
    sel <- which(nu(allom$parm$Component.ID) == component)
    ## Select just the PFT we want
    sel <- sel[!is.na(allompft[sel])]
    
    ## eliminate entries that lack an error estimate
    R2 = apply(rbind(nu(allom$parm$R2[sel]),nu(allom$parm$r[sel])^2),2,max)
    R2[R2 == 0] <- NA
    sel <- sel[!is.na(R2)]
    
    ## check and make sure we have data and can continue
    if(sum(!is.na(allompft)) == 0 | length(sel) == 0){      
      print(c("QUERY.ALLOM.DATA: ** Warning no match of PFT to tally data **",pft_name))
      if(is.null(allom$field)){
        print(c("QUERY.ALLOM.DATA: ** No allometric data available **"))
        return(NULL)
      }
    }
    
    ## extract parameter set
    ########################
    
    ## coefficients
    a <- nu(allom$parm$a[sel])
    b <- nu(allom$parm$b[sel])
    c <- nu(allom$parm$c[sel])
    d <- nu(allom$parm$d[sel])
    e <- nu(allom$parm$e[sel])
    
    ## equation number
    eqn <- nu(allom$parm$Equation.Form.ID[sel])
    
    ## data range for each study
    rng <- rbind(nu(allom$parm$MinDiameter[sel]),nu(allom$parm$MaxDiameter[sel]))  ## units = cm
    rng[1,is.na(rng[1,])] <- ceiling(mean(rng[1,],na.rm=TRUE))
    rng[2,is.na(rng[2,])] <- floor(mean(rng[2,],na.rm=TRUE))
    n <-  nu(allom$parm$Sample.size[sel])  ## sample size for each study
    n[is.na(n)] <- min(n,na.rm=TRUE)
    nt <- sum(n)  ## total sample size
    
    ## data error
    R2 = apply(rbind(nu(allom$parm$R2[sel]),nu(allom$parm$r[sel])^2),2,max)
    R2[R2 == 0] <- NA
    
    ## bias correction factor (multiplicative)
    cf <- nu(allom$parm$Bias.correction..CF.[sel])
    cf[is.na(cf)] <- 1 ## non-log don't get a correction factor
    cf[cf == 0] <- 1 ## non-log don't get a correction factor
    
    ## units corrections
    Xunits <- as.character(allom$parm$Units.diameter[sel])
    Xtype  <- as.character(allom$parm$Diameter[sel])
    Xunits[Xunits == 'cm' & Xtype %in% c("BA","BArc","d150 (BA)")] <- "cm2"
    Xcor   <- AllomUnitCoef(Xunits,Xtype)
    
    Yunits <- as.character(allom$parm$Units.biomass[sel])
    Ycor   <- AllomUnitCoef(Yunits)
    
    ## citations
    cite   <- allom$parm$Source[sel]
    
    ##estimate standard error from the R2
    #####################################
    ##
    ## code assumes a uniform distribution on the X
    ## simulates pseudo data to estimate var(f(X))
    ## solves for var(residual) as a fcn of var(f(X)) and R^2
    ##
    ## for definitions of equations and equation codes
    ## See Jenkins 2004 USFS GTR-NE-319 Table 6
    ##
    se <- 1/12*(rng[2,]-rng[1,])^2   ## start by calculating var(x) for later
    Rratio <- (1-R2)/R2
    for(i in 1:length(sel)){
      
      x = runif(nsim,rng[1,i],rng[2,i])
      if(!is.na(Xcor[i])){
        x = Xcor[i]*x
      } else {
        if(Xtype[i] == "d.b.h.^2"){
          ## convert to sq inches
          x = x*x/(2.54*2.54)
        } else {
          x = x*x*pi/4 ## convert to cm Basal Area
        }
      }
      if(eqn[i] == 1){
        if(b[i] == 0 & c[i] > 0) b[i] = 1
        if(c[i] == 0 & b[i] > 0) c[i] = 1
        y = a[i] + b[i]*c[i]*log10(x)
      } else if(eqn[i] == 2){
        if(is.na(d[i]) | d[i] == 0) d[i] <- 1
        y = a[i] + b[i]*x + c[i]*d[i]*log(x)
      } else if(eqn[i] == 3){
        y = a[i] + b[i]*log(x) + c[i]*(d[i]+(e[i]*log(x)))
      } else if(eqn[i] == 4){
        if(is.na(d[i])) d[i] <- 0
        y = a[i] + b[i]*x + c[i]*x^d[i]
      } else if(eqn[i] == 5){
        y = a[i] + b[i]*x + c[i]*x^2 + d[i]*x^3
      } else if(eqn[i] == 6){
        y = a[i] *(exp( b[i] + (c[i]*log(x)) + d[i]*x))
      } else if(eqn[i] == 7){
        y = a[i] + ((b[i]*(x^c[i]))/((x^c[i])+ d[i]))
      } else if(eqn[i] == 8){
        y = a[i] + b[i]*log10(x)
      }else if(eqn[i] == 9){
        y = log(a[i]) + b[i]*log(x)
      }
      
      se[i] = sqrt(Rratio[i]*var(y))
      ## note: y is not units corrected because SE needs to be
      ## in original units, same as the other parms
    }
    Xmin = rng[1,]
    Xmax = rng[2,]
    
    allomParms <- as.data.frame(cbind(a,b,c,d,e,se,R2,Rratio,cf,eqn,n,Xmin,Xmax,Xcor,Ycor,Xtype,cite))
    
    ## screen TALLY data for nonsensible allometric coefficients
    drop <- NULL
    drop <- c(drop,which(eqn == 1 & b*c <= 0))         #eqn 1, negative slope
    drop <- c(drop,which(eqn == 2 & b == 0 & c*d <= 0))#eqn 2, negative slope
    drop <- c(drop,which(eqn == 4 & b == 0 & c*d < 0)) #eqn 4, negative slope
    drop <- c(drop,which(eqn == 8 & b <= 0))           #eqn 8, negative slope
    drop <- c(drop,which(eqn == 9 & b <= 0))           #eqn 9, negative slope
    
    ## HACK: drop papers known to be suspect until units and coef can be confirmed
    drop <- c(drop, which(cite %in% c(12,103,121)))
    drop <- c(drop, which(cite == 91 & eqn == 4))
    
    ## can't confirm equation for #8, predictions way off, looks like actually #2
    drop <- c(drop,which(eqn == 8))
    
    if(!is.null(drop) & length(drop) > 0){
      print("** WARNING: DROPPING EQUATIONS WITH ILL-DEFINED PARAMETERS")
      print(c("Entry = ",sel[drop]))
      print(allomParms[drop,])
      allomParms <- allomParms[-drop,]
    }
    
  } ## end HAVE ALLOM TALLY DATA
  
  
  return(list(parm = allomParms, field = allom$field))
    
}