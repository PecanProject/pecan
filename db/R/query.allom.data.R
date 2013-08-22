#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------
## Module to grab allometric information from the raw data table
## Will grab both original field data and tallied equations
##
## Tallied equation format based on Jenkins et al 2004 USFS
## General Technical Report NE-319
##        
## Code by Michael Dietze
## University of Illinois, 2011
## mdietze@illinois.edu

query.allom.data <- function(pft_name,variable,con,nsim = 10000){
  ## inputs:
  ##   pft_name: name of Plant Functional Type to be queried
  ##   variable: name of response variable
  ##   con: open database connection
  ##   nsim:  number of pseudo-data simulations for estimating SE  ##
  ##
  ##   database is assumed to conform to the PEcAn Schema


  
  ## check validity of inputs
  if(is.null(pft_name) | is.na(pft_name)){print(c("invalide PFT_NAME in QUERY.ALLOM.DATA",pft_name)); return(NULL)}
  if(length(pft_name) > 1) {print(c("query.allom.data does not currently support multiple simultaneous PFT queries",pft_name)); return(NULL)}
  if(is.null(variable) | is.na(variable)){print(c("invalide VARIABLE in QUERY.ALLOM.DATA",variable)); return(NULL)}
  if(is.null(con)){print("Connection not open in query.allom.data"); return(NULL)}

  ## define storage
  allomParms <- NULL
  
  ## PFTs from trait database
  ##################################################################
  ## used to match species data to functional type
  query <- paste("select s.spcd, p.id as pft,s.commonname as common,s.scientificname as scientific, s.Symbol as acronym, s.genus,s.Family,p.name from pfts as p join pfts_species on p.id = pfts_species.pft_id join species as s on pfts_species.specie_id = s.id where p.name like '%",pft_name,"%'",sep="")
  pft.data <- db.query(query, con)
  if(length(pft.data) < 1){ print(c("QUERY.ALLOM.DATA: No species found for PFT - ",pft_name)); return(NULL)}
                            
  ## Field data from 'Raw' data table
  ####################################################################
  allomField <- NULL
  query <- "select * from raws as r join formats as f on f.id = r.format_id where f.name like 'crownAllom'"
  allom.files <- db.query(query, con)
  if(length(allom.files)>0){
    for(f in allom.files$filepath){

      ## load data
      dat <- read.csv(f)

      ## grab the response variable
      y = switch(as.character(variable),
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
          if(is.null(allomField)){
            allomField <- list();
            allomField[[1]] <- entry[sel,]
          } else {
            allomField[[length(allomField)+1]] <- entry[sel,]
          }
        }
      } ## end Y data exists

    } ## end loop over FIELD files
  }   ## end field fiels exist
  
  
  ## Tally data from 'Raw' data table
  #####################################################################
  ## Species          = FIA code (table 4, also includes sp gravity)
  ## Equation.Form.ID = which equation to use [1-9] (diff from Equation.number [1-5]) (Table 6)
  ## a...e            = equation parameters
  ## Component.ID     = table 5. priorities: Foliar=18,stem=6,16, maybe 4, fine root=28,
  allom <- NULL
  query <- "select * from raws as r join formats as f on f.id = r.format_id where f.name like 'allomTally'"
  allom.files <- db.query(query, con)
  if(length(allom.files)>0){
    for(f in allom.files$filepath){
      if(file.exists(f)){
        allom <- rbind(allom,read.csv(f,skip=2))
      }
    }
  }
  
  ## debugging hack
  ##allom <- read.csv("/home/mdietze/stats/AllomAve/Table3_GTR-NE-319.csv",skip=2)

  ## make sure some data was found
  #####################################################
  if(is.null(allomField) & is.null(allom)){
    print("QUERY.ALLOM.DATA: Neither field data nor talley data was found, please check the database")
    return(NULL)
  }
  
  if(!is.null(allom)){

    ## Match TALLY data to PFT
    ######################################################
    allompft <- rep(NA,nrow(allom))
    for(i in 1:nrow(allom)){
      sel <- which(pft.data$spcd == allom$Species[i]) ## match on USFS spp code
      if(length(sel) > 0){
        allompft[i] <- pft.data$name[sel[1]]
      } 
    }

    ## Select the entries that match the variable we want
    sel <- which(nu(allom$Component.ID) == variable)
    ## Select just the PFT we want
    sel <- sel[!is.na(allompft[sel])]

    ## eliminate entries that lack an error estimate
    R2 = apply(rbind(nu(allom$R2[sel]),nu(allom$r[sel])^2),2,max)
    R2[R2 == 0] <- NA
    sel <- sel[!is.na(R2)]
    
    ## check and make sure we have data and can continue
    if(sum(!is.na(allompft)) == 0 | length(sel) == 0){      
      print(c("QUERY.ALLOM.DATA: ** Warning no match of PFT to tally data **",pft_name))
      if(is.null(allomField)){
        print(c("QUERY.ALLOM.DATA: ** No allometric data available **"))
        return(NULL)
      }
    }
    
    
    ## extract parameter set
    ########################

    ## coefficients
    a <- nu(allom$a[sel])
    b <- nu(allom$b[sel])
    c <- nu(allom$c[sel])
    d <- nu(allom$d[sel])
    e <- nu(allom$e[sel])

    ## equation number
    eqn <- nu(allom$Equation.Form.ID[sel])

    ## data range for each study
    rng <- rbind(nu(allom$MinDiameter[sel]),nu(allom$MaxDiameter[sel]))  ## units = cm
    rng[1,is.na(rng[1,])] <- ceiling(mean(rng[1,],na.rm=TRUE))
    rng[2,is.na(rng[2,])] <- floor(mean(rng[2,],na.rm=TRUE))
    n <-  nu(allom$Sample.size[sel])  ## sample size for each study
    n[is.na(n)] <- min(n,na.rm=TRUE)
    nt <- sum(n)  ## total sample size
    
    ## data error
    R2 = apply(rbind(nu(allom$R2[sel]),nu(allom$r[sel])^2),2,max)
    R2[R2 == 0] <- NA

    ## bias correction factor (multiplicative)
    cf <- nu(allom$Bias.correction..CF.[sel])
    cf[is.na(cf)] <- 1 ## non-log don't get a correction factor
    cf[cf == 0] <- 1 ## non-log don't get a correction factor

    ## units corrections
    Xunits <- as.character(allom$Units.diameter[sel])
    Xtype  <- as.character(allom$Diameter[sel])
    Xunits[Xunits == 'cm' & Xtype %in% c("BA","BArc","d150 (BA)")] <- "cm2"
    Xcor   <- AllomUnitCoef(Xunits,Xtype)
    
    Yunits <- as.character(allom$Units.biomass[sel])
    Ycor   <- AllomUnitCoef(Yunits)

    ## citations
    cite   <- allom$Source[sel]
    
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
            
  return(list(parm = allomParms, field = allomField))

}

nu <- function(x){as.numeric(as.character(x))}

AllomUnitCoef <- function(x,tp=NULL){
  ## converts length units FROM cm
  ## converts mass units TO kg

  y = rep(1,length(x))

  for(i in 1:length(x)){
    y[i] <- switch(x[i],
                   mm = 10,
                   cm = 1,
                   cm2 = NA,
                   m  = 0.01,
                   'in' = 1/2.54,
                   g = 0.001,
                   kg = 1,
                   lb = 0.4545,
                   Mg = 1000                   
                   )
    ## variable type corrections
    if(!is.null(tp)){
      if(tp[i] == 'd.b.h.^2') y[i] <- NA
      if(tp[i] %in% c('cbh','crc')) y[i] <- y[i]*pi
    }
  }  
  return(as.numeric(y))
}
