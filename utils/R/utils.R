#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------#
# Small, miscellaneous functions for use throughout PECAn
#--------------------------------------------------------------------------------------------------#

#--------------------------------------------------------------------------------------------------#
##' return MstMIP variable as ncvar
##'
##' returns a MstMIP variable as a ncvar based on name and other parameters
##' passed in.
##'
##' @title MstMIP variable
##' @export
##' @param name name of variable
##' @param lat latitude if dimension requests it
##' @param lon longitude if dimension requests it
##' @param time time if dimension requests it
##' @param nsoil nsoil if dimension requests it
##' @return ncvar based on MstMIP definition
##' @author Rob Kooper
mstmipvar <- function(name, lat=NA, lon=NA, time=NA, nsoil=NA, silent=FALSE) {
  data(mstmip_vars, package="PEcAn.utils")
  var <- mstmip_vars[mstmip_vars$Variable.Name==name,]
  dims <- list()

  if (nrow(var) == 0) {
    data(mstmip_local, package="PEcAn.utils")
    var <- mstmip_local[mstmip_local$Variable.Name==name,]
    if (nrow(var) == 0) {
      if (!silent) {
        logger.info("Don't know about variable", name, " in mstmip_vars in PEcAn.utils")
      }
      if (is.na(time)) {
        time <- ncdim_def(name="time", units="days since 1900-01-01 00:00:00", vals=1:365, calendar="standard", unlim=TRUE)
      }
      return(ncvar_def(name, "", list(time), -999, name))
    }
  }

  for(i in 1:4) {
    vd <- var[[paste0('dim', i)]]
    if (vd == 'lon' && !is.na(lon)) {
      dims[[length(dims)+1]] <- lon
    } else if (vd == 'lat' && !is.na(lat)) {
      dims[[length(dims)+1]] <- lat
    } else if (vd == 'time' && !is.na(time)) {
      dims[[length(dims)+1]] <- time
    } else if (vd == 'nsoil' && !is.na(nsoil)) {
      dims[[length(dims)+1]] <- nsoil
    } else if (vd == 'na') {
      # skip
    } else {
      if (!silent) {
        logger.info("Don't know dimension for", vd, "for variable", name)
      }
    }
  }
  ncvar <- ncvar_def(name, as.character(var$Units), dims, -999)
  if (var$Long.name != 'na') {
    ncvar$longname <- as.character(var$Long.name)
  }
  return(ncvar)
}


#--------------------------------------------------------------------------------------------------#
##' left padded by zeros up to a given number of digits.
##'
##' returns a string representing a given number 
##' @title Left Pad Zeros
##' @export
##' @param num number to be padded (integer)
##' @param digits number of digits to add
##' @return num with zeros to the left
##' @export
##' @author Carl Davidson
left.pad.zeros <- function(num, digits = 5){
  format_string <- paste('%',sprintf('0%.0f.0f',digits),sep='')
  return(sprintf(format_string, num))
}
#==================================================================================================#

##' Truncates vector at 0
##' @name zero.truncate
##' @title Zero Truncate 
##' @param y numeric vector
##' @return numeric vector with all values less than 0 set to 0
##' @export
##' @author <unknown>
zero.truncate <- function(y) {
  y[y<0 | 
          is.na(y)] <- 0
  return(y)
}
#==================================================================================================#


#--------------------------------------------------------------------------------------------------#
##' R implementation of rsync
##'
##' rsync is a file copying tool in bash
##' @title rsync 
##' @param args rsync arguments (see man rsync)
##' @param from source 
##' @param to destination
##' @param pattern file pattern to be matched 
##' @return nothing, transfers files as a side effect 
##' @export
##' @author David LeBauer
##' @author Shawn Serbin
#--------------------------------------------------------------------------------------------------#
rsync <- function(args, from, to, pattern='') {
  system(paste0('rsync',' ', args,' ', from, pattern, ' ', to), intern=TRUE )
}
#==================================================================================================#


#--------------------------------------------------------------------------------------------------#
##' R implementation of SSH
##'
##' @title SSH
##' @param host 
##' @param ... 
##' @param args 
##' @export
#--------------------------------------------------------------------------------------------------#
ssh <- function(host, ..., args=''){
  if(host == 'localhost'){
    command <- paste(..., args, sep='')
  } else {
    command <- paste('ssh -T ', host, ' "', ..., '" ', args, sep='')
  }
  system(command)
}
#==================================================================================================#


#--------------------------------------------------------------------------------------------------#
##' Convert vector to comma delimited string
##'
##' vecpaste, turns vector into comma delimited string fit for SQL statements.
##' @title vecpaste
##' @param x vector
##' @return comma delimited string
##' @export
vecpaste <- function(x) paste(paste("'", x, "'", sep=''), collapse=',')
#==================================================================================================#


#--------------------------------------------------------------------------------------------------#
##' returns an id representing a model run
##'
##' Provides a consistent method of naming runs; for use in model input files and indices
##' @title Get Run ID
##' @param run.type character, can be any character; currently "SA" is used for sensitivity analysis, "ENS" for ensemble run.
##' @param index unique index for different runs, e.g. integer counting members of an 
##' ensemble or a quantile used to which a trait has been perturbed for sensitivity analysis   
##' @param trait name of trait being sampled (for sensitivity analysis)
##' @param pft.name name of PFT (value from pfts.names field in database)
##' @return id representing a model run
##' @export
##' @examples
##' get.run.id("ENS", left.pad.zeros(1, 5))
##' get.run.id("SA", round(qnorm(-3),3), trait = "Vcmax")
##' @author Carl Davidson, David LeBauer
#--------------------------------------------------------------------------------------------------#
get.run.id <- function(run.type, index, trait = NULL, pft.name = NULL){
  result <- paste(c(run.type, pft.name, trait, index), collapse = "-")
  return(result)
}
#==================================================================================================#


#--------------------------------------------------------------------------------------------------#
##' Convert List to XML
##'
##' Can convert list or other object to an xml object using xmlNode
##' @title List to XML
##' @param item 
##' @param tag xml tag
##' @return xmlNode
##' @export
##' @author David LeBauer, Carl Davidson, Rob Kooper
#--------------------------------------------------------------------------------------------------#
listToXml <- function(item, tag) {
  # just a textnode, or empty node with attributes
  if(typeof(item) != 'list') {
    if (length(item) > 1) {
      xml <- xmlNode(tag)
      for (name in names(item)) {
        xmlAttrs(xml)[[name]] <- item[[name]]
      }
      return(xml)
    } else {
      return(xmlNode(tag, item))
    }
  }

  # create the node
  if (identical(names(item), c("text", ".attrs"))) {
    # special case a node with text and attributes
    xml <- xmlNode(tag, item[['text']])
  } else {
    # node with child nodes
    xml <- xmlNode(tag)
    for(i in 1:length(item)) {
      if (names(item)[i] != ".attrs") {
        xml <- append.xmlNode(xml, listToXml(item[[i]], names(item)[i]))
      }
    }    
  }
  
  # add attributes to node
  attrs <- item[['.attrs']]
  for (name in names(attrs)) {
    xmlAttrs(xml)[[name]] <- attrs[[name]]
  }
  return(xml)
}
#==================================================================================================#




#--------------------------------------------------------------------------------------------------#
##' Zero bounded density using log density transform
##'
##' Provides a zero bounded density estimate of a parameter.
##' Kernel Density Estimation used by the \code{\link{stats::density}} function will cause problems at the left hand end because it will put some weight on negative values. One useful approach is to transform to logs, estimate the density using KDE, and then transform back.
##' @title Zero Bounded Density
##' @param x 
##' @param bw The smoothing bandwidth to be used. See 'bw.nrd'
##' @return data frame with back-transformed log density estimate 
##' @author \href{http://stats.stackexchange.com/q/6588/2750}{Rob Hyndman}
##' @references M. P. Wand, J. S. Marron and D. Ruppert, 1991. Transformations in Density Estimation. Journal of the American Statistical Association. 86(414):343-353 \url{http://www.jstor.org/stable/2290569}
zero.bounded.density <- function (x, bw = "SJ", n = 1001) {
  y <- log(x)
  g <- density(y, bw = bw, n = n)
  xgrid <- exp(g$x)
  g$y <- c(0, g$y/xgrid)
  g$x <- c(0, xgrid)
  return(g)
}
#==================================================================================================#


#--------------------------------------------------------------------------------------------------#
##' Summarize results of replicate observations in trait data query
##'
##' @title Summarize Results
##' @param result dataframe with results of trait data query
##' @return result with replicate observations summarized
##' @export
##' @author David LeBauer
summarize.result <- function(result) {
  ans1 <- ddply(result[result$n==1,],
                .(citation_id, site_id, trt_id, control, greenhouse, date, time, cultivar_id, specie_id),
                summarise,
                n = length(n),
                mean = mean(mean),
                statname = ifelse(length(n)==1,'none','SE'),
                stat = sd(mean)/sqrt(length(n)))
  ans2 <- result[result$n!=1, colnames(ans1)]
  return(rbind(ans1, ans2))
}
#==================================================================================================#


#--------------------------------------------------------------------------------------------------#
##' Further summarizes output from summary.mcmc
##'
##' @title Get stats for parameters in MCMC output
##' @param mcmc.summary 
##' @param sample.size 
##' @return list with summary statistics for parameters in an MCMC chain
##' @author David LeBauer
get.stats.mcmc <- function(mcmc.summary, sample.size){
  a <- list(n = sample.size)
  for (parm in c('beta.o','sd.y', 'sd.site','sd.trt','beta.ghs[2]')){
    parm.name <- ifelse(parm == 'beta.ghs[2]', 'beta.ghs', parm)
    if(parm %in% rownames(mcmc.summary$statistics)){
      a[[parm.name]] <-  get.parameter.stat(mcmc.summary, parameter = parm)
    } else {
      a[[parm.name]] <- NA
    }
  }
  return(unlist(a))
}
#==================================================================================================#


#--------------------------------------------------------------------------------------------------#
##' A helper function for building a LaTex table.
##'
##' Used by \code{\link{get.parameter.stat}}.
##' @title Paste Stats
##' @name paste.stats
##' @param mcmc.summary 
##' @param median 
##' @param lcl 
##' @param ucl 
##' @param n
##' @export
##' @author David LeBauer
paste.stats <- function(mcmc.summary, median, lcl, ucl, n = 2) {  
  paste("$", tabnum(median, n),  "(", tabnum(lcl, n), ",", tabnum(ucl,n), ")", "$", sep = '')
}
#==================================================================================================#


#--------------------------------------------------------------------------------------------------#
##' Gets statistics for LaTeX - formatted table
##'
##' @title Get Parameter Statistics
##' @param mcmc.summary 
##' @param parameter 
##' @return table with parameter statistics
##' @author David LeBauer
##' @export
##' @examples
##' \dontrun{get.parameter.stat(mcmc.summaries[[1]], 'beta.o')}
get.parameter.stat <- function(mcmc.summary, parameter){
  paste.stats(median = mcmc.summary$quantiles[parameter, "50%"],
              lcl   = mcmc.summary$quantiles[parameter, c("2.5%")],
              ucl   = mcmc.summary$quantiles[parameter, c("97.5%")],
              n     = 2)
}
#==================================================================================================#


#--------------------------------------------------------------------------------------------------#
##' Calculate mean, variance statistics, and CI from a known distribution 
##'
##' @title Probability Distirbution Function Statistics
##' @param distn name of distribution used by R (beta, f, gamma, lnorm, norm, weibull) 
##' @param A first parameter 
##' @param B second parameter
##' @return list with mean, variance, and 95 CI
##' @author David LeBauer
## in future, perhaps create S3 functions:
## get.stats.pdf <- pdf.stats
#--------------------------------------------------------------------------------------------------#
pdf.stats <- function(distn, A, B) {
  distn <- as.character(distn)
  mean <- switch(distn,
                 gamma   = A/B,
                 lnorm   = exp(A + 1/2 * B^2),
                 beta    = A/(A+B),
                 weibull = B * gamma(1 + 1/A),
                 norm    = A,
                 f       = ifelse(B>2, B/(B - 2), mean(rf(10000, A, B)))
                 )
  var <- switch(distn,
                gamma   = A/B^2,
                lnorm   = exp(2*A + B^2) * (exp(B^2) - 1),
                beta    =  A*B/((A+B)^2 * (A + B + 1)),
                weibull = B^2 * (gamma(1 + 2 / A) - gamma(1 + 1/A)^2),
                norm    = B^2,
                f       = ifelse(B>4, 2*B^2*(A+B-2) / (A*(B-2)^2*(B-4)), var(rf(100000, A, B)))
                )
  qci  <- get(paste("q", distn, sep = ""))
  ci <- qci(c(0.025, 0.975), A, B)
  lcl <- ci[1]
  ucl <- ci[2]
  out  <- unlist(list(mean = mean, var = var, lcl = lcl, ucl = ucl)) 
  return(out)
}
#==================================================================================================#


#--------------------------------------------------------------------------------------------------#
##' Dictionary of terms used to identify traits in ed, filenames, and figures 
##'
##' @return a dataframe with id, the name used by ED and PEcAn database for a parameter; fileid, an abbreviated  
##'     name used for files; figid, the parameter name written out as best known in english for figures 
##'     and tables.
##'
##' @param traits a vector of trait names, if traits = NULL, all of the traits will be returned.
##' @export
##' @examples
##' # convert parameter name to a string appropriate for end-use plotting
##' \dontrun{
##' trait.lookup('growth_resp_factor')
##' trait.lookup('growth_resp_factor')$figid
##'
##' # get a list of all traits and units in dictionary
##' trait.lookup()[,c('figid', 'units')]
##' }
trait.lookup <- function(traits = NULL) {
  #HACK: shameless hack
  #Ultimately we'll want this to be read once at the start of run time
  #This could also be represented in the database, 
  #but because it is used to determine which parameters to feed to the model,
  #it could be argued that it's conceptually model specific
  data(trait.dictionary)
  if(is.null(traits)) {
    trait.defs <- trait.dictionary
  } else {
    trait.defs <- trait.dictionary[match(traits, trait.dictionary$id),]
  }
  return(trait.defs)
}
#==================================================================================================#


#--------------------------------------------------------------------------------------------------#
##' Convert number to n significant digits
##'
##' @title Table numbers
##' @param x numeric value or vector
##' @param n number of significant figures
##' @export
##' @author David LeBauer
##' @return x rounded to n significant figures
##' @examples
##' tabnum(1.2345)
##' tabnum(1.2345, n = 4)
tabnum <- function(x, n=3) {
  ans <- as.numeric(signif(x,n))
  names(ans) <- names(x)
  return(ans)
}
#==================================================================================================#


#--------------------------------------------------------------------------------------------------#
##' Scale temperature dependent trait from measurement temperature to reference temperature 
##'
##' @title Arrhenius scaling 
##' @param observed.value observed value of temperature dependent trait, e.g. Vcmax, root respiration rate
##' @param old.temp temperature at which measurement was taken or previously scaled to
##' @param new.temp temperature to be scaled to, default = 25 C  
##' @return numeric value at reference temperature
##' @export
##' @author unknown
#--------------------------------------------------------------------------------------------------#
arrhenius.scaling <- function(observed.value, old.temp, new.temp = 25){
  return(observed.value / exp (3000 * ( 1 / (273.15 + new.temp) - 1 / (273.15 + old.temp))))
}
#==================================================================================================#


#--------------------------------------------------------------------------------------------------#
##' Capitalize a string
##'
##' @title Capitalize a string 
##' @param x string
##' @return x, capitalized
##' @author David LeBauer
#--------------------------------------------------------------------------------------------------#
capitalize <- function(x) {
  x <- as.character(x)
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}
#==================================================================================================#

#--------------------------------------------------------------------------------------------------#

isFALSE <- function(x) !isTRUE(x)
#==================================================================================================#




#--------------------------------------------------------------------------------------------------#
##' New xtable
##'
##' utility to properly escape the "%" sign for latex
##' @title newxtable
##' @param x data.frame to be converted to latex table
##' @param environment can be 'table'; 'sidewaystable' if using latex rotating package
##' @param table.placement 
##' @param label 
##' @param caption 
##' @param caption.placement 
##' @param align 
##' @return Latex version of table, with percentages properly formatted 
##' @author David LeBauer
newxtable <- function(x, environment = 'table', table.placement = 'ht',
                      label = NULL, caption = NULL, caption.placement = NULL, align = NULL) {
  print(xtable(x, label = label, caption = caption, align = align),
        floating.environment = environment,
        table.placement = table.placement,
        caption.placement = caption.placement,
#        sanitize.text.function = function(x) gsub("%", "\\\\%", x),
        sanitize.rownames.function = function(x) paste(''))
}
#==================================================================================================#


#--------------------------------------------------------------------------------------------------#
##' Convert author, year, title to bibtex citation format
##'
##' Converts author year title to author1999abc format
##' @title bibtexify
##' @param author name of first author
##' @param year year of publication
##' @param title manuscript title
##' @return bibtex citation
#--------------------------------------------------------------------------------------------------#
bibtexify <- function (author, year, title) {
  acronym <- abbreviate(title, minlength = 3, strict=TRUE)
  paste(author, year, acronym, sep='')
}
#==================================================================================================#


#--------------------------------------------------------------------------------------------------#
##' Convert categorical variable into sequential integers
##'
##' Turns any categorical variable into a sequential integer.
##' This transformation is required for using data in BUGS/JAGS
##' @title as.sequence
##' @param x categorical variable as vector
##' @param na.rm logical: return NA's or replace with max(x) + 1 
##' @return sequence from 1:length(unique(x))
##' @export
##' @author David LeBauer
as.sequence <- function(x, na.rm = TRUE){
  x2 <- as.integer(factor(x, unique(x)))
  if(all(is.na(x2))){
    x2 <- rep(1, length(x2))
  }
  if(na.rm == TRUE){
    x2[is.na(x2)] <- max(x2, na.rm = TRUE) + 1
  }
  return(x2)
}

#--------------------------------------------------------------------------------------------------#
##' Test ssh access
##'
##' Test to determine if access to a remote server is available.
##' Can be used to exclude / include tests or to prevent / identify access errors
##' @title Test Remote
##' @param host 
##' @return logical - TRUE if remote connection is available 
##' @author Rob Kooper
test.remote <- function(host){
  return(try(system(paste("ssh", host, "/bin/true"))) == 0)
}

##' Create a temporary settings file
##'
##' Uses \code{\link{tempfile}} function to provide a valid temporary file (OS independent)
##' Useful for testing functions that depend on settings file
##' Reference: http://stackoverflow.com/a/12940705/199217
##' @title temp.settings
##' @param settings.txt 
##' @return character vector written to and read from a temporary file
##' @export
##' @author David LeBauer
temp.settings <- function(settings.txt){
  temp <- tempfile()
  on.exit(unlink(temp))
  writeLines(settings.txt, con = temp)
  settings <- readLines(temp)
  return(settings)
}


##' Test if function gives an error
##' 
##' adaptation of try that returns a logical value (FALSE if error)
##' @title tryl
##' @param FUN function to be evaluated for error
##' @return FALSE if function returns error; else TRUE
##' @export
##' @examples
##' tryl(1+1)
##' # TRUE
##' tryl(sum("a"))
##' # FALSE
##' @author David LeBauer
tryl <- function(FUN){
  out <- tryCatch(FUN, error = function(e) e)
  ans <- !any(class(out) == "error")
  return(ans)
}

##' load model package
##' @title Load model package
##' @param model name of model
##' @return FALSE if function returns error; else TRUE
##' @export
##' @examples
##' \dontrun{require.modelpkg(BioCro)}
##' @author David LeBauer
load.modelpkg <- function(model){
  pecan.modelpkg <- paste0("PEcAn.", model)
  if(!pecan.modelpkg  %in% names(sessionInfo()$otherPkgs)){
    if(pecan.modelpkg  %in% rownames(installed.packages())) {
      do.call(require, args = list(pecan.modelpkg))
    } else {
      logger.error("I can't find a package for the ", model, "model; I expect it to be named ", pecan.modelpkg)
    }
  }
}
####################################################################################################
### EOF.  End of R script file.              
####################################################################################################
