#--------------------------------------------------------------------------------------------------#
# Small, miscellaneous functions for use throughout PECAn
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
##' left padded by zeros up to a given number of digits.
##'
##' returns a string representing a given number 
##' @title Left Pad Zeros
##' @param num number to be padded (integer)
##' @param digits number of digits to add
##' @return num with zeros to the left
##' @author Carl Davidson
left.pad.zeros <- function(num, digits = 5){
  format_string <- paste('%',sprintf('0%.0f.0f',digits),sep='')
  return(sprintf(format_string, num))
}
#==================================================================================================#

##' Truncates vector at 0
##'
##' @title Zero Truncate 
##' @param y numeric vector
##' @return numeric vector with all values less than 0 set to 0
zero.truncate <- function(y) {
  y[y<0 | 
          is.na(y)] <- 0
  return(y)
}

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
#--------------------------------------------------------------------------------------------------#
rsync <- function(args, from, to, pattern=''){
  system(paste('rsync',' ', args,' ', from, pattern, ' ', to, sep = ''), intern=TRUE )
}
#==================================================================================================#


#--------------------------------------------------------------------------------------------------#
##' R implementation of SSH
##'
##' @title SSH
##' @param host 
##' @param ... 
##' @param args 
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
##' ## vecpaste, turns vector into comma delimited string fit for SQL statements.
##' @title vecpaste
##' @param x vector
##' @return comma delimited string
##' @export
#--------------------------------------------------------------------------------------------------#
vecpaste <- function(x) paste(paste("'", x, "'", sep=''), collapse=',')
#==================================================================================================#


#--------------------------------------------------------------------------------------------------#
##' returns an id representing a model run
##'
##' for use in model input files and indices
##' @title Get Run ID
##' @param run.type 
##' @param index 
##' @param trait 
##' @param pft.name 
##' @return id representing a model run
#--------------------------------------------------------------------------------------------------#
get.run.id <- function(run.type, index, trait='', pft.name=''){
  run.id <- paste(pft.name, run.type, trait, index, sep='')
  return(abbreviate.run.id.ED(run.id))
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
#--------------------------------------------------------------------------------------------------#
listToXml <- function(item, tag){
  if(typeof(item)!='list')
    return(xmlNode(tag, item))
  xml <- xmlNode(tag)
  for(name in names(item)){
    xml <- append.xmlNode(xml, listToXml(item[[name]], name))
  }
  return(xml)
}
#==================================================================================================#


#--------------------------------------------------------------------------------------------------#
##' Convert List to XML
##'
##'
##'  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
##'  I THINK THIS IS DEPRECIATED [sps]
##'  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#--------------------------------------------------------------------------------------------------#
list2XML <- function (dat, myName = "config", delim = ",") 
{
    if (!require(XML)) {
        print("Requires XML library")
        return(0)
    }
    if (typeof(dat) != "list") 
        dat <- as.list(dat)
    n <- xmlNode(myName)
    if (length(dat) <= 0) 
        return(n)
    myNodeNames <- names(dat)
    for (i in 1:length(dat)) {
        if (is.null(myNodeNames[i])) 
            myNodeNames[i] <- as.character(i)
        if (typeof(dat[[i]]) == "list") {
            n <- append.xmlNode(n, list2XML(dat[[i]], myNodeNames[i], 
                delim))
        }
        else {
            if (length(dat[[i]]) > 0) {
                val <- as.character(dat[[i]])
                if (length(val) > 1) {
                  vtemp <- val[1]
                  for (j in 2:length(val)) {
                    vtemp <- paste(vtemp, val[j], sep = delim)
                  }
                  val <- vtemp
                }
                n <- append.xmlNode(n, xmlNode(myNodeNames[i], 
                  val))
            }
        }
    }
    n
}
#==================================================================================================#


#--------------------------------------------------------------------------------------------------#
##' Take n random samples from prior
##'
##' @title Sample from prior 
##' @param distn 
##' @param parama 
##' @param paramb 
##' @param n number of samples to return
##' @return vector with n random samples from prior
##' @seealso \link{get.sample}
#--------------------------------------------------------------------------------------------------#
pr.samp <- function(distn, parama, paramb, n) {
    do.call(paste('r', distn, sep=""), list(n, parama, paramb))
}
#==================================================================================================#


#--------------------------------------------------------------------------------------------------#
##' Take n random samples from prior
##'
##' Like pr.samp, with prior as a single input
##' @title Get Samples
##' @param prior data.frame with distn, parama, paramb
##' @param n number of samples to return
##' @return vector with n random samples from prior
##' @seealso \link{pr.samp}
#--------------------------------------------------------------------------------------------------#
get.sample <- function(prior, n) {
  do.call(paste('r', prior$distn, sep=""), list(n, prior$parama, prior$paramb))
}
#==================================================================================================#


#--------------------------------------------------------------------------------------------------#
##' Calculates density at n points across the range of a parameter
##'
##' For a distribution and parameters, return the density for values ranging from alpha to 1-alpha 
##' @title Calculate densities
##' @param distn distribution
##' @param parama parameter
##' @param paramb parameter
##' @param n length of vector to be returned
##' @param alpha sets range at which the distribution will be evaluated (e.g. from alpha to 1-alpha)
##' @return dataframe with equally spaced x values and the corresponding densities
##' @export
##' @author David LeBauer
pr.dens <- function(distn, parama, paramb, n = 1000, alpha = 0.001) {
  alpha <- ifelse(alpha < 0.5, alpha, 1-alpha)
  n <- ifelse(alpha == 0.5, 1, n)
  range.x <- do.call(paste('q', distn, sep = ""), list(c(alpha, 1-alpha), parama, paramb))
  seq.x   <- seq(from = range.x[1], to = range.x[2], length.out = n)
  dens.df <- data.frame(x = seq.x,
                        y = do.call(paste('d', distn, sep=""),
                          list(seq.x, parama, paramb)))
  return(dens.df)
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
zero.bounded.density <- function (x, bw = "SJ") {
  y <- log(x)
  g <- density(y, bw = bw, n = 1001)
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
  ans2 <- result[result$n!=1,which(colnames(result) %in% colnames(ans1))]
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
##' @return a dataframe with id, the name used by ED and BETY for a parameter; fileid, an abbreviated  
##'     name used for files; figid, the parameter name written out as best known in english for figures 
##'     and tables.
##'
##' @param traits a vector of trait names, if traits = NULL, all of the traits will be returned.
##' @export
##' @examples
##' # convert parameter name to a string appropriate for end-use plotting
##' \dontrun{
##' trait.dictionary('growth_resp_factor')
##' trait.dictionary('growth_resp_factor')$figid
##'
##' # get a list of all traits and units in dictionary
##' trait.dictionary()[,c('figid', 'units')]
##' }
trait.dictionary <- function(traits = NULL) {
  #HACK: shameless hack
  #Ultimately we'll want this to be read once at the start of run time
  #This could also be represented in BETY, 
  #but because it is used to determine which parameters to feed to the model,
  #it could be argued that it's conceptually model specific
  data(ed.trait.dictionary)
  if(is.null(traits)) {
    trait.defs <- ed.trait.dictionary
  } else {
    trait.defs <- ed.trait.dictionary[match(traits, ed.trait.dictionary$id),]
  }
  return(trait.defs)
}
#==================================================================================================#


#--------------------------------------------------------------------------------------------------#
##' 

##' Convert number to n significant digits
##'
##' @title Table numbers
##' @param x numeric value or vector
##' @param n number of significant figures
##' @return x rounded to n significant figures
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
##' @author
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
##' Fit a distribution to data
##'
##' @title Fit distribution to data  
##' @param trait.data data for distribution
##' @param dists list of distribution names
##' @return best fit distribution
##' @author David LeBauer
#--------------------------------------------------------------------------------------------------#
fit.dist <- function(trait.data, trait = colnames(trait.data), 
                     dists = c('weibull', 'lognormal', 'gamma'), n = NULL) {
  if(class(trait.data) == 'data.frame') trait.data <- trait.data[,1]
  ## warning(immediate. = TRUE)
  nostart.dists <- dists[dists %in% c('weibull', 'lognormal', 'gamma', 'normal')]
  a <- lapply(nostart.dists, function(x) suppressWarnings(fitdistr(trait.data,x)))
  names(a) <- nostart.dists
  if('f' %in% dists){
    print(trait)
    if(trait == 'tt') {
      a[['f']] <- suppressWarnings(fitdistr(trait.data, 'f', start = list(df1=100, df2=200)))
    } else if (trait == 'sla') {
      a[['f']] <- suppressWarnings(fitdistr(trait.data, 'f', start = list(df1=6, df2=1)))
    } else if(trait == 'rrr') {
      a[['f']] <- suppressWarnings(fitdistr(trait.data, 'f', start = list(df1=6, df2=1)))
    } else if (trait == 'q'){
      a[['f']] <- suppressWarnings(fitdistr(trait.data, 'f', start = list(df1=1, df2=2)))
    }
  }
  if('beta' %in% dists){
    a[['beta']] <- suppressWarnings(fitdistr(trait.data, 'beta', 
                                             start = list(shape1 = 2, shape2 = 1 )))
  }
  aicvalues <- lapply(a, AIC)
  result <- t(sapply(dists, function(x) cbind(t(tabnum(a[[x]]$estimate)), 
                                              signif(aicvalues[[x]]))))
  colnames(result) <- c('a', 'b', 'AIC')
  print(result)
  bestfitdist <- names(which.min(aicvalues))
  parms <- tabnum(a[[bestfitdist]]$estimate)
  dist <- ifelse(bestfitdist == 'normal', 'norm', bestfitdist)
  return(data.frame(distribution = dist, 
                    a = as.numeric(parms[1]), 
                    b = as.numeric(parms[2]), 
                    n = ifelse(is.null(n), length(trait.data), n)))
} 
#==================================================================================================#


#--------------------------------------------------------------------------------------------------#
##' Reads output from model ensemble
##'
##' Reads output for an ensemble of length specified by \code{ensemble.size} and bounded by \code{start.year} and \code{end.year}
##' @title Read ensemble output
##' @return a list of ensemble output 
##' @param ensemble.size 
##' @param outdir 
##' @param pft.name 
##' @param start.year 
##' @param end.year 
##' @param read.output model specific read output function, \cite{\link{read.output.ed}} by default.
#--------------------------------------------------------------------------------------------------#
read.ensemble.output <- function(ensemble.size, outdir, 
                                 start.year, end.year, read.output = read.output.ed){
  ensemble.output <- list()
  for(ensemble.id in 1:ensemble.size) {
    run.id <- get.run.id('ENS', left.pad.zeros(ensemble.id, 5))#log10(ensemble.size)+1))
    if(any(grep('h5',dir()[grep(run.id, dir())]))) {
      ensemble.output[[ensemble.id]] <- read.output(run.id, outdir, start.year, end.year)
    } else {
      ensemble.output[[ensemble.id]] <- NA
    }
  }
  return(ensemble.output)
}
#==================================================================================================#


#--------------------------------------------------------------------------------------------------#
##' Reads output of sensitivity analysis runs
##'
##' 
##' @title Read Sensitivity Analysis output 
##' @return dataframe with one col per quantile analysed and one row per trait,
##'  each cell is a list of AGB over time
##' @param traits 
##' @param quantiles 
##' @param outdir 
##' @param pft.name 
##' @param start.year 
##' @param end.year 
##' @param read.output model specific read.output function
#--------------------------------------------------------------------------------------------------#
read.sa.output <- function(traits, quantiles, outdir, pft.name='', 
                           start.year, end.year, read.output = read.output.ed){
  sa.output <- data.frame()
  for(trait in traits){
    for(quantile in quantiles){
      run.id <- get.run.id('SA', round(quantile,3), trait=trait, pft.name=pft.name)
      print(run.id)
      sa.output[as.character(round(quantile*100,3)), 
                trait] <- read.output(run.id, outdir, start.year, end.year)
    }
  }
  sa.output['50',] <- read.output(get.run.id('SA', 'median'), outdir, start.year, end.year)
  sa.output <- sa.output[order(as.numeric(rownames(sa.output))),]
  return(sa.output)
}
#==================================================================================================#


#--------------------------------------------------------------------------------------------------#
##'
##'
#--------------------------------------------------------------------------------------------------#
isFALSE <- function(x) !isTRUE(x)
#==================================================================================================#


#--------------------------------------------------------------------------------------------------#
##' Prior fitting function for optimization
##'
##' This function is used within \cite{\link{DEoptim}} to parameterize a distribution to the central tendency and confidence interval of a parameter. This function is not very robust; currently it needs to be tweaked when distributions require starting values (e.g. beta, f, 
##' @title prior.fn 
##' @param parms target for optimization
##' @param x vector with c(lcl, ucl, ct) lcl / ucl = confidence limits, ct = entral tendency 
##' @param alpha quantile at which lcl/ucl are estimated (e.g. for a 95% CI, alpha = 0.5)
##' @param distn named distribution, one of 'lnorm', 'gamma', 'weibull', 'beta'; support for other distributions not currently implemented 
##' @param central.tendency one of 'mode', 'median', and 'mean' 
##' @param trait name of trait, can be used for exceptions (currently used for trait == 'q')
##' @return parms
##' @author David LeBauer
##' @examples
##' DEoptim(fn = prior.fn, 
##'                 lower = c(0, 0), 
##'                 upper = c(1000, 1000), 
##'                 x=c(2, 6, 3.3), 
##'                 alpha = 0.05, 
##'                 distn = 'lnorm')$optim$bestmem
#--------------------------------------------------------------------------------------------------#
prior.fn <- function(parms, x, alpha, distn, central.tendency = NULL, trait = NULL) {
  if(!distn %in% c('lnorm', 'gamma', 'weibull', 'beta')){
    stop(paste(distn, "not currently supported by prior.fn"))
  }
  if(distn == 'lnorm') {
    mu <- parms[1]
    sigma <- parms[2]         
    lcl <- mu + qnorm(alpha/2)*sigma
    ucl <- mu + qnorm(1-alpha/2)*sigma
    if(is.null(central.tendency)) {
      ct <- x[3]
    } else if (central.tendency == 'mean'){
      ct <-  mu - sigma^2
    } else if (central.tendency == 'median') {
      ct <- qlnorm(0.5, parms[1], parms[2])
    }
    x <- log(x)
  }
  if(distn == 'gamma'){
    lcl <- qgamma(alpha/2,   parms[1], parms[2])
    ucl <- qgamma(1-alpha/2, parms[1], parms[2])
    if(is.null(central.tendency)) {
      ct <- x[3]
    } else if(central.tendency == 'median'){
      ct <- qgamma(0.5, parms[1], parms[2])
    } else if (central.tendency == 'mean') {
      ct <- parms[1]/parms[2]
    } else if (central.tendency == 'mode') {
      ct <- ifelse (parms[1]>1, (parms[1]-1)/parms[2], 0)
    }
  }
  if(distn == 'weibull'){
    lcl <- qweibull(alpha/2,   parms[1], parms[2])
    ucl <- qweibull(1-alpha/2, parms[1], parms[2])
    if(is.null(central.tendency)) {
      ct <- x[3]
    } else if(central.tendency == 'median'){
      ct <- parms[2] * log(2)^(1/parms[1])
    } else if (central.tendency == 'mean') {
      ct <- parms[2] * gamma(1 +  1 / parms[2])
    } else if (central.tendency == 'mode') {
      stop("mode calculation not currently supported for weibull distribution")
    }
  }
  if (distn == 'beta') {
    a <- parms[1]
    if(central.tendency == 'mean' & trait == 'fineroot2leaf'){ ## fixed mean, optimize for a
      b <- a * (1/x[3] - 1)
    } else {
      b <- parms[2]
    }
    lcl <- qbeta(alpha/2,   a, b)  
    ucl <- qbeta(1-alpha/2, a, b) 
    if(is.null(central.tendency)) {
      ct <- x[3]
    } else if (central.tendency == 'mean'){
      ct <- a/(a+b)
    } else if (central.tendency == 'median'){
      ct <- qbeta(0.5, a, b)  ## median
    } else if (central.tendency == 'mode') {
      ct <- ifelse(a>1 & b>1,(a-1)/(a+b-2) , 0) ## mode
    } 
  } 
  return(sum(abs(c(lcl, ucl, ct) - x)))
}
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
##' Transform misc. statistics to SE
##'
##' Automates transformations of SD, MSE, LSD, 95\%CI, HSD, and MSD to conservative estimates of SE.
##' @title Transform Stats 
##' @param data data frame with mean, statistic, n, and statistic name: \code{example data <- data.frame(Y=rep(1,5), stat=rep(1,5), n=rep(4,5), statname=c('SD', 'MSE', 'LSD', 'HSD', 'MSD'))}
##' @return dataframe with statistics transformed to SE
##' @author David LeBauer
##' @export
##' @examples statdf <- data.frame(Y=rep(1,5),
##'                                stat=rep(1,5),
##'                                n=rep(4,5),
##'                                statname=c('SD', 'MSE', 'LSD', 'HSD', 'MSD'))
##' transformstats(statdf)
transformstats <- function(data) {
  if(!"SE" %in% levels(data$statname)){
    data$statname <- factor(data$statname, levels = c(levels(data$statname), "SE"))
  }
  ## Transformation of stats to SE
  ## transform SD to SE
  if (max(c("SD","sd") %in% data$statname)) {
    sdi <- which(data$statname %in% c("SD","sd"))
    data$stat[sdi] <- data$stat[sdi] / sqrt(data$n[sdi])
    data$statname[sdi] <- "SE"
  }
  ## transform MSE to SE
  if ("MSE" %in% data$statname) {
    msei <- which(data$statname == "MSE")
    data$stat[msei] <- sqrt (data$stat[msei]/data$n[msei])
    data$statname[msei] <- "SE"
  }
  ## 95%CI measured from mean to upper or lower CI
  ## SE = CI/t
  if ("95%CI" %in% data$statname) {
    cii <- which(data$statname == '95%CI')
    data$stat[cii] <- data$stat[cii]/qt(0.975,data$n[cii])
    data$statname[cii] <- "SE"
  }
  ## Fisher's Least Significant Difference (LSD)
  ## conservatively assume no within block replication
  if ("LSD" %in% data$statname) {
    lsdi <- which(data$statname == "LSD")
    data$stat[lsdi] <- data$stat[lsdi] / (qt(0.975,data$n[lsdi]) * sqrt( (2 * data$n[lsdi])))
    data$statname[lsdi] <- "SE"
  }
  ## Tukey's Honestly Significant Difference (HSD),
  ## conservatively assuming 3 groups being tested so df =2
  if ("HSD" %in% data$statname) {
    hsdi <- which(data$statname == "HSD")
    n = data$n[hsdi]
    n[is.na(n)] = 2 ## minimum n that can be used if NA
    data$stat[hsdi] <- data$stat[hsdi] / (qtukey(0.975, n, df = 2))
    data$statname[hsdi] <- "SE"
    data$n[hsdi] <- n
  }              
  ## MSD Minimum Squared Difference
  ## MSD = t_{\alpha/2, 2n-2}*SD*sqrt(2/n)
  ## SE  = MSD*n/(t*sqrt(2))
  if ("MSD" %in% data$statname) {
    msdi <- which(data$statname == "MSD")
    data$stat[msdi] <- data$stat[msdi] * data$n[msdi] / ( qt(0.975,2*data$n[msdi]-2)*sqrt(2))
    data$statname[msdi] <- "SE"
  }
  if (FALSE %in% c('SE','none') %in% data$statname) {
    print(paste(trait, ': ERROR!!! data contains untransformed statistics'))
  }
  return(data)
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
##' @author David LeBauer
#--------------------------------------------------------------------------------------------------#
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
#==================================================================================================#


####################################################################################################
### EOF.  End of R script file.              
####################################################################################################
