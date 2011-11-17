# Small, miscellaneous functions for use throughout PECAn

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
##' R implementation of rsync
##'
##' rsync is a file copying tool in bash
##' @title rsync 
##' @param from source 
##' @param to destination
##' @param pattern file pattern to be matched 
##' @return nothing, transfers files as a side effect 
rsync <- function(from, to, pattern=''){
  system(paste('rsync -outi', from, to, sep = ' '))
}
##' R implementation of SSH
##'
##' @title SSH
##' @param host 
##' @param ... 
##' @param args 
ssh <- function(host, ..., args=''){
  if(host == 'localhost'){
    command <- paste(..., args, sep='')
  } else {
    command <- paste('ssh -T ', host, ' "', ..., '" ', args, sep='')
  }
  system(command)
}


##' Convert vector to comma delimited string
##'
##' ## vecpaste, turns vector into comma delimited string fit for SQL statements.
##' @title vecpaste
##' @param x vector
##' @return comma delimited string
vecpaste <- function(x) paste(paste("'", x, "'", sep=''), collapse=',')

##' returns an id representing a model run
##'
##' for use in model input files and indices
##' @title Get Run ID
##' @param run.type 
##' @param index 
##' @param trait 
##' @param pft.name 
##' @return id representing a model run
get.run.id <- function(run.type, index, trait='', pft.name=''){
  run.id <- paste(pft.name, run.type, trait, index, sep='')
  return(abbreviate.run.id.ED(run.id))
}

##' Convert List to XML
##'
##' Can convert list or other object to an xml object using xmlNode
##' @title List to XML
##' @param item 
##' @param tag xml tag
##' @return xmlNode
listToXml <- function(item, tag){
  if(typeof(item)!='list')
    return(xmlNode(tag, item))
  xml <- xmlNode(tag)
  for(name in names(item)){
    xml <- append.xmlNode(xml, listToXml(item[[name]], name))
  }
  return(xml)
}
 
##' Take n random samples from prior
##'
##' @title Sample from prior 
##' @param distn 
##' @param parama 
##' @param paramb 
##' @param n number of samples to return
##' @return vector with n random samples from prior
##' @seealso \link{get.sample}
pr.samp <- function(distn, parama, paramb, n) {
    do.call(paste('r', distn, sep=""), list(n, parama, paramb))
}


##' Take n random samples from prior
##'
##' Like pr.samp, with prior as a single input
##' @title Get Samples
##' @param prior data.frame with distn, parama, paramb
##' @param n number of samples to return
##' @return vector with n random samples from prior
##' @seealso \link{pr.samp}
get.sample <- function(prior, n) {
  do.call(paste('r', prior$distn, sep=""), list(n, prior$parama, prior$paramb))
}

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
pr.dens <- function(distn, parama, paramb, n = 1000, alpha = 0.0001) {
  alpha <- ifelse(alpha < 0.5, alpha, 1-alpha)
  n <- ifelse(alpha == 0.5, 1, n)
  range.x <- do.call(paste('q', distn, sep = ""), list(c(alpha, 1-alpha), parama, paramb))
  seq.x   <- seq(from = range.x[1], to = range.x[2], length.out = n)
  dens.df <- data.frame(x = seq.x,
                        y = do.call(paste('d', distn, sep=""), list(seq.x, parama, paramb)))
  return(dens.df)
}


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




##' Summarize results of replicate observations in trait data query
##'
##' @title Summarize Results
##' @param result dataframe with results of trait data query
##' @return result with replicate observations summarized 
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

##' Calculate mean, variance statistics, and CI from a known distribution 
##'
##' @title Probability Distirbution Function Statistics
##' @param distn name of distribution used by R (beta, f, gamma, lnorm, norm, weibull) 
##' @param A first parameter 
##' @param B second parameter
##' @return list with mean, variance, and 95 CI
##' @author David LeBauer
pdf.stats <- function(distn, A, B) {
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

##' Dictionary of terms used to identify traits in ed, filenames, and figures 
##'
##' @return a dataframe with id, the name used by ED and BETY for a parameter; fileid, an abbreviated  
##'     name used for files; figid, the parameter name written out as best known in english for figures 
##'     and tables.
##'
##' @param traits a vector of trait names, if traits = NULL, all of the traits will be returned.
trait.dictionary <- function(traits = NULL) {
  defs<-data.frame(id = c("plant_min_temp", "c2n_leaf", "dark_respiration_factor", "f_labile", "growth_resp_factor", "leaf_turnover_rate", "leaf_width", "mort2", "nonlocal_dispersal", "fineroot2leaf", "quantum_efficiency", "root_respiration_rate", "root_turnover_rate", "SLA", "stomatal_slope", "Vcmax", "Vm_low_temp", "water_conductance","cuticular_cond","seedling_mortality","r_fract","storage_turnover_rate", "T", "agf_bs"),
                   figid = c("Plant Minimum Temperature", "Leaf C:N" ,"Dark Respiration Rate", "Litter% Labile C", "Growth Respiration", "Leaf Turnover Rate", "Leaf Width", "Mortality Rate", "Seed Dispersal", "Fine Root Allocation","Quantum Efficiency", "Root Respiration Rate", "Root Turnover Rate", "Specific Leaf Area", "Stomatal Slope", "Vcmax", "Photosynthesis min temp", "Water Conductance","Cuticular Conductance", "Seedling Mortality", "Reproductive Allocation","Storage Turnover Rate","Transpiration", "Abovground fraction of structural biomass"),
                   units = c("Celsius ", "ratio ", "fraction", "fraction", "fraction", "yr-1 ", "mm", "yr-1 ", "fraction", "ratio", "fraction", "umol CO2 kg-1 s-1", "yr-1 ", "m2 kg-1", "ratio ", "umol CO2 m-2 s-1", "Celsius ", "mm", "umol H2O m-2 s-1", "fraction", "fraction", "yr-1 ", "mm  H2O m-1s-1", "fraction")
)
  if(is.null(traits)) {
    trait.defs <- defs
  } else {
    trait.defs <- defs[defs$id %in% traits,]
  }
  return(trait.defs)
}
##' @examples
##' # convert parameter name to a string appropriate for end-use plotting 
##' trait.dictionary('growth_resp_factor')
##' trait.dictionary('growth_resp_factor')$figid
##'
##' # get a list of all traits and units in dictionary
##' trait.dictionary()[,c('figid', 'units')]
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

##' Scale temperature dependent trait from measurement temperature to reference temperature 
##'
##' @title Arrhenius scaling 
##' @param observed.value observed value of temperature dependent trait, e.g. Vcmax, root respiration rate
##' @param old.temp temperature at which measurement was taken or previously scaled to
##' @param new.temp temperature to be scaled to, default = 25 C  
##' @return numeric value at reference temperature
arrhenius.scaling <- function(observed.value, old.temp, new.temp = 25){
  return(observed.value / exp (3000 * ( 1 / (273.15 + new.temp) - 1 / (273.15 + old.temp))))
}

##' Capitalize a string
##'
##' @title Capitalize a string 
##' @param x string
##' @return x, capitalized
##' @author David LeBauer
capitalize <- function(x) {
  x <- as.character(x)
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

##' Calculate the density of a distribution for use in plotting
##'
##' @title Prior Density 
##' @param distribution one of R's supported distributions (character)
##' @param a first parameter of distribution (numeric)
##' @param b second parameter of distribution (numeric)
##' @return data frame with values of x, the density at each x and the probability at each x
##' @author David LeBauer
prior.density <- function(distribution = 'norm', a = 0, b = 1){
  distribution <- gsub('lognormal', 'lnorm', distribution)
  if(distribution != 'beta'){
    range.x <- range(pretty(do.call(paste('q', distribution, sep = ''), list(c(0.005, 0.995),a,b))))
    prior.x <- seq(from=range.x[1], to = range.x[2], length = 1000)  
  } else {
    range.x <- c(0,1)
    prior.x <- seq(from=0, to = 1, length = 1000)  
  }
  dens.x  <- do.call(paste('d', distribution, sep=''),list(prior.x, a, b))
  prob.x  <- do.call(paste('p', distribution, sep=''),list(prior.x, a, b))
  return(data.frame(prior.x, dens.x, prob.x))
}

##' Plot prior density and data
##'
##' @title Prior Figure 
##' @param priordata observations to be plotted as points
##' @param priordensity density of prior distribution, calculated by \code{\link{prior.density}}
##' @param trait name of trait
##' @param xlim limits for x axis
##' @return plot / grob of prior distribution with data used to inform the distribution 
priorfig <- function(priordata = 'n', priordensity = 'n', trait = '', xlim = 'auto'){
  x.breaks <- pretty(priordensity$prior.x, 4)
  xlim <- range(priordensity$prior.x)
  priorfigure <- ggplot() + theme_bw() + 
    scale_x_continuous(limits = xlim, breaks = x.breaks, trait.dictionary(trait)$units) +
    opts(title = trait.dictionary(trait)$figid,
         panel.grid.major = theme_blank(),    
         panel.grid.minor = theme_blank(),
         axis.text.y = theme_blank(),
         axis.text.x = theme_text(size=12)
     ) 
  

  if(is.data.frame(priordata)){
    rug <- geom_point(data = priordata, aes(x=x, y = 0), size = 4, alpha = 2/sqrt(nrow(priordata)))
    #hist <-  geom_histogram(data = priordata, aes(x=x, y = ..density..),  alpha = 0.5, binwidth = diff(range(priordata))/sqrt(nrow(priordata)))
    priorfigure <- priorfigure + rug
  } 
  if(is.data.frame(priordensity[1])){
    dens <- geom_line(data=priordensity, aes(x=prior.x, y=dens.x))
    priorfigure <- priorfigure + dens
  } 
  return(priorfigure)
} 

##' Fit a distribution to data
##'
##' @title Fit distribution to data  
##' @param trait.data data for distribution
##' @param dists list of distribution names
##' @return best fit distribution
##' @author David LeBauer
fit.dist <- function(trait.data, dists = c('weibull', 'lognormal', 'gamma'), n = NULL) {
  warning(immediate. = TRUE)
  nostart.dists <- dists[dists %in% c('weibull', 'lognormal', 'gamma')]
  a <- lapply(nostart.dists, function(x) suppressWarnings(fitdistr(trait.data[,1],x)))
  trait <- colnames(trait.data)
  names(a) <- nostart.dists
  if('f' %in% dists){
    if(trait == 'tt') {
      a[['f']] <- suppressWarnings(fitdistr(trait.data[,1], 'f', start = list(df1=100, df2=2)))
    } else if (trait == 'sla') {
      a[['f']] <- suppressWarnings(fitdistr(trait.data[,1], 'f', start = list(df1=6, df2=1 )))
    } else if(trait == 'rrr') {
      a[['f']] <- suppressWarnings(fitdistr(trait.data[,1], 'f', start = list(df1=6, df2=1 )))
    }
  }
  if('beta' %in% dists){
    a[['beta']] <- suppressWarnings(fitdistr(trait.data[,1], 'beta', start = list(shape1 = 2, shape2 = 1 )))
  }
  aicvalues <- lapply(a, AIC)
  bestfitdist <- names(which.min(aicvalues))
  parms <- tabnum(a[[bestfitdist]]$estimate)
  return(data.frame(distribution = bestfitdist, 
                    a = as.numeric(parms[1]), 
                    b = as.numeric(parms[2]), 
                    n = ifelse(is.null(n), nrow(trait.data), n)))
} 

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
read.ensemble.output <- function(ensemble.size, outdir, pft.name='', 
                                 start.year, end.year, read.output = read.output.ed){
  ensemble.output <- list()
  for(ensemble.id in 1:ensemble.size) {
    run.id <- get.run.id('ENS', left.pad.zeros(ensemble.id, 5), pft.name=pft.name)#log10(ensemble.size)+1))
    if(any(grep('h5',dir()[grep(run.id, dir())]))) {
      ensemble.output[[ensemble.id]] <- read.output(run.id, outdir, start.year, end.year)
    } else {
      ensemble.output[[ensemble.id]] <- NA
    }
  }
  return(ensemble.output)
}

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
read.sa.output <- function(traits, quantiles, outdir, pft.name='', 
                           start.year, end.year, read.output = read.output.ed){
  sa.output <- data.frame()
  for(trait in traits){
    for(quantile in quantiles){
      run.id <- get.run.id('SA', round(quantile,3), trait=trait, pft.name=pft.name)
      print(run.id)
      sa.output[as.character(round(quantile*100,3)), trait] <- read.output(run.id, outdir, start.year, end.year)
    }
  }
  sa.output['50',] <- read.output(get.run.id('SA', 'median'), outdir, start.year, end.year)
  sa.output <- sa.output[order(as.numeric(rownames(sa.output))),]
  return(sa.output)
}
