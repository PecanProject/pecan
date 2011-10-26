#Small, miscellaneous functions for use throughout PECAn

#returns a string representing a given number 
#left padded by zeros up to a given number of digits
left.pad.zeros <- function(num, digits = 5){
  format_string <- paste('%',sprintf('0%.0f.0f',digits),sep='')
  return(sprintf(format_string, num))
}
rsync <- function(from, to, pattern=''){
  system(paste('rsync -outi', from, to, sep = ' '), intern=TRUE)
}
ssh <- function(host, ..., args=''){
  if(host == 'localhost'){
    command <- paste(..., args, sep='')
  } else {
    command <- paste('ssh -T ', host, ' "', ..., '" ', args, sep='')
  }
  system(command)
}
sed <- function(find, replace, dirname = getwd(), filename){
  system(paste("sed -i 's/", find, "/", replace, "/g' ", dirname, '/', filename, sep = ''))
}
cp  <- function(option = '', from = getwd(), to = getwd(), oldfilename, newfilename) {
  system(paste('cp', option, paste(from, oldfilename, sep = '/'), paste(to, newfilename, sep = '/')))
}
mkdir <- function(args = '', dir) {
  system(paste('mkdir', args, dir))
}

## vecpaste, turns vector into comma delimited string fit for SQL statements. 
vecpaste <- function(x) paste(paste("'", x, "'", sep=''), collapse=',')

#returns an id representing a model run
#for use in model input files and indices
get.run.id <- function(run.type, index, trait='', pft.name=''){
  run.id <- paste(pft.name, run.type, trait, index, sep='')
  return(abbreviate.run.id.ED(run.id))
}

##' Convert List to XML
##'
##' .. content for \details{} ..
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
##' @author David LeBauer
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
##' @author David LeBauer
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
##' Kernel Density Estimation used by the \code{\link{density}} function will cause problems at the left hand end because it will put some weight on negative values. One useful approach is to transform to logs, estimate the density using KDE, and then transform back.
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

#' Dictionary of terms used to identify traits in ed, filenames, and figures 
#'
#' @return a dataframe with id, the name used by ED and BETY for a parameter; fileid, an abbreviated  
#'     name used for files; figid, the parameter name written out as best known in english for figures 
#'     and tables.
#'
#' @param traits a vector of trait names, if traits = NULL, all of the traits will be returned.
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


#' @examples
#' #translate a parameter name
#' trait.dictionary(c('growth_resp_factor'))
#' trait.dictionary(c('growth_resp_factor'))$figid
#' 
#' #append the names to a dataframe of priors
#' priors <- query.bety("select priors.id, name, phylogeny, distn, parama, paramb, from priors join variables
#'                       on priors.variable_id = variables.id where priors.id in
#'                       (select prior_id from pfts_priors where pft_id = 10);")
#' data.frame(name = trait.dictionary(priors$name)$figid, priors)
#'
#' 

#' do.call(rbind, lapply(traits, .trait.dictionary))
#' 
#' .trait.dictionary<-function(trait)
#' {
#'   defs<-data.frame(id = c("plant_min_temp", "c2n_leaf", "dark_respiration_factor", "f_labile", "growth_resp_factor", "leaf_turnover_rate", "leaf_width", "mort2", "nonlocal_dispersal", "fineroot2leaf", "quantum_efficiency", "root_respiration_rate", "root_turnover_rate", "SLA", "stomatal_slope", "Vcmax", "Vm_low_temp", "water_conductance","cuticular_cond","seedling_mortality","r_fract","storage_turnover_rate", "T"),
#'       figid = c("Plant Minimum Temperature", "Leaf C:N" ,"Dark Respiration Rate", "Litter% Labile C", "Growth Respiration", "Leaf Turnover Rate", "Leaf Width", "Mortality Rate", "Seed Dispersal", "Fine Root Allocation","Quantum Efficiency", "Root Respiration Rate", "Root Turnover Rate", "Specific Leaf Area", "Stomatal Slope", "Vcmax", "Photosynthesis min temp", "Water Conductance","Cuticular Conductance", "Seedling Mortality", "Reproductive Allocation","Storage Turnover Rate","Transpiration")
#'   )
#'   if(trait %in% defs$id) {
#'     return(defs[defs$id == trait,])
#'   }
#'   else{
#'     return(data.frame(id=trait, figid=trait))
#'   }
#' }

##' Identifies experimental replicates and calculates summary statistics.
##'
##' Used after queries in \code{\link{query.bety.trait.data}}
##' @title Summarize Results
##' @param result dataframe of results from query of trait data 
##' @return dataframe with experimental replicates summarized
##' @seealso \code{\link{query.bety.trait.data}}
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
  
