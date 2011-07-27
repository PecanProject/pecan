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
  command<-paste('ssh -T ', host, ' "', ..., '" ', args, sep='')
  system(command)
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
##' Get units for a trait or set of traits
##'
##' @title Get Units
##' @param traits trait or set of traits
##' @return data.frame with trait names and trait units as provided by BETY
##' @author David LeBauer
get.units <- function(traits) {
  print(traits)
  print(trait.dictionary(traits))
  units <- query.bety(paste('select name, units from variables where name in (',
                            vecpaste(trait.dictionary(traits)$id),');'))
  ans <- merge(data.frame(name = traits), units, by = 'name', sort =FALSE)
  return(ans)
}
 
pr.samp <- function(distn, parama, paramb, n) {
    do.call(paste('r', distn, sep=""), list(n, parama, paramb))
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
##' Update ebi_analysis with information in ebi_production
##'
##' Backs up ebi_analysis, copies ebi_production to ebi_analysis. This function is based on the db_copy.sh script, and is useful 
##' @title 
##' @param tables.to.exclude list of large tables that do not need to be backed up (excluding default values add 1 hr to backup time)
##' @return updated ebi_analysis
##' @author David LeBauer, Patrick Mulrooney
transfer.bety <- function(tables.to.exclude =
                          c('counties', 'county_boundaries',
                            'county_paths', 'location_yields')){
  
  USER="ebi_user"
  PASSWORD="mScGKxhPhdq"
  DB="ebi_analysis"
  backup.dir <- "/home/share/ebi_analysis-backup.sql"
  ignore.tables <- paste(paste(' --ignore-table=', tables.to.exclude, sep = ''), collapse = " ")
  print("Backing up $DB to $DB-backup.sql")
  backup.db <- paste("mysqldump ", DB, ignore.tables, " -u ", USER, " -p", PASSWORD," >> ", backup.dir, sep = '')
  system(backup.db)
  
  print("Dropping tables from $DB")
  drop.tables <- paste("mysqldump ", DB, " -u ", USER, " -p", PASSWORD, ' ', ignore.tables," --add-drop-table --no-data | grep ^DROP | mysql -u ", USER, " -p", PASSWORD, ' ', DB, sep = '')
  system(drop.tables)
  

  print("Transfering tables from ebi_production to $DB")
  transfer.tables <- paste("mysqldump -u", USER, " -p", PASSWORD, ' ', ignore.tables, " ebi_production | mysql -u ", USER, " -p", PASSWORD, ' ', DB, sep = '')
  system(transfer.tables)
                                        #mysqldump -u $DB_USER -p$DB_PASSWORD ebi_production | mysql -u $DB_USER -p$DB_PASSWORD $DB                                                                     
  print("Done!") 
}
