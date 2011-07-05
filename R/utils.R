#Small, miscellaneous functions for use throughout PECAn

#returns a string representing a given number 
#left padded by zeros up to a given number of digits
left.pad.zeros <- function(num, digits){
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
  units <- query.bety(paste('select name, units from variables where name in (',
                            vecpaste(trait.dictionary(traits)$id),');'))
  ans <- merge(data.frame(name = traits), units, by = 'name', sort =FALSE)
  return(ans)
}
 
pr.samp <- function(distn, parama, paramb, n) {
    do.call(paste('r', distn, sep=""), list(n, parama, paramb))
}
get.sample <- function(prior, n) {
  do.call(paste('r', prior$distn, sep=""), list(n, prior$parama, prior$paramb))
}
