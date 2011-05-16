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

#returns an id representing a model run
#for use in model input files and indices
get.run.id <- function(run.type, index, trait='', pft.name=''){
  run.id <- paste(pft.name, run.type, trait, index, sep='')
  return(abbreviate.run.id.ED(run.id))
}
get.run.time <- function(){
  format(Sys.time(), '%Y.%m.%d')
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
