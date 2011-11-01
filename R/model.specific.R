PREFIX_XML <- '<?xml version="1.0"?>\n<!DOCTYPE config SYSTEM "ed.dtd">\n'

#As is the case with ED, input files must be <32 characters long.
#this function abbreviates run.ids for use in input files
##TODO fix this filename restriction bug in ED, remove abbreviate.run.id.ED 
abbreviate.run.id.ED <- function(run.id){
  #TODO: remove references to specific pft names and use outdir
  run.id <- gsub('tundra.', '', run.id)
  run.id <- gsub('ebifarm.', '', run.id)
  run.id <- gsub('deciduous', 'decid', run.id)
  run.id <- gsub('evergreen', 'everg', run.id)
  run.id <- gsub('_', '', run.id)
  run.id <- gsub('root', 'rt', run.id)
  run.id <- gsub('water', 'h2o', run.id)
  run.id <- gsub('factor', '', run.id)
  run.id <- gsub('turnover', 'tnvr', run.id)
  run.id <- gsub('mortality', 'mort', run.id)
  run.id <- gsub('conductance', 'cond', run.id)
  run.id <- gsub('respiration', 'resp', run.id)
  run.id <- gsub('stomatalslope', 'stmslope', run.id)
  run.id <- gsub('nonlocaldispersal', 'nldisprs', run.id)
  run.id <- gsub('quantumefficiency', 'quantef', run.id)
  
  return(run.id)
} 
#Performs model specific unit conversions on a a list of trait values,
#such as those provided to write.config
convert.samples.ED <- function(trait.samples){
  DEFAULT.LEAF.C <- 0.48
  DEFAULT.MAINTENANCE.RESPIRATION <- 1/2
  ## convert SLA from m2 / kg leaf to m2 / kg C 
    
  if('SLA' %in% names(trait.samples)){
    sla <- trait.samples[['SLA']]
    trait.samples[['SLA']] <- sla / DEFAULT.LEAF.C
  }
  
  ## convert leaf width / 1000
  if('leaf_width' %in% names(trait.samples)){
    lw <- trait.samples[['leaf_width']]
    trait.samples[['leaf_width']] / 1000.0
  }
  
  if('root_respiration_rate' %in% names(trait.samples)) {
    rrr1 <- trait.samples[['root_respiration_rate']]
    rrr2 <-  rrr1 * DEFAULT.MAINTENANCE.RESPIRATION
    trait.samples[['root_respiration_rate']] <- arrhenius.scaling(rrr2, old.temp = 25, new.temp = 15)
    names(trait.samples)[names(trait.samples)=='root_respiration_rate'] <- 'root_respiration_factor'
  }
  
  if('Vcmax' %in% names(trait.samples)) {
    vcmax <- trait.samples[['Vcmax']]
    trait.samples[['Vcmax']] <- arrhenius.scaling(vcmax, old.temp = 25, new.temp = 15)
    names(trait.samples)[names(trait.samples) == 'Vcmax'] <- 'Vm0'
  }

  if('fineroot2leaf' %in% names(trait.samples)){
    names(trait.samples)[names(trait.samples) == 'fineroot2leaf'] <- 'q'
  }
  return(trait.samples)
}
#Writes an xml and ED2IN config files for use with the Ecological Demography model.
#Requires a pft xml object, a list of trait values for a single model run,
#and the name of the file to create
write.config.ED <- function(pft, trait.samples, settings, outdir, run.id){
  xml <- listToXml(pft$constants, 'pft')
  for (trait in names(trait.samples)) {
    xml <- append.xmlNode(xml, xmlNode(trait, trait.samples[trait]))
  }
  config.header <- xmlNode("config")
  if ('config.header' %in% names(settings)){
    config.header <- listToXml(settings$config.header, 'config')
  } 
  xml <- append.xmlNode(config.header, xml)
  #c stands for config, abbreviated to work within ED's character limit
  xml.file.name <-paste('c.',run.id,sep='')  
  if(nchar(xml.file.name) >= 32) 
    stop(paste('The file name, "',xml.file.name,
            '" is too long and will cause your ED run to crash ',
            'if allowed to continue. '))
  saveXML(xml, file = paste(outdir, xml.file.name, sep=''), 
      indent=TRUE, prefix = PREFIX_XML)
  
  startdate <- as.Date(settings$run$start.date)
  enddate <- as.Date(settings$run$end.date)
  ed2in.text <- scan(file = pft$edin, 
      what="character",sep='@', quote=NULL, quiet=TRUE)
  ed2in.text <- gsub('START_MONTH', format(startdate, "%m"), ed2in.text)
  ed2in.text <- gsub('START_DAY', format(startdate, "%d"), ed2in.text)
  ed2in.text <- gsub('START_YEAR', format(startdate, "%Y"), ed2in.text)
  ed2in.text <- gsub('END_MONTH', format(enddate, "%m"), ed2in.text)
  ed2in.text <- gsub('END_DAY', format(enddate, "%d"), ed2in.text)
  ed2in.text <- gsub('END_YEAR', format(enddate, "%Y"), ed2in.text)
  ed2in.text <- gsub('OUTDIR', settings$run$host$outdir, ed2in.text)
  ed2in.text <- gsub('ENSNAME', run.id, ed2in.text)
  ed2in.text <- gsub('CONFIGFILE', xml.file.name, ed2in.text)
  ed2in.text <- gsub('OUTFILE', paste('out', run.id, sep=''), ed2in.text)
  ed2in.text <- gsub('HISTFILE', paste('hist', run.id, sep=''), ed2in.text)
  ed2in.file.name <- paste('ED2INc.',run.id, sep='')
  writeLines(ed2in.text, con = paste(outdir, ed2in.file.name, sep=''))
  
  print(run.id)
}
