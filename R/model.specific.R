PREFIX_XML <- '<?xml version="1.0"?>\n<!DOCTYPE config SYSTEM "ed.dtd">\n'

##' Abbreviate run id to ed limits
##'
##' As is the case with ED, input files must be <32 characters long.
##' this function abbreviates run.ids for use in input files
##' @param run.id string indicating nature of the run
abbreviate.run.id.ED <- function(run.id){
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


##' convert parameters from BETY default units to ED defaults
##' 
##' Performs model specific unit conversions on a a list of trait values,
##' such as those provided to write.config
##' @title Convert samples for ed
##' @param trait.samples a matrix or dataframe of samples from the trait distribution
##' @return matrix or dataframe with values transformed
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
    trait.samples[['leaf_width']] <- lw / 1000.0
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

##' Writes an xml and ED2IN config files for use with the Ecological Demography model.
##'
##' Requires a pft xml object, a list of trait values for a single model run,
##' and the name of the file to create
##' @title Write ED configuration files
##' @param pft 
##' @param trait.samples vector of samples for a given trait
##' @param settings list of settings from pecan settings file
##' @param outdir directory for config files to be written to
##' @param run.id id of run
##' @return configuration file and ED2IN namelist for given run
##' @author David
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
  if(any(grep("OUTDIR/OUTFILE", ed2in.text)) &
     length(grep('ebi-cluster', settings$run$host$name)) > 0){
    print(cat("speed up runs by changing \n",
              "NL%FFILOUT = '/OUTDIR/OUTFILE' to NL%FFILOUT = '/scratch/OUTFILE' \n",
              "in ED2IN template as per feature #421"))
  }
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

write.run.ED <- function(settings){
  run.text <- scan(file = paste(settings$pecanDir,
                     'bash/run-template.ED', sep = ''), 
                   what="character",sep='@', quote=NULL, quiet=TRUE)
  run.text <- gsub('OUTDIR', settings$run$host$outdir, run.text)
  run.text  <- gsub('BINARY', settings$run$host$ed$binary, run.text)
  runfile <- paste(settings$outdir, 'run', sep='')
  writeLines(run.text, con = runfile)
  if(settings$run$host$name == 'localhost') {
    system(paste('cp ', runfile, settings$run$host$rundir))
  }else{
    system(paste("rsync -outi ", runfile , ' ', settings$run$host$name, ":",
                 settings$run$host$rundir, sep = ''))
  }
}

##' Extract ED output for specific variables from an hdf5 file
##' @title read output - ED
##' @param filename string, name of file with data
##' @param variables  variables to extract from file
##' @return single value of output variable from filename. In the case of AGB, it is summed across all plants
read.output.file.ed <- function(filename, variables = c("AGB_CO", "NPLANT")){
  if(filename %in% dir(pattern = 'h5')){
    require(hdf5)
    Carbon2Yield = 20
    data <- hdf5load(filename, load = FALSE)[variables]
    if(all(c("AGB_CO", "NPLANT") %in% variables)) {
      return(sum(data$AGB_CO * data$NPLANT, na.rm =TRUE) * Carbon2Yield)
    } else {
      return(sum(data[[variables]]))
    }
  } else {
    return(NA)
  }
}

##' Reads the output of a single model run
##'
##' This function applies \link{\code{read.output.file.ed}} to a list of files from a single run
##' @title Read ED output
##' @param run.id the id distiguishing the model run
##' @param outdir the directory that the model's output was sent to
##' @param start.year 
##' @param end.year
##' @param output.type type of output file to read, can be "-Y-" for annual output, "-M-" for monthly means, "-D-" for daily means, "-T-" for instantaneous fluxes. Output types are set in the ED2IN namelist as NL%I[DMYT]OUTPUT  
##' @return vector of output variable for all runs within ensemble
read.output.ed <- function(run.id, outdir, start.year=NA, end.year=NA, output.type = 'Y'){
  if(any(grep(run.id, dir(pattern = 'finished')))){
    file.names <- dir(outdir, pattern=run.id, full.names=FALSE)
    file.names <- grep(paste('-', output.type, '-', sep = ''), file.names, value = TRUE)
    file.names <- grep('([0-9]{4}).*', file.names, value=TRUE)
    if(length(file.names) > 0) {
      years <- sub('((?!-Y-).)*-Y-([0-9]{4}).*', '\\2', file.names, perl=TRUE)
      if(!is.na(start.year) && nchar(start.year) ==  4){
        file.names <- file.names[years>=as.numeric(start.year)]
      }
      if(!is.na(end.year) && nchar(end.year) == 4){
        file.names <- file.names[years<=as.numeric(end.year)]
      }
      file.names <- file.names[!is.na(file.names)]
      
      result <- mean(sapply(file.names, read.output.file.ed)) ## if any are NA, NA is returned
      result[is.na(result)] <- NULL
    } else {
      warning(cat(paste('no output files in', outdir, '\nfor', run.id, '\n')))
      result <- NA
    }
  } else {
    warning(cat(paste(run.id, 'not finished \n')))
    result <- NA
  }
  return(result)
}

