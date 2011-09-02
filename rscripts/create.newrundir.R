library(XML)

if(interactive()){
  Sys.setenv(SETTINGSTEMPLATE='template.settings.ebifarm.pavi.xml')
  Sys.setenv(RUNDIRNAME='test')
}
# load variables
template.settings.filename <- Sys.getenv("SETTINGSTEMPLATE")
rundirname <- Sys.getenv("RUNDIRNAME")

# load template settings file (has everything except rundirname)
template.settings <- xmlToList(xmlParse(paste('edin/', template.settings.filename, sep = '')))

# load utility commands
source(paste(template.settings$pecanDir, 'R/utils.R', sep = ''))


############
# Local Host
############
# name output directory on localhost
outdir <- paste(template.settings$pecanDir, rundirname, '/', sep = '')

## create new run directory
if(!rundirname %in% dir(template.settings$pecanDir)) mkdir('-p', paste(outdir, '/out', sep = ''))

## name settings and ed2in files
settings.filename <- paste('settings.', template.settings$pfts$pft$name, '.xml', sep = '')
ed2in.filename    <- paste('templateED2IN-', template.settings$pfts$pft$name, sep = '')

## move settings file to new rundir
cp(from         = paste(template.settings$pecanDir, 'edin/', sep = ''),
   to           = outdir,
   oldfilename  = template.settings.filename,
   newfilename  = settings.filename)
## find and replace rundirname in settings file (sed -i /RUNDIRNAME/rundirname/ )
sed(find     = 'RUNDIRNAME',
    replace  = rundirname,
    dirname  = outdir,
    filename = settings.filename)
# copy ed2in file over
cp(from         = paste(template.settings$pecanDir, 'edin/', sep = ''),
   to           = outdir,
   oldfilename  = ed2in.filename,
   newfilename  = ed2in.filename)


############################
# Model Inputs and Outputs
############################

settings <- xmlToList(xmlParse(paste(outdir, settings.filename, sep = '')))
# create outdir on model host
system(paste("ssh -T ", settings$run$host$name,
             " '",'mkdir -p ', settings$run$host$outdir,"'",sep=''))

## make symbolic link from met data to model rundirectory

hostrundir <- gsub('out', '', settings$run$host$outdir) #needs revision in settings file
copy.met.command <- paste("cp -lr ", settings$run$host$inputs, settings$run$site$name,
                          ' ', hostrundir , sep = '')
system(paste("ssh -T ", settings$run$host$name, "'", copy.met.command, "'"))
