library(XML)

settings.template.filename <- Sys.getenv("SETTINGSTEMPLATE")
rundirname <- Sys.getenv("RUNDIRNAME")

settings <- xmlToList(xmlParse(settings.template.filename))

outdir <- paste(settings$pecanDir, rundirname, sep = '')

sed <- function(find, replace, dirname = getwd(), filename){
  system(paste("sed -i 's/", find, "/", replace, "/g' ", dirname, '/', filename, sep = ''))
}
cp  <- function(option = '', from = getwd(), to = getwd(), oldfilename, newfilename) {
  system(paste('cp', option, paste(from, oldfilename, sep = '/'), paste(to, newfilename, sep = '/')))
}
mkdir <- function(dir) {
  system(paste('mkdir', dir))
}

if(!rundirname %in% dir(settings$pecanDir)) mkdir(outdir)
settings.filename <- paste('settings.', settings$pfts$pft$name, '.xml', sep = '')
ed2in.filename    <- paste('templateED2IN-', settings$pfts$pft$name, sep = '')

cp(from         = paste(settings$pecanDir, '/edin/', sep = ''),
   to           = outdir,
   oldfilename  = 'template.settings.xml',
   newfilename  = settings.filename)
sed(find     = 'RUNDIRNAME',
    replace  = rundirname,
    dirname  = outdir,
    filename = settings.filename)
cp(from         = paste(settings$pecanDir, '/edin/', sep = ''),
   to           = outdir,
   oldfilename  = ed2in.filename,
   newfilename  = ed2in.filename)
