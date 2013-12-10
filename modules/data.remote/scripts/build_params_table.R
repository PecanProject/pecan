######
##Author Brady S. Hardiman
## 04/25/2013

##path, directory
setwd("~/git/pecan/modules/data.remote/palsar_scenes/Link_to_cheas/uncorrected/ChEAS_PALSAR")

num_folders <-length(as.vector(dir()))
filelist <- as.vector(list.dirs(path=getwd(), recursive=F))

##create metadata file (csv)
#file.create(file.path(outpath, "palsar_metadata.csv"), overwrite=F, showWarnings=T) ##not sure about overwriting (will we need to? Should we?) and this configuration does not seem to generate any actual warnings when overwriting extant files.

header <- c("scnid","scndate", "scnUTMzone", "scnpix","scncf","scn_nwlat","scn_nwlon","scn_nelat","scn_nelon","scn_swlat","scn_swlon","scn_selat","scn_selon","scn_centlat","scn_centlon")
output <- matrix(data=NA, nrow=num_folders+1, ncol=length(header), byrow=T)
output[1,] <-header
outpath <- "/home/bhardima/git/pecan/modules/data.remote/"

for (i in 1:num_folders){
  inpath <-filelist[i]
  
  ##function to extract palsar metadata, input to function is path of directory containing palsar folders. This needs to be able to loop ove all folders in thet directory.
  output[(i+1),] <- extract_palsar_metadata_function(inpath)
}

write.table(output,file="/home/bhardima/git/pecan/modules/data.remote/output/metadata/output_metadata.csv",quote=F,sep="\t",eol="\r\n", row.names=F,col.names=F)





