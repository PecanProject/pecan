######
##Author Brady S. Hardiman
## 04/25/2013

##path, directory
setwd("/home/bhardima/pecan/modules/data.remote/palsar_scenes/Link_to_cheas/uncorrected")

num_folders <-length(as.vector(dir()))
filelist <- as.vector(list.dirs(path=getwd(), recursive=F))

##create metadata file (csv)
#file.create(file.path(outpath, "palsar_metadata.csv"), overwrite=F, showWarnings=T) ##not sure about overwriting (will we need to? Should we?) and this configuration does not seem to generate any actual warnings when overwriting extant files.

header <- c("scnid","scndate", "scnUTMzone", "scnpix","scncf","scn_pix2coord_a11","scn_pix2coord_a12","scn_pix2coord_a13","scn_pix2coord_a14","scn_pix2coord_a21","scn_pix2coord_a22","scn_pix2coord_a23","scn_pix2coord_a24","scn_coord2pix_b11","scn_coord2pix_b12","scn_coord2pix_b13","scn_coord2pix_b14","scn_coord2pix_b21","scn_coord2pix_b22","scn_coord2pix_b23","scn_coord2pix_b24","scn_coord2pix_a0","scn_coord2pix_a1","scn_coord2pix_a2","scn_coord2pix_a3","scn_coord2pix_a4","scn_coord2pix_a5","scn_coord2pix_a6","scn_coord2pix_a7","scn_coord2pix_a8","scn_coord2pix_a9","scn_coord2pix_b0","scn_coord2pix_b1","scn_coord2pix_b2","scn_coord2pix_b3","scn_coord2pix_b4","scn_coord2pix_b5","scn_coord2pix_b6","scn_coord2pix_b7","scn_coord2pix_b8","scn_coord2pix_b9","scn_nwlat","scn_nwlon","scn_nelat","scn_nelon","scn_swlat","scn_swlon","scn_selat","scn_selon","scn_centlat","scn_centlon")
output <- matrix(data=NA, nrow=num_folders+1, ncol=length(header), byrow=T)
output[1,] <-header

for (i in 1:num_folders){
  inpath <-filelist[i]
  outpath <- "/home/bhardima/pecan/modules/data.remote/"
  
  ##function to extract palsar metadata, input to function is path of directory containing palsar folders. This needs to be able to loop ove all folders in thet directory.
  output[(i+1),] <- extract_palsar_metadata_function(inpath)
}

write.table(output,file="/home/bhardima/pecan/modules/data.remote/output/metadata/output_metadata.csv",quote=F,sep="\t",eol="\r\n", row.names=F,col.names=F)





