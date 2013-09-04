
src = "/home/mdietze/stats/spectral/pseudo/"
dst = "/scratch/NACP/spectral/pseudo/"

sfiles = dir(src,"Rdata")
dfiles = dir(dst,"Rdata")

for(i in 1:length(sfiles)){
  print(sfiles[i])
  if(sfiles[i] %in% dfiles){
    ##check if equal
    sdu = strsplit(system(paste("du ",src,sfiles[i],sep=""),intern=TRUE),"\t")[[1]]
    rdu = strsplit(system(paste("du ",dst,dfiles[i],sep=""),intern=TRUE),"\t")[[1]]
    ## delete
    if(sdu[1] == rdu[1]){
      system(paste("rm ",src,sfiles[i],sep=""))
    } else {
      ## move
      system(paste("mv ",src,sfiles[i]," ",dst,sep=""))
    }
  } else {
    ## move
    system(paste("mv ",src,sfiles[i]," ",dst,sep=""))
  }
}
