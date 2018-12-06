## Open binary file
setwd("/home/bhardima/pecan/modules/data.remote/palsar_scenes/UNDERC/ALPSRP185270910-L1.5")
bin.file=file("LED-ALPSRP185270910-H1.5_UA","rb")

## read the first 1024 bytes as a raw data:
raw<-readBin(bin.file,what="raw",size=1,n=30)

## read 10 integer values
int.values<-readBin( bin.file, integer(),10)

## close file
close(bin.file)