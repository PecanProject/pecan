### Process all HyspIRI spectra (.sed)

source("sed_proc.R")

hyspiri.path <- "~/Dropbox/NASA_HyspIRI_Project_Spectra/"
sed.list <- readLines("sefilelist.txt")

for (f in sed.list){
        full.path <- paste(hyspiri.path, f, sep='')
        print(full.path)
        flist <- list.files(full.path, full.names=TRUE)
        for (i in flist){
                read.spec(i)
        }
}
