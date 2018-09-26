
src <- "/home/mdietze/stats/spectral/pseudo/"
dst <- "/scratch/NACP/spectral/pseudo/"

sfiles <- dir(src, "Rdata")
dfiles <- dir(dst, "Rdata")

for (i in seq_along(sfiles)) {
    print(sfiles[i])
    if (sfiles[i] %in% dfiles) {
        ## check if equal
        sdu <- strsplit(system(paste0("du ", src, sfiles[i]), intern = TRUE), "\t")[[1]]
        rdu <- strsplit(system(paste0("du ", dst, dfiles[i]), intern = TRUE), "\t")[[1]]
        ## delete
        if (sdu[1] == rdu[1]) {
            system(paste0("rm ", src, sfiles[i]))
        } else {
            ## move
            system(paste0("mv ", src, sfiles[i], " ", dst))
        }
    } else {
        ## move
        system(paste0("mv ", src, sfiles[i], " ", dst))
    }
}
