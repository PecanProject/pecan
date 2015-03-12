### Exploration of raw results
library(data.table)
load("~/Documents/Unsynced/pecan/modules/rtm/run_results/samples_FFT0203.Rdata")
all.bind2 <- lapply(1:1382, function(l) data.table(do.call(rbind,
                                                          all.samples[[l]]),
                                                  spectra = names(all.samples)[l]))
all.bind <- data.table(do.call(rbind, all.bind2))
rm(all.bind2, all.samples)

PATH.spectra = "~/Documents/Dropbox/FFT_spectra/NASA_FFT_LC_Refl_Spectra_v4.csv"
info.orig <- fread(PATH.spectra, header=TRUE, select = c(1:9,12,13))
setkey(info.orig, Spectra)
setkey(all.bind, spectra)
samples <- all.bind[info.orig]

PATH.speciesinfo <- file.path("~/Documents", "Dropbox",
                              "NASA_TE_PEcAn-RTM_Project",
                              "FFT_species_info_csv.csv")
species.info <- fread(PATH.speciesinfo, header=TRUE)
species.info[,Scientific := sprintf("%s %s", Genus, Species)]
species.info[, Species := NULL]

setkey(species.info, Label)
setnames(samples, "Species", "Label")
setkey(samples, Label)
samples <- samples[species.info]

write.csv(samples, "~/Documents/Unsynced/pecan/modules/rtm/data/samples.dat")
# save(samples, file="~/Documents/Unsynced/pecan/modules/rtm/data/samples.Rdata")
