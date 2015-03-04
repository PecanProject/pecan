##' Load traits into table
library(reshape2)
library(data.table)

### Setup paths
PATH.data = "~/Documents/Dropbox/NASA_TE_PEcAn-RTM_Project/Data"
PATH.data.FFT = file.path(PATH.data, "NASA_FFT_Project")
PATH.data.FFT.d15N = file.path(PATH.data.FFT, "NASA_FFT_d15N_ANALYZED_DATA_UPDATED_4R.csv")
PATH.data.FFT.lignin = file.path(PATH.data.FFT, "NASA_FFT_FIB_LIG_CELL_RESULTS_FINAL_4R.csv")
PATH.data.FFT.CN = file.path(PATH.data.FFT, "NASA_FFT_Project_CN_Data_4R.csv")
PATH.data.FFT.SLA_LMA = file.path(PATH.data.FFT, "NASA_FFT_SLA_LMA_Data_v2_4R_updated_new.csv")
PATH.results = "~/Documents/Dropbox/run_results/results_FFT0203.Rdata"
PATH.spectra = "~/Documents/Dropbox/FFT_spectra/NASA_FFT_LC_Refl_Spectra_v4.csv"

### Read data
FFT.d15N <- read.csv(PATH.data.FFT.d15N, header=TRUE, stringsAsFactors = FALSE)
FFT.lignin <- read.csv(PATH.data.FFT.lignin, header=TRUE, stringsAsFactors = FALSE)
FFT.CN <- read.csv(PATH.data.FFT.CN, header=TRUE, stringsAsFactors = FALSE)
FFT.SLA <- read.csv(PATH.data.FFT.SLA_LMA, header=TRUE, stringsAsFactors = FALSE)


### Remove bad data
FFT.d15N <- subset(FFT.d15N, COMMENTS == "")
FFT.lignin <- subset(FFT.lignin, COMMENTS == "")
FFT.SLA$EWT_g_cm2[FFT.SLA$EWT_g_cm2 < 0] <- NA
FFT.SLA$LMA_g_DW_cm2[FFT.SLA$LMA_g_DW_cm2 < 0] <- NA

### Subset necessary columns
mergeby.caps <- c("SAMPLE_NAME",
                  "SAMPLE_YEAR")
mergeby.lower <- c("Sample_Name",
                   "Sample_Year")

FFT.d15N.sub <- FFT.d15N[,c(mergeby.caps,
                            "SAMPLE_dN15")]
FFT.lignin.sub <- FFT.lignin[,c(mergeby.caps,
                                "ADF_PERC_DW",
                                "ADL_PERC_DW",
                                "CELL_PERC_DW")]
FFT.CN.sub <- FFT.CN[, c(mergeby.lower,
                         "Perc_N",
                         "Perc_C",
                         "CNRatio")]
FFT.SLA.sub <- FFT.SLA[, c(mergeby.lower,
                           "EWT_g_cm2",
                           "LMA_g_DW_cm2")]

### Merge into large data file
FFT.p1 <- merge(FFT.d15N.sub, FFT.lignin.sub, by=mergeby.caps, all=TRUE)
FFT.p2 <- merge(FFT.CN.sub, FFT.SLA.sub, by=mergeby.lower, all=TRUE)
FFT.all <- merge(FFT.p2, FFT.p1, by.x=mergeby.lower, by.y=mergeby.caps, all=TRUE)
FFT.all <- data.table(FFT.all)

#### Load results
## Append information from original spectra to results table
load(PATH.results)
setnames(results, "leaf", "Spectra")
info.orig <- fread(PATH.spectra, header=TRUE, select = c(1:9,12,13))
dup <- info.orig[,.N, by="Spectra"][N>1, Spectra]
setkey(info.orig, Spectra)
info.orig[dup, Spectra := sprintf("%s_%d", Spectra, Sample_Year)]
results <- merge(info.orig, results, by="Spectra")

## Average duplicates
FFT2 <- FFT.all[, lapply(.SD, mean, na.rm=TRUE), by=c("Sample_Name", "Sample_Year")]

## Merge results with traits
fftdat <- merge(x=results, y=FFT2, by=mergeby.lower, all=TRUE)


