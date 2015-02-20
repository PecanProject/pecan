##' Load traits into table
library(reshape2)

### Setup paths
PATH.data = "~/Documents/Dropbox/NASA_TE_PEcAn-RTM_Project/Data"
PATH.data.FFT = file.path(PATH.data, "NASA_FFT_Project")
PATH.data.FFT.d15N = file.path(PATH.data.FFT, "NASA_FFT_d15N_ANALYZED_DATA_UPDATED_4R.csv")
PATH.data.FFT.lignin = file.path(PATH.data.FFT, "NASA_FFT_FIB_LIG_CELL_RESULTS_FINAL_4R.csv")
PATH.data.FFT.CN = file.path(PATH.data.FFT, "NASA_FFT_Project_CN_Data_4R.csv")
PATH.data.FFT.SLA_LMA = file.path(PATH.data.FFT, "NASA_FFT_SLA_LMA_Data_v2_4R_updated_new.csv")
PATH.results = "~/Documents/Dropbox/NASA_TE_PEcAn-RTM_Project/Results/FFT no random effects/"
PATH.results.summary = file.path(PATH.results, "fft_summary.csv")

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
mergeby.caps <- c("SITE",
                  "PLOT",
                  "SITE_PLOT",
                  "SPECIES",
                  "HEIGHT",
                  "AGE",
                  "SAMPLE_NAME",
                  "SAMPLE_YEAR")
mergeby.lower <- c("Site",
                   "Plot",
                   "Site_Plot",
                   "Species",
                   "Height",
                   "Age",
                   "Sample_Name",
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
FFT.all <- merge(FFT.p1, FFT.p2, by.x=mergeby.caps, by.y=mergeby.lower, all=TRUE)

#### Load results
#FFT.results <- read.table(PATH.results.summary, header=TRUE)
#FFT.all <- merge(FFT.results, FFT.sp.mean, by="Group.1")
