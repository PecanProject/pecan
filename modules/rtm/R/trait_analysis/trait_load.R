##' Load traits into table

library(reshape2)


### Load data in convenient table
load.data <- function(PATH.data = "~/Documents/Dropbox/NASA_TE_PEcAn-RTM_Project/Data",
                      PATH.data.FFT = file.path(PATH.data, "NASA_FFT_Project"),
                      PATH.data.FFT.d15N = file.path(PATH.data.FFT, "NASA_FFT_d15N_ANALYZED_DATA_UPDATED_4R.csv"),
                      PATH.data.FFT.lignin = file.path(PATH.data.FFT, "NASA_FFT_FIB_LIG_CELL_RESULTS_FINAL_4R.csv"),
                      PATH.data.FFT.CN = file.path(PATH.data.FFT, "NASA_FFT_Project_CN_Data_4R.csv"),
                      PATH.data.FFT.SLA_LMA = file.path(PATH.data.FFT, "NASA_FFT_SLA_LMA_Data_v2_4R_updated_new.csv"),
                      PATH.results = "~/Documents/Dropbox/NASA_TE_PEcAn-RTM_Project/Results/FFT no random effects/",
                      PATH.results.summary = file.path(PATH.results, "fft_summary.csv")){
        
        ### Read data
        FFT.d15N <- read.csv(PATH.data.FFT.d15N, header=TRUE)
        FFT.lignin <- read.csv(PATH.data.FFT.lignin, header=TRUE)
        FFT.CN <- read.csv(PATH.data.FFT.CN, header=TRUE)
        FFT.SLA <- read.csv(PATH.data.FFT.SLA_LMA, header=TRUE)
        
        ### Remove bad data
        FFT.d15N <- subset(FFT.d15N, COMMENTS == "")
        FFT.lignin <- subset(FFT.lignin, COMMENTS == "")
        FFT.SLA$LDMC_g_g[FFT.SLA$LDMC_g_g < 0] <- NA
        FFT.SLA$EWT_g_cm2[FFT.SLA$EWT_g_cm2 < 0] <- NA
        FFT.SLA$SLA_cm2_g_DW[FFT.SLA$SLA_cm2_g_DW < 0] <- NA
        FFT.SLA$LMA_g_DW_cm2[FFT.SLA$LMA_g_DW_cm2 < 0] <- NA
        
        ### Species means
        FFT.d15N.sp.mean <- aggregate(FFT.d15N[,"SAMPLE_dN15"],
                                      by=list(FFT.d15N$SPECIES),
                                      FUN=mean)
        FFT.lignin.sp.mean <- aggregate(FFT.lignin[,c("FIB_LIG_RATIO",
                                                      "FIB_CELL_RATIO")],
                                        by=list(FFT.lignin$SPECIES),
                                        FUN=mean)
        FFT.CN.sp.mean <- aggregate(FFT.CN[, c("Perc_N", "Perc_C", "CNRatio")],
                                    by=list(FFT.CN$Species),
                                    FUN=mean)
        FFT.SLA.sp.mean <- aggregate(FFT.SLA[, c("LDMC_g_g",
                                                 "EWT_g_cm2",
                                                 "SLA_cm2_g_DW",
                                                 "LMA_g_DW_cm2")],
                                     by=list(FFT.SLA$Species),
                                     FUN=mean)
        FFT.sp.mean <- merge(FFT.d15N.sp.mean, FFT.lignin.sp.mean, by="Group.1", all=T)
        FFT.sp.mean <- merge(FFT.sp.mean, FFT.CN.sp.mean, by="Group.1", all=T)
        FFT.sp.mean <- merge(FFT.sp.mean, FFT.SLA.sp.mean, by="Group.1", all=T)
        colnames(FFT.sp.mean)[2] <- "d15N"
                
        ### Load results
        FFT.results <- read.table(PATH.results.summary, header=TRUE)
        FFT.all <- merge(FFT.results, FFT.sp.mean, by="Group.1")
        
        return(FFT.all)
}
