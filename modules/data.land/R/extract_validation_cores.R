# script to read in validation cores:
library(dplR)
cov.join <- read.csv("FIA_inc_data/validation_cores_SDI_plot.csv")
head(cov.join)


unique(cov.join$FILE)

rwls<- list.files(path = "FIA_inc_data/PIPO_validation_cores/rwls/")
all.valid <- paste0(unique(cov.join$FILE), "-A.rwl")
all.valid %in% rwls
validation.rwls <- rwls[rwls %in% all.valid]

full.validation.rwls <- paste0("FIA_inc_data/PIPO_validation_cores/rwls/",validation.rwls)

rwls.list <- lapply(full.validation.rwls, FUN =  read.rwl)


# we want a dataframe with columns for each year 1966:2010, and rows for each individual tree. 
# Have a column or rownames be the tellervo file nam

cut.df <- function(x){
  
    df.rwl <- data.frame(x)
    df.rwl$year <- row.names(df.rwl)
    df.rwl <- df.rwl[df.rwl$year >=1966,]
    if(!1996 %in% df.rwl$year){
      df.rwl[31,] <- c(NA, 1996)
    }
    colnames(df.rwl) <- c("rwl", "year")
    df.rwl
    
}

cut.rwls <- lapply(rwls.list, FUN = cut.df)
all.rwls.long <- do.call(rbind, cut.rwls)

# add an file ID column
all.rwls.long$FILE <- rep(validation.rwls, sapply(cut.rwls , nrow)) # add the tellervo file name

# save to an RDS:
saveRDS(all.rwls.long, "FIA_inc_data/PIPO_validation_cores/validation.rwl.year.df.rds")
