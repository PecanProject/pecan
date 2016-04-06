# 1. Global subset
library(data.table)
message("Loading TRY data...")
load("try.RData")     # Loads try.raw ("data.table")
message("Loaded!")

# a. Select only standardized values
# This eliminates any values whose units were not standardized by the TRY curators. It's a pretty strict constraint on the TRY database, but a reasonable one.
message("First subset: Standardized values")
try.sub <- try.raw[!is.na(StdValue)]

#   b. Merge with TRY-BETY translation and select only data that have a match
# This accesses a Google Sheet containing TRY-BETY translation (linked), ensuring that this workflow is always up to date with the latest changes on the sheet.
message("Matching with TRY-BETY translation")
gs.url <- "https://docs.google.com/spreadsheets/d/1bQhwSIw4rwiWMw1O3K_zDH-i0Br0BXYJEgY3wb0weVg/pub?gid=1996728948&single=true&output=csv"
try.bety.info <- fread(gs.url, header=TRUE)
data.in.bety <- try.bety.info[(!is.na(bety_id)) & (bety_id != ""), DataID]
try.sub <- try.sub[DataID %in% c(data.in.bety, 241, 394)]   # 241 -- Measurement Date; 394 -- Measurement Time
keys <- c("DataID", "DataName")
setkeyv(try.sub, keys)
setkeyv(try.bety.info, keys)
try.sub <- try.bety.info[try.sub]

#   c. Select only observation IDs that have at least one trait value -- any(type == "t"), by=ObservationID
message("Subsetting to only trait-containing entities")
setkey(try.sub, ObservationID)
obsid.trait <- try.sub[, has.trait := any(type == "t"), by=ObservationID][has.trait == TRUE, ObservationID]
try.dat <- try.sub[ObservationID %in% obsid.trait]


message("Saving try.dat...")
save(try.dat, file="try.1.RData")
message("Done!")