# 2. Data-specific subset
# Deal with datasets that report values that are anything other than "Single"
library(data.table)
load("try.1.RData")

# Identify datasets that contain ValueKinds that are not "Single"
valuekinds <- try.dat[, unique(ValueKindName)]
vk.notsingle <- valuekinds[!(valuekinds %in% c("Single", ""))]
dataset.kinds <- try.dat[, lapply(vk.notsingle, function(x) any(ValueKindName == x)),by=DatasetID]
setnames(dataset.kinds, paste0("V", 1:length(vk.notsingle)), vk.notsingle)
dataset.kinds[, Sum := rowSums(.SD), .SDcols = -1]
print(dataset.kinds[Sum > 0][order(Sum, decreasing=TRUE)])

# Define an additional "keep" column, indicating values that will not be removed
keep.all.ids <- c(59, 60, 241, 394)  # Lat, Lon, Date, Time
try.dat[, keep := FALSE]
try.dat[DataID %in% keep.all.ids, keep := TRUE]
cols.to.check <- c("DataName", "bety_name", "bety_id", "ObservationID", "ValueKindName",
                   "StdValue", "UncertaintyName", "Replicates")
setkey(try.dat, DatasetID)

# Subset based on specific cases (set StdValue to NA, then delete them all in one command)
# Dataset ID 25 -- Reports lots of summary statistics, but inconsistently -- use only "Best Estimate"
tmp.dat <- try.dat[DatasetID == 25][, cols.to.check, with=F]
try.dat[DatasetID == 25 & ValueKindName == "Best Estimate", keep := TRUE]

# Dataset ID 68 -- Plant longevity values are defined by "Low", "High", "Maximum"
# Not sure how to handle these (or if PEcAn can do anything with them), so drop them
# Other data are all single values, so keep them
# tmp.dat <- try.dat[DatasetID == 68][, cols.to.check, with=F][!(ValueKindName %in% c("Single", ""))]
# try.dat[DatasetID == 68 & !(ValueKindName %in% vk.notsingle), keep := TRUE]

# Dataset ID 4 -- Reports Mean, Maximum, and Minimum for traits -- use only the Mean
tmp.dat <- try.dat[DatasetID == 4][, cols.to.check, with=F][, .N, by=list(DataName, ValueKindName)]
try.dat[DatasetID == 4 & ValueKindName == "Mean", keep := TRUE]

# Dataset ID 129 reports species means, but no sample sizes -- assume 1?
tmp.dat <- try.dat[DatasetID == 129][, cols.to.check, with=F]
try.dat[DatasetID == 129, keep := TRUE]

# Dataset ID 159 reports some values as single, others as mean, but only one of each, so leave as is
tmp.dat <- try.dat[DatasetID == 159][, cols.to.check, with=F]
try.dat[DatasetID == 159, keep := TRUE]

# Dataset ID 216 reports means with replicates and uncertainties
tmp.dat <- try.dat[DatasetID == 216][, cols.to.check, with=F]
try.dat[DatasetID == 216, keep := TRUE]

# Dataset ID 263 reports only mean values with no sample sizes -- assume 1
tmp.dat <- try.dat[DatasetID == 263][, cols.to.check, with=F]
try.dat[DatasetID == 263, keep := TRUE]

# Repeat subset on trait-containing observation IDs.
# Have to do this again because this script can result in Lat-Lon pairs that don't correspond to any traits
setkey(try.dat, ObservationID)
obsid.trait <- try.dat[, has.trait := any(type == "t"), by=ObservationID][has.trait == TRUE, ObservationID]
try.dat <- try.dat[ObservationID %in% obsid.trait]

try.dat <- try.dat[keep == TRUE]
save(try.dat, file="try.2.RData")
