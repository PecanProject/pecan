# Convert the BADM data to rda for ~30x compression!
#
# BADM = Biological, Ancillary, Disturbance and Metadata;
#	part of the FluxNet protocol.
# Exact provenance of this particular file is not clear, but it contains data
# for 246 siteIDs and includes observations from 120 variable names.
# Not all rows have dates, but those that do range from 1983 to 2018.
# Most sites have one or fewer rows with a date.
#
# TODO:
# * Could probably reduce size further if desired by dropping variables not
#   used by the package
# * Seems to be intended as an internal reference rather than a dataset
#	intended to be exposed to users. Consider moving to sysdata.rda instead?

BADM <- read.csv("BADM.csv")
save(BADM, file = "../data/BADM.rda")
