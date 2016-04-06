# 3. Create TRY sites in BETY.

# a. Cast latitudes, longitudes, and data names
# b. Assign each unique lat-lon pair a unique location.id -- := .GRP, by=list(Latitude,Longitude)
# c. Determie sites using cluster analysis, and create unique site.id
# d. Append location.id and site.id to full data set by merging on ObservationID
# e. Create data.table for site creation:
#   sitename = "TRY_SITE_<site.id>"
#   notes = "TRY_DATASETS = <dataset IDs>"
#   geometry = ST_GeomFromText('POINT(lat, lon)', 4263)
# f. Loop over rows. Check site centroid against BETY. Create new sites if necessary. Append "site_id" to full data.table at every step.
#   NOTE: In the future, change centroid to bounding box containing all sites?