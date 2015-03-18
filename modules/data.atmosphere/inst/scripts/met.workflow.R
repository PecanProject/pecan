require(PEcAn.data.atmosphere)
require(PEcAn.ED2)
require(PEcAn.SIPNET)

options(warn=2)

site <- "US-Dk3"
overwrite <- FALSE
verbose <- TRUE

start_date <- as.POSIXlt("2001-01-01 00:00:00", tz = "GMT")
end_date <- as.POSIXlt("2001-12-31 23:59:59", tz = "GMT")

# download data
print(download.Ameriflux(site, "/tmp/met/ameriflux", start_date=start_date, end_date=end_date, overwrite=overwrite))

# convert to CF
print(met2CF.Ameriflux("/tmp/met/ameriflux", site, "/tmp/met/cf", start_date=start_date, end_date=end_date, overwrite=overwrite))

# only execute if regional dataset
#print(permute.nc("/tmp/met/cf", site, "/tmp/met/permute"))
#print(extract.nc("/tmp/met/permute", site, "/tmp/met/extract", 45.2041, -68.7403))

# only execute if site level dataset
print(metgapfill("/tmp/met/cf", site, "/tmp/met/gapfill", start_date=start_date, end_date=end_date, overwrite=overwrite))

# model specific
print(met2model.ED2("/tmp/met/gapfill", site, "/tmp/met/ed", start_date=start_date, end_date=end_date, overwrite=overwrite))
print(met2model.SIPNET("/tmp/met/gapfill", site, "/tmp/met/sipnet", start_date=start_date, end_date=end_date, overwrite=overwrite))

