require(PEcAn.data.atmosphere)
require(PEcAn.ED2)
require(PEcAn.SIPNET)

site <- "US-Dk3"
overwrite <- FALSE
verbose <- FALSE

# download data
print(download.Ameriflux(2000, 2001, site, "/tmp/met/ameriflux", overwrite=overwrite))

# convert to CF
print(met2CF.Ameriflux(2000, 2001, "/tmp/met/ameriflux", site, "/tmp/met/cf", overwrite=overwrite, verbose=verbose))

# only execute if regional dataset
#print(permute.nc("/tmp/met/cf", site, "/tmp/met/permute"))
#print(extract.nc("/tmp/met/permute", site, "/tmp/met/extract", 45.2041, -68.7403))

# only execute if site level dataset
print(metgapfill(2000, 2001, "/tmp/met/cf", site, "/tmp/met/gapfill", overwrite=overwrite, verbose=verbose))

# model specific
#print(met2model.ED2("/tmp/met/gapfill", site, "/tmp/met/ed", 0, overwrite=overwrite, verbose=verbose))
print(met2model.SIPNET(2000, 2001, "/tmp/met/gapfill", site, "/tmp/met/sipnet", overwrite=overwrite, verbose=verbose))

