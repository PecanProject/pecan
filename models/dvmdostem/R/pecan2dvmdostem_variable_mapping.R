##' Build a mapping from dvmdostem names to PEcAn names, units, etc.
##' The temunits should be (are) looked up from the dvmdostem output 
##' file's units attributes.
##' 
##' This data structure allows us to keep track of PEcAn output variables
##' that might depend on more than one DVMDOSTEM files.
##' @export
vmap_reverse <- list(
  "GPP"        = c(depends_on="GPP", longname="Gross Primary Productivity", newunits="kg C m-2 s-1"),
  "NPP"        = c(depends_on="NPP", longname="Net Primary Productivity", newunits="kg C m-2 s-1"),
  "HeteroResp" = c(depends_on="RH", longname="Heterotrophic Respiration", newunits="kg C m-2 s-1"),
  "SoilOrgC"   = c(depends_on="DEEPC SHLWC SOC", longname="Soil Organic Carbon", newunits="kg C m-2"),
  "LAI"        = c(depends_on="LAI", longname="Leaf Area Index", newunits="m2/m2"),
  "VegC"       = c(depends_on="VEGC", longname="Vegetation Carbon", newunits="kg C m-2"),
  "DeepC"      = c(depends_on="DEEPC", longname="Deep (amporphous) Soil C", newunits="kg C m-2"),
  "AvailN"     = c(depends_on="AVLN", longname="Available Nitrogen", newunits="kg N m-2")
)