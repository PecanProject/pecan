##' Build a mapping from dvmdostem names to PEcAn names, units, etc.
##' The temunits should be (are) looked up from the dvmdostem output 
##' file's units attributes.
##' 
##' This data structure allows us to keep track of PEcAn output variables
##' that might depend on more than one DVMDOSTEM files.
##' @export
vmap_reverse <- list(
  "GPP"        = c(depends_on="GPP", longname="Gross Primary Productivity", newunits="kg m-2 s-1"),
  "NPP"        = c(depends_on="NPP", longname="Net Primary Productivity", newunits="kg m-2 s-1"),
  "HeteroResp" = c(depends_on="RH", longname="Heterotrophic Respiration", newunits="kg m-2 s-1"),
  "AutoResp"   = c(depends_on="RM,RG", longname="Autotrophic Respiration", newunits="kg m-2 s-1"),
  "SoilOrgC"   = c(depends_on="SHLWC,SOMA,SOMCR,SOMPR,SOMRAWC", longname="Soil Organic Carbon", newunits="kg m-2"),
  "LAI"        = c(depends_on="LAI", longname="Leaf Area Index", newunits="m2/m2"),
  "VegC"       = c(depends_on="VEGC", longname="Vegetation Carbon", newunits="kg m-2"),
  "DeepC"      = c(depends_on="DEEPC", longname="Deep (amporphous) soil C", newunits="kg m-2"),
  "AvailN"     = c(depends_on="AVLN", longname="Available Nitrogen", newunits="kg m-2"),
  "NetNMin"    = c(depends_on="NETNMIN", longname="Net N Mineralization", newunits="kg m-2 s-1"),
  "NImmob"     = c(depends_on="NIMMOB", longname="N Immobilization", newunits="kg m-2 s-1"),
  "NInput"     = c(depends_on="NINPUT", longname="N Inputs to soil", newunits="kg m-2 s-1"),
  "NLost"      = c(depends_on="NLOST", longname="N Lost from soil", newunits="kg m-2 s-1"),
  "NUptakeIn"  = c(depends_on="INNUPTAKE", longname="N Uptake ignoring N limitation", newunits="kg m-2 s-1"),
  "NUptakeSt"  = c(depends_on="NUPTAKEST", longname="N Uptake Structural", newunits="kg m-2 s-1"),
  "NUptakeLab" = c(depends_on="NUPTAKELAB", longname="N Uptake Labile", newunits="kg m-2 s-1"),
  "OrgN"       = c(depends_on="ORGN", longname="Total Soil Organic N", newunits="kg m-2"),
  "ShlwC"      = c(depends_on="SHLWC", longname="Shallow (fibrous) soil C", newunits="kg m-2"),
  "VegN"       = c(depends_on="VEGN", longname="Vegetation N", newunits="kg m-2")
  #"" = c(depends_on="", longname="", newunits="")
)