% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Create_Site_PFT_CSV.R
\name{Create_Site_PFT_CSV}
\alias{Create_Site_PFT_CSV}
\title{Title Identify pft for each site of a multi-site settings using NLCD and Eco-region}
\usage{
Create_Site_PFT_CSV(settings, Ecoregion, NLCD, con)
}
\arguments{
\item{settings}{a multi-site settings}

\item{Ecoregion}{path of Ecoregion data (*.shp)}

\item{NLCD}{path of NLCD img file}

\item{con}{connection to bety}
}
\value{
pft info with each site
}
\description{
Title Identify pft for each site of a multi-site settings using NLCD and Eco-region
}
\examples{
\dontrun{
 NLCD <- file.path(
   "/fs", "data1", "pecan.data", "input",
   "nlcd_2001_landcover_2011_edition_2014_10_10",
   "nlcd_2001_landcover_2011_edition_2014_10_10.img")
 Ecoregion <- file.path(
   "/projectnb", "dietzelab", "dongchen",
   "All_NEON_SDA", "NEON42", "eco-region", "us_eco_l3_state_boundaries.shp")
 settings <- PEcAn.settings::read.settings(
   "/projectnb/dietzelab/dongchen/All_NEON_SDA/NEON42/pecan.xml")
 con <- PEcAn.DB::db.open(settings$database$bety)
   site_pft_info <- Create_Site_PFT_CSV(settings, Ecoregion, NLCD, con)
}

}
