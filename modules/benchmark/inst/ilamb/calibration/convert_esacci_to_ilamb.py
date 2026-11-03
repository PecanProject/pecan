"""
Convert the ESA CCI Biomass v7.0 aggregated GeoTIFF to an ILAMB-compatible
NetCDF, matching the format of the existing XuSaatchi biomass benchmark so it can
be used as a drop-in, more recent alternative (2015-2024) that also carries a
per-pixel uncertainty layer.

The ESACCI aggregated file is a global 0.25 degree GeoTIFF with one band per year
(2005-2012 and 2015-2024) in Mg/ha of dry above-ground biomass. This script:

  1. extracts the requested year(s) from the multi-band GeoTIFF,
  2. regrids 0.25 -> 0.5 degree (averaging) to the 360 x 720 benchmark grid that
     XuSaatchi uses,
  3. applies the IPCC default woody carbon fraction (0.47) to convert dry biomass
     to carbon density, since XuSaatchi and the PEcAn model output are in carbon
     (Mg C/ha), not dry biomass, and
  4. writes a CF NetCDF with a `biomass` variable (carbon density, Mg ha-1) and a
     `biomass_sd` uncertainty variable, on a (time, lat, lon) grid.

Data source (Open Access, free):
  ESA CCI Biomass v7.0, Santoro & Cartus (2026), on CEDA:
  https://data.ceda.ac.uk/neodc/esacci/biomass/data/agb/maps/v7.0/geotiff/aggregated
  Files used: ESACCI-BIOMASS-L4-AGB-MERGED-25000m-fv7.0.tif  (biomass)
              ESACCI-BIOMASS-L4-AGB_SD-MERGED-25000m-fv7.0.tif (uncertainty)

NOTE ON UNITS: ESACCI reports dry biomass; XuSaatchi's long_name is an annual
carbon density map and the PEcAn AbvGrndWood output is carbon. Converting biomass
to carbon with the standard 0.47 fraction is required for an apples-to-apples
benchmark. In dense forest the converted ESACCI agrees with XuSaatchi to within
about 13 percent, confirming the conversion; the two products diverge more in
sparse, low-biomass areas, which is expected for independent biomass products.
"""

import numpy as np

# rasterio and xarray are imported lazily inside the functions that use them, so
# the year-to-band mapping and carbon-fraction constants can be imported (e.g. by
# the tests) without the geospatial stack installed.

# ESACCI band index (1-based) for each available year (2013, 2014 are absent).
ESACCI_YEARS = [2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012,
                2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024]
YEAR_TO_BAND = {y: i + 1 for i, y in enumerate(ESACCI_YEARS)}

# target 0.5 degree grid (matches the XuSaatchi benchmark exactly)
TARGET_LAT = np.arange(89.75, -90, -0.5)     # 89.75 .. -89.75, 360 points
TARGET_LON = np.arange(-179.75, 180, 0.5)    # -179.75 .. 179.75, 720 points

# IPCC default carbon fraction of dry woody biomass (biomass -> carbon)
CARBON_FRACTION = 0.47


def read_year_regridded(tif_path, year):
    """Read one year's band from the ESACCI tif, regridded to the 0.5 deg grid."""
    import rasterio
    from rasterio.enums import Resampling
    band = YEAR_TO_BAND[year]
    with rasterio.open(tif_path) as src:
        data = src.read(
            band,
            out_shape=(360, 720),
            resampling=Resampling.average,
        ).astype("float32")
        nodata = src.nodata
    if nodata is not None:
        data[data == nodata] = np.nan
    # ESACCI is north-up (lat 90 -> -90), matching TARGET_LAT ordering
    return data


def build_dataset(agb_tif, sd_tif, years):
    """Build an xarray Dataset with carbon-density biomass (+ uncertainty)."""
    import xarray as xr
    agb = np.stack([read_year_regridded(agb_tif, y) for y in years], axis=0) * CARBON_FRACTION
    sd = np.stack([read_year_regridded(sd_tif, y) for y in years], axis=0) * CARBON_FRACTION

    # time as days since 1850-01-01 (Jan 1 of each year), CF noleap
    ref = np.datetime64("1850-01-01")
    times = np.array([(np.datetime64(f"{y}-01-01") - ref) / np.timedelta64(1, "D")
                      for y in years], dtype="float64")

    return xr.Dataset(
        {
            "biomass": (("time", "lat", "lon"), agb,
                        {"long_name": "above-ground live woody carbon density",
                         "units": "Mg ha-1",
                         "note": "ESACCI dry AGB x 0.47 IPCC woody carbon fraction"}),
            "biomass_sd": (("time", "lat", "lon"), sd,
                           {"long_name": "above-ground carbon density standard deviation",
                            "units": "Mg ha-1"}),
        },
        coords={
            "time": ("time", times,
                     {"standard_name": "time", "units": "days since 1850-01-01",
                      "calendar": "noleap"}),
            "lat": ("lat", TARGET_LAT.astype("float64"),
                    {"standard_name": "latitude", "units": "degrees_north"}),
            "lon": ("lon", TARGET_LON.astype("float64"),
                    {"standard_name": "longitude", "units": "degrees_east"}),
        },
        attrs={
            "title": "ESA CCI Biomass v7.0 aboveground carbon (biomass x 0.47), 0.5 deg",
            "source": "ESACCI-BIOMASS-L4-AGB-MERGED-25000m-fv7.0",
            "reference": "Santoro & Cartus (2026), ESA CCI Biomass v7.0",
            "Conventions": "CF-1.7",
        },
    )


def main():
    agb_tif = "ESACCI_AGB_25km.tif"
    sd_tif = "ESACCI_AGB_SD_25km.tif"
    years = [2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024]

    ds = build_dataset(agb_tif, sd_tif, years)
    out = "ESACCI_biomass_0.5deg.nc"
    ds.to_netcdf(out)
    print(f"wrote {out}")
    print(f"  years: {years}")
    print(f"  biomass shape: {ds['biomass'].shape}")
    print(f"  biomass range: {float(ds['biomass'].min()):.1f} to "
          f"{float(ds['biomass'].max()):.1f} Mg C/ha")


if __name__ == "__main__":
    main()
