 # Irrigation Projection

This workflow projects irrigation events for California agricultural parcels from 2024–2045.

The workflow has two main steps:

1. Convert the downloaded Cal-Adapt climate design-point data into county-level daily climate.
2. Combine projected crop identity, planting date, harvest date, crop water requirements, soil water-holding capacity, and daily climate to estimate irrigation events for each parcel.

The final annual irrigation files contain:

```text
event_type
parcel_id
date
amount_mm
```

---

# 1. User setup

The user should first define a working directory.

```r
work_root = "/path/to/your/folder"
```

This directory is used for intermediate products and final irrigation outputs.

Shared project data are read from:

```r
ccmmf_root = "/projectnb/dietzelab/ccmmf"
```

Most users should not need to modify `ccmmf_root`.

The irrigation workflow expects the following upstream projection products inside `work_root`:

```text
<work_root>/
├── crop_predictions/
├── planting_projections/
├── harvest_projections/
└── caladapt_county_daily_climate.csv
```

The irrigation script is currently configured to project:

```text
2024–2045
```

and uses historical phenology information from:

```text
2018–2023
```

The current climate selection is:

```text
GCM = CESM2
SSP = ssp245
```

---

# 2. Prepare county-level Cal-Adapt climate

The Cal-Adapt climate data needed for irrigation have already been downloaded for 2025–2045.

The raw files are stored under:

```text
/projectnb/dietzelab/ccmmf/ensemble/CalAdapt_runs/data_raw/CalAdaptWRF
```

The downloaded data represent approximately 198 Cal-Adapt spatial design points across California.

The goal of this preprocessing step is to convert those design-point climate data into one daily climate series for each county.

The spatial assumption used here is All agricultural parcels within the same county experience the same daily climate.

This is a deliberate spatial simplification. Parcel-level differences in crop identity, phenology, soil properties, and water requirements are retained later in the irrigation calculation, while meteorological forcing is summarized at the county level.

---

## 2.1 Assign climate design points to counties

Each Cal-Adapt design-point folder contains NetCDF files with latitude and longitude coordinates.

The script reads one NetCDF file from each folder and constructs a site index containing:

```text
site_hash
latitude
longitude
```

The expected number of unique design points is approximately:

```text
198
```

California county boundaries are retrieved using `tigris`.

Each design point is spatially assigned to the county containing it.

For counties that do not contain a Cal-Adapt design point, the workflow:

1. projects the county and design-point geometries to California Albers coordinates;
2. identifies a representative point within the county;
3. finds the nearest available Cal-Adapt design point;
4. assigns that design point to the county.

This produces:

```text
caladapt_county_site_lookup.csv
```

with the basic mapping:

```text
County
site_hash
```

Every county should have at least one assigned climate site before continuing.

---

## 2.2 Convert hourly NetCDF data to daily climate

Each downloaded NetCDF file is processed independently.

The helper functions:

```text
specific_humidity_to_ea()
calculate_net_radiation()
parse_nc_time()
calculate_daily_et0()
process_nc_file()
```

convert the raw climate variables into daily meteorological values.

The NetCDF files provide variables including:

```text
air temperature
wind speed
precipitation
specific humidity
shortwave radiation
longwave radiation
air pressure
```

Temperature is converted from Kelvin to Celsius and precipitation flux is converted to millimeters.

Daily reference evapotranspiration (`ET0_mm`) is then calculated using the FAO-56 Penman-Monteith method.

The resulting daily design-point data include variables such as:

```text
date
site_hash
model
scenario
ET0_mm
precip_mm
mean_temp_c
T_min
T_max
```

NetCDF processing is parallelized across available CPU cores because this is one of the more computationally expensive preprocessing steps.

---

## 2.3 Collapse design-point climate to county climate

The daily design-point climate is joined to the county-site lookup using `site_hash`.

The data are then grouped by:

```text
County
date
model
scenario
```

If multiple Cal-Adapt design points contribute to the same county, the daily values are averaged across those sites.

The county climate product includes:

```text
ET0_mm
precip_mm
mean_temp_c
min_temp_c
max_temp_c
n_climate_sites
Year
Week
GCM
SSP
```

The final climate input is written to:

```text
<work_root>/caladapt_county_daily_climate.csv
```

This file is then used directly by the irrigation projection.

---

# 3. Load future crop and phenology projections

The irrigation workflow combines three upstream projection products for every year from 2025–2045:

```text
crop identity
planting date
harvest date
```

The expected files are:

```text
<work_root>/crop_predictions/crop_identity_statewide_<YEAR>.parquet
<work_root>/planting_projections/planting_statewide_<YEAR>.parquet
<work_root>/harvest_projections/harvest_statewide_<YEAR>.parquet
```

Each active parcel must have:

```text
parcel_id
crop identity
planting date
harvest date
```

The script checks that there is only one record per parcel and year and stops if active crops are missing planting or harvest dates.

LandIQ classes representing non-active agricultural land are excluded before irrigation is modeled.

---

# 4. Estimate crop peak date

The irrigation water-demand calculation also requires a crop peak date.

Rather than projecting peak date independently, the workflow estimates the historical relative position of the peak within the growing season.

For each historical observation:

```text
peak fraction =
(peak date - planting date) /
(harvest date - planting date)
```

Historical peak fractions are calculated from the matched LandIQ/MSLSP phenology records for 2018–2023.

For a future crop, the workflow attempts to estimate the peak fraction using increasingly broad historical groups:

```text
county + crop code
        ↓
county + crop class
        ↓
crop code
        ↓
crop class
        ↓
statewide/global mean
```

The projected peak date is then:

```text
planting date +
peak fraction × growing-season length
```

The workflow checks that projected peak dates remain between planting and harvest. :contentReference[oaicite:1]{index=1}

---

# 5. Assign crop water parameters

Projected LandIQ crop classes and subclasses are mapped to crop names used by `PEcAn.data.land`.

The workflow uses:

```r
PEcAn.data.land::bism_kc_by_crop
```

for crop mappings and:

```r
PEcAn.data.land::crop_whc
```

for crop-specific soil-water parameters.

For non-rice crops, the important parameters include:

```text
rooting_depth_m
whc_min_frac
```

`whc_min_frac` controls the soil-water depletion threshold used to determine when irrigation should occur.

A small set of LandIQ classes that do not have an exact BISM mapping are assigned predefined proxy crops.

Rows without a usable crop mapping, rooting depth, or water-depletion parameter are identified before irrigation is modeled. :contentReference[oaicite:2]{index=2}

---

# 6. Calculate and cache soil available water capacity

Non-rice irrigation also depends on the amount of water that can be stored in the soil within the crop rooting zone.

Available water capacity is calculated using shared SSURGO soil information.

The relevant SSURGO inputs are:

```text
parcel-to-soil-map-unit weights
soil components
soil horizons
available water capacity by horizon
```

For each unique combination of:

```text
parcel_id + rooting_depth_m
```

the workflow calculates effective root-zone water-holding capacity (`whc_mm`).

Because this calculation is relatively expensive, results are cached by county under:

```text
<work_root>/irrigation_awc_cache/
```

For example:

```text
irrigation_awc_cache/
├── Fresno_awc.parquet
├── Kern_awc.parquet
├── Tulare_awc.parquet
└── ...
```

On the first run, the required AWC values are calculated from SSURGO.

On later runs:

```text
already cached parcel/root depth
    → reuse existing AWC

new parcel/root depth
    → calculate from SSURGO and add to cache
```

This prevents the full soil calculation from being repeated every time the irrigation workflow is run. :contentReference[oaicite:3]{index=3}

Rice does not use this soil AWC calculation because it is modeled using a separate flooded-field water balance.

---

# 7. Select future climate

The county climate CSV is filtered to the selected GCM and SSP combination.

The current configuration uses:

```text
CESM2
ssp245
```

The actual Cal-Adapt climate must cover the full 2025–2045 projection period.

Some crop seasons can extend across the boundary of the available climate period. For example, a crop assigned to projection year 2025 may have a planting date in late 2024.

For these boundary cases only, the workflow uses the nearest available boundary year as a climate proxy.

Conceptually:

```text
required 2024 climate → use 2025 daily climate
required 2046 climate → use 2045 daily climate
```

The actual 2025–2045 projection years are always expected to use the downloaded Cal-Adapt climate data. :contentReference[oaicite:4]{index=4}

---

# 8. Calculate daily crop water demand

For each crop season, the model generates a daily sequence from:

```text
planting date → harvest date
```

Canopy cover is approximated as increasing from planting to the projected crop peak and decreasing from peak to harvest.

Reference evapotranspiration is converted to crop evapotranspiration using:

```r
PEcAn.data.land::eto_to_etc_bism()
```

Conceptually:

```text
Cal-Adapt ET0
      +
crop identity
      +
canopy development
      ↓
crop ET
```

Daily crop ET, precipitation, soil water storage, and crop-specific depletion thresholds are then used to estimate irrigation.

---

# 9. Non-rice water balance

For non-rice crops, irrigation is estimated using:

```r
PEcAn.data.land::calc_water_balance()
```

The model uses:

```text
daily crop ET
daily precipitation
root-zone water-holding capacity
crop-specific minimum soil-water fraction
maximum irrigation amount
```

The current maximum irrigation event is:

```text
150 mm
```

An irrigation event is recorded whenever the modeled daily irrigation amount is greater than zero.

---

# 10. Rice water balance

Rice is modeled separately because flooded rice fields cannot be represented appropriately with the same soil-water bucket model used for other crops.

Rice uses:

```r
PEcAn.data.land::calc_water_balance_rice()
```

with the current configuration:

```text
target flood depth = 125 mm
minimum flood depth = 62.5 mm
maximum flood depth = 175 mm
seepage = 2.5 mm/day
```

The rice model generates irrigation events needed to maintain the flooded-field water level.

The script therefore internally tracks two irrigation methods:

```text
canopy    non-rice crops
flood     rice
```

The method variable is used during processing but is not required in the final projection product. :contentReference[oaicite:5]{index=5}

---

# 11. Parallel county prediction

The statewide irrigation calculation is divided by county.

Each county is processed independently, allowing the workflow to run several counties in parallel.

Temporary county-level results are stored under:

```text
<work_root>/irrigation_projections/_county_tmp/
```

This also allows interrupted runs to resume without recalculating counties that have already completed.

Large counties are processed first and jobs are load-balanced across the available workers. :contentReference[oaicite:6]{index=6}

If the irrigation methodology or code changes, the temporary county directory should be deleted before rerunning so that old county results are not reused.

---

# 12. Final irrigation outputs

After all counties have completed, the temporary county outputs are combined into annual statewide irrigation files.

One parquet is written for each year:

```text
<work_root>/irrigation_projections/irrigation_statewide_2025.parquet
<work_root>/irrigation_projections/irrigation_statewide_2026.parquet
...
<work_root>/irrigation_projections/irrigation_statewide_2045.parquet
```

Each irrigation event contains:

```text
event_type
parcel_id
date
amount_mm
```

where:

```text
event_type = "irrigation"
parcel_id  = LandIQ parcel identifier
date       = projected irrigation date
amount_mm  = irrigation applied on that date, in millimeters
```

The workflow checks that final irrigation amounts are finite, positive, and complete before writing each yearly parquet. :contentReference[oaicite:7]{index=7}

---

# Workflow summary

The complete irrigation projection can be summarized as:

```text
Projected crop identity
Projected planting date
Projected harvest date
          │
          ├── historical phenology → projected peak date
          │
          ├── crop mapping → crop water parameters
          │
          ├── SSURGO → parcel/root-depth AWC
          │
Cal-Adapt climate
          │
          └── 198 design points
                  ↓
              county climate
                  │
                  ↓
        daily crop evapotranspiration
                  │
                  ↓
          daily water balance
            /             \
       non-rice           rice
       soil bucket      flood balance
            \             /
                  ↓
          irrigation events
                  ↓
      annual statewide parquets
```

The major simplifying spatial assumption is that climate is shared among all parcels within a county. Crop identity, planting and harvest timing, projected peak timing, soil water capacity, rooting depth, and crop water parameters remain parcel-specific.