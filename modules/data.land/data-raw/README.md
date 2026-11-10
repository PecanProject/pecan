
## BADM

Various ancillary data from Fluxnet.
Provenence of this  particular file not clear, but it contains data from
246 Fluxnet sites. Variables include aboveground and belowground biomass
in various pools, plus soil texture/chemistry/horizonation/C&N stocks.

`load_BADM.R` simply reads `BADM.csv` and writes it as `../data/BADM.rda`.


# Soil variables

Default parameter values for soil of various texture classes.
The values are apparently derived from Cosby et al 1984,
"A Statistical Exploration of the Relationships of Soil Moisture
Characteristics to the Physical Properties of Soils",
Water Resources Research 20(6): 682-690.
This implementation comes from one provided by the ED2 model.
Not clear if texture.csv came from there or another source.
	PEcAn.linkages contains an identical texture.csv,
	also with no obvious source label.

`build_soil_texture_variables.R` hardcodes the values for many parameters
in a list named `soil_class` and saves it to `../data/soil_class.rda`,
from which it is lazy-loaded when `soil_class` is used inside
package functions.
The saved file includes all of `texture.csv`, hence why that lives in
`data-raw` rather than directly in `data/`.


# California fertilization datasets

Three datasets cover nitrogen and organic amendment management for California
crops. Each ships as a harmonized CSV in this folder that a `create_*.R` script
reads and writes to `.rda`. `ca_n_application_rate` carries recommended N rate
ranges for 40 crops. `ca_organic_amendment_properties` carries the intrinsic
properties of the amendment materials (C:N, total N, plant available N at 4
weeks, and the CalRecycle feedstock class) in 32 rows that cover 29 distinct
materials, since a few are reported by two sources. `ca_organic_amendment_app_rate`
carries the application rates, 64 rows because each of the 32 property rows has
one rate for row crops and one for orchards. `create_ca_n_application_rate.R`
builds the first dataset and `create_ca_organic_amendment.R` builds the two
amendment datasets.

The raw curation lives in `n_fertilization.tsv` and `organic_amendments.tsv` in
this folder, exported from the CCMMF fertilization spreadsheet with the source
URLs kept. `build_ca_fertilization_data.R` reads them, shortens the URLs to
author year citations, folds the per-crop N rows into one range per crop,
converts lbs/acre to g/m2, and splits the wide
Rows/Trees compost layout into the long properties and application rate tables;
it writes the three `ca_*.csv`. `material_class` is read from the spreadsheet's
CalRecycle feedstock column.

## Where the numbers come from

The N rates trace back to Rosenstock et al. 2013 (*California
Agriculture* 67(1), doi:10.3733/ca.E.v067n01p68), the CDFA-FREP and UC
Davis fertilization guidelines, Meyer et al. 2007 for alfalfa (UC ANR
pub 8292), Brown et al. 2020 NBMP Table 2
for almonds by orchard age (Almond Board of California), and Lazicki,
Geisseler and Horwath 2016 for potato (CDFA-FREP / UC Davis). The citation
for each crop is in its `source` column. The `pft_group` labels (row, woody,
rice) come from the `cadwr_pfts.csv`

The amendment numbers come from Eghball (UNL Extension G2222) and Rynk
(NC State Extension compost handbook), cited per row. `material_class` is
hand assigned in the spreadsheet, placing each material in the CalRecycle
feedstock taxonomy of 14 CCR 17852; the 32 rows use four of its classes
(ag, food, yard, wood).

Everything else is taken verbatim from the spreadsheet: crop, pft_group,
material, material_class, total N, PAN, the C:N range, the N class, and the raw
Rows and Trees application rates. The derived fields are the per-crop N range
(aggregated as below), the short citation strings, the `crop_structure` label,
and the g/m2 columns (converted from lbs/acre).

## How the per-crop N range is built

Each crop is assigned one N range by one of three rules. Most crops (33)
report a whole season rate, taken as the envelope of their total season rows.
Six report only by within season stage -- Barley, Cauliflower, Melons, and the
blackeye, common, and lima beans -- so their stage rows are summed into a
season total. Almonds report by orchard age, so their age class rows are
enveloped across all ages. An `Onions` row carries only a "1/3 of total N
at preplant" note with no rate and is dropped; the `Onion` crop itself is kept,
at a 100 to 400 lb N/acre whole season range.

## Corrections applied before harmonizing

The following corrections were applied to the spreadsheet before harmonizing:

- Removed `Peach, bell` and `Peach, chili`. Their values were exact copies of
  `Pepper, bell` and `Pepper, chili`, and the cited source (Rosenstock 2013
  Table 2, p. 76) lists no `Peach, bell` or `Peach, chili`, so the rows were a
  copy error.
- Added `Potato` at 189 to 248 lb N/acre from Lazicki, Geisseler and
  Horwath 2016. Rosenstock 2013 p. 76 states that no UC ANR guideline exists
  for potato in California, so this is a grower-reported envelope rather than
  an official guideline.
- Repointed the `Grapevines` N source. The spreadsheet cited Meyer et al. 2007
  UC ANR 8295 and 8296, which are alfalfa insect and disease chapters rather
  than grape references. The 20 to 60 lb N/acre value is the raisin grape
  guideline, so the source now matches `Grape, raisin` (Rosenstock 2013,
  Table 2, from Christensen 2000 UC ANR 3393).
- Excluded the spreadsheet's `Total_C` columns. They were computed as
  `Total_N * C_assumed` rather than `AppRate * C_assumed`, which is
  dimensionally inconsistent; carbon is not carried in these tables.
- Recorded `material_class` for all 32 rows against the CalRecycle taxonomy.

For the amendment rates, 30 of the 32 rows carry the same rate for row crops
and orchards; two N-rich materials (Blood meal and fresh poultry manure) carry
lower rates on trees than on rows. The long application rate table keeps both.

## Updating the data

1. Edit `n_fertilization.tsv` or `organic_amendments.tsv` in this folder (or
   re-export them from the spreadsheet), keeping the source URLs.
2. Run `Rscript data-raw/build_ca_fertilization_data.R` from the package root.
   It rewrites the three `ca_*.csv` in this folder.
3. Rerun `create_ca_n_application_rate.R` and
   `create_ca_organic_amendment.R`.
4. Bump the package version, add a `NEWS.md` line, and regenerate the man
   pages with `devtools::document()`.
