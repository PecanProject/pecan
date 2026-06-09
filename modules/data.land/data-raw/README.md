
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

Three datasets for nitrogen management on California crops. Like BADM
above, each one ships as a harmonized CSV in this folder, and its
`create_*.R` script just reads that CSV and writes the `.rda`, nothing
fancy. `ca_n_application_rate` carries recommended N rate ranges for 40
crops. `ca_organic_amendment_properties` carries the intrinsic
properties of 32 amendment materials (C:N, total N, plant available N at
4 weeks, and the CalRecycle feedstock class).
`ca_organic_amendment_app_rate` carries how much of each material you
put down, 64 rows because every material gets one rate for row crops and
one for orchards.

The CSVs are not where the work happens. They fall out of
`harmonize_fertilization_data.R`, which lives with the rest of the CCMMF
management workflow under
`/projectnb/dietzelab/ccmmf/management/fertilization/`, outside this
package. That script reads the curated TSV exports of the CCMMF
fertilization spreadsheet, maps the source URLs to short author-year
citations, folds the per-stage N rows into one range per crop, converts
lbs/acre to g/m2 with `PEcAn.utils::ud_convert`, splits the
spreadsheet's wide Rows/Trees layout into the long properties and
app-rate tables, drops a couple of bad columns, and assigns the
`material_class`. Whatever lands in the three CSVs here is what the
package builds from.

## Where the numbers come from

The N rates trace back to Rosenstock et al. 2013 (*California
Agriculture* 67(1), doi:10.3733/ca.E.v067n01p68), the CDFA-FREP and UC
Davis fertilization guidelines, Meyer et al. 2007 for alfalfa (UC ANR
pubs 8292 and 8296), Brown et al. 2020 NBMP Table 2 for almonds by age
(Almond Board of California), and Lazicki, Geisseler and Horwath 2016
for potato (CDFA-FREP / UC Davis). The citation for each crop sits in
its `source` column. The `pft_group` labels (row, woody, rice) come
straight off the spreadsheet, hand-authored rather than mapped from
`cadwr_pfts.csv`, though they follow the same grouping it uses.

The amendment numbers come from Eghball (UNL Extension G2222) and Rynk
(NC State Extension compost handbook), again cited per row.
`material_class` is the one field we set by hand, slotting each material
into the CalRecycle feedstock taxonomy from 14 CCR 17852. The current 32
materials only land in four of the classes (ag, food, yard, wood); green
and biosolids are valid classes but nothing here falls in them.

Everything else is verbatim off the spreadsheet: crop, pft_group,
material, total N, PAN, the C:N range, the N tier, and the raw Rows and
Trees application rates. The pieces we computed or assigned are the
per-crop N range (aggregated as below), the short citation strings, the
`material_class`, the `crop_structure` label, and every g/m2 column
(converted from lbs/acre).

## How the per-crop N range gets built

Most crops, 33 of them, report a whole season rate, so we take that
range as is. Six report only by growth stage (Barley, the three Beans,
Cauliflower, Melons), so their within year stages get summed into a
season total. Almonds are the odd one out, reporting by orchard age
across 8 NBMP stages, so there we envelope the low and high across all
ages. Onions drop out, since its only row is a "1/3 of total N at
preplant" note with no rate attached.

## Fixes we made at the source

A few things in the spreadsheet were wrong and got fixed upstream before
harmonizing:

- Dropped `Peach, bell` and `Peach, chili`. Their numbers were
  byte for byte copies of `Pepper, bell` and `Pepper, chili`, and the
  paper they cite (Rosenstock 2013 Table 2, p. 76) lists no peach-bell
  or peach-chili at all. Clearly a copy paste slip.
- Added `Potato` at 189 to 248 lb N/acre from Lazicki, Geisseler and
  Horwath 2016. Worth being upfront: Rosenstock 2013 p. 76 says outright
  that no UC ANR guideline exists for potato in California, so this is a
  grower-reported envelope, not an official guideline.
- Tossed the `Total_C` columns. The spreadsheet built them as
  `Total_N * C_assumed` instead of `AppRate * C_assumed`, which is
  dimensionally wrong and would feed the model garbage carbon inputs.
- Added the `material_class` column with the 32 hand-assigned CalRecycle
  classes.

One more on the amendment rates: for 30 of the 32 materials the row-crop
and orchard rates match in the spreadsheet, but two N-rich materials
(Blood meal and fresh poultry manure) carry lower rates on trees than on
rows. Splitting into the long app-rate table keeps both rather than
picking one.

## Updating the data

1. Edit the source TSV under
   `/projectnb/dietzelab/ccmmf/management/fertilization/`.
2. Rerun `harmonize_fertilization_data.R` from that folder. It
   overwrites the three `ca_*.csv` files there.
3. Copy those three CSVs into this folder.
4. Rerun `create_ca_n_application_rate.R` and
   `create_ca_organic_amendment.R`.
5. Bump the package version, add a `NEWS.md` line, and regenerate the
   man pages with `devtools::document()`.
