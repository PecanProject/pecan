
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
