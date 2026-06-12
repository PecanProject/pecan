# Package data

Shipped with the package:

| File | Role |
|------|------|
| `LandIQ_cropCode_lookup_table.csv` | CLASS / SUBCLASS metadata, `is_agricultural`, legend harmonization |
| `LandIQ_grouped_subclass_cdl_split.csv` | CDL disambiguation for RS-grouped codes T31, D16 |
| `cdl_nass_cropland_code_lookup.csv` | USDA NASS CDL code names |

**Transition matrices** (required for full-gap CLASS fill):

| Path | Role |
|------|------|
| `county_transition_matrices/` | One `{County}_transition_matrix.csv` per county |
| `state_transition_matrix.csv` | Statewide CLASS transition fallback |

On SCC these are symlinks to the project training matrices. For other sites, copy your own CSVs here or set `COUNTY_TRANSITION_MATRICES_DIR` and `EXTERNAL_TRANSITION_MATRIX_CSV`.
