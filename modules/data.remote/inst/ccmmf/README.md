# California Cropland Monitoring

This is the **monitoring** part of the California Cropland Monitoring and Modeling Framework (CCMMF) -- specifically the **Management Tracking** pipeline inside [PEcAn](https://pecanproject.github.io).

CCMMF uses the ecosystem model SIPNET to simulate California cropland carbon stocks and greenhouse-gas fluxes. That modeling needs a consistent picture of each field over time: which crop was grown, and what management happened (planting, harvest, tillage, fertilizer, irrigation, and related timing). Management Tracking produces those **management events** as inputs to the MAGiC annual inventory and scenario projections.

The sections below list the products, how they fit together, and where to run each part of the workflow.

## Products

The workflow is built in layers across three sessions. Session 1 establishes crop identity: the fields and seasons. Session 2 uses that crop identity to define phenology for each season, and also produces tillage. Session 3 produces fertilization and irrigation. Tillage, fertilization, and irrigation are all conditional on crop identity and phenology.

```mermaid
flowchart TB
  CROP["Crop identity"]
  PHENO["Phenology / planting / harvest"]
  TILL["Tillage"]
  FERT["N fertilization + organic amendments"]
  IRR["Irrigation"]

  CROP --> PHENO
  CROP --> TILL
  CROP --> FERT
  CROP --> IRR
  PHENO --> TILL
  PHENO --> FERT
  PHENO --> IRR
```




| Product            | Description                                        | Main source                                                        | Session |
| ------------------ | -------------------------------------------------- | ------------------------------------------------------------------ | ------- |
| Crop identity      | Crop type of each field each season                | LandIQ + CDL                                                       | 1       |
| Planting           | Crop start date and initial C/N pools              | HLS phenology + plant traits                                       | 2       |
| Harvest            | Biomass removal date and fractions                 | HLS phenology + plant traits                                       | 2       |
| Phenology          | Leaf-on / leaf-off timing                          | HLS phenology                                                      | 2       |
| Tillage            | Soil/residue disturbance in fallow windows         | HLS phenology + tillage index                                      | 2       |
| N fertilization    | Synthetic nitrogen applications by crop            | California crop guidelines                                         | 3       |
| Organic amendments | Manure, compost, biochar, and similar applications | Literature-derived amendment rates                                 | 3       |
| Irrigation         | Water applications over the season                 | Precip (CHIRPS), reference ET (CIMIS), soil water holding (SSURGO) | 3       |




## Run order by session



### Session 1 - Crop identity

Why: before we can say how a field was managed, we need to know which fields exist and what crop was grown on each one. LandIQ is California's statewide crop map. This session aligns successive LandIQ years onto stable field IDs and fills missing crop information so the rest of the pipeline can use them.


| Output                                | Source                                                                                 |
| ------------------------------------- | -------------------------------------------------------------------------------------- |
| Annual LandIQ shapefiles              | [CNRA Statewide Crop Mapping](https://data.cnra.ca.gov/dataset/statewide-crop-mapping) |
| Harmonized parcels + multi-year crops | [cadwr-landuse](https://github.com/ccmmf/cadwr-landuse)                                |
| Gap-filled crop identity              | [landiq-gapfill](landiq-gapfill/README.md)                                             |


Commands: [Session 1](documentation/sessions/01-landiq.md).

### Session 2 - Phenology, planting, harvest, tillage

Why: we also need to know when each field was planted, when leaves came on and off, when it was harvested, and when the soil was disturbed. This session extracts those dates from HLS phenology, sets planting C/N pools and harvest fractions from crop-specific literature, and detects tillage in the fallow period between crop seasons.


| Output                                 | Source                                       |
| -------------------------------------- | -------------------------------------------- |
| Parcel-tile map                        | [hls](hls/README.md)                         |
| LandIQ seasons matched to MSLSP cycles | [phenology/match](phenology/match/README.md) |
| Planting / harvest / phenology events  | [events](events/README.md)                   |
| Tillage events                         | [events](events/README.md)                   |
| Plant trait lookups                    | [traits](traits/README.md)                   |


Commands: [Session 2](documentation/sessions/02-phenology.md).

### Session 3 - Fertilizer, organic amendments, and irrigation

Why: finally, we need nitrogen fertilization, organic amendments, and irrigation on each field. Timing for these depends on the phenology from Session 2. This session sets N rates from California crop guidelines, sets organic amendment rates from literature-derived values, and computes irrigation with a simple water-bucket balance from precip, reference ET, and soil water holding capacity.


| Output                   | Source                                                                                       |
| ------------------------ | -------------------------------------------------------------------------------------------- |
| N fertilization events   | [fertilization-statewide](../../../../workflows/fertilization-statewide/README.md)           |
| Organic amendment events | [ncc-statewide](../../../../workflows/ncc-statewide/README.md)                               |
| Irrigation events        | [irrigation-statewide](../../../../workflows/irrigation-statewide/README.md)                 |


Commands: [Session 3](documentation/sessions/03-fertilizer-irrigation.md).

Together these products give MAGiC a wall-to-wall management record for California cropland: what grew on each field, when it was managed, and what was applied.
