# Product metadata (column dictionaries)

Column definitions live **with each product component**.
This page indexes the dictionaries saved in `inst/ccmmf`.
Session I/O tables in [pipeline.md](pipeline.md) and
[sessions/](sessions/) link here instead of duplicating full schemas.

Fertilization and irrigation column docs live with those separate workflows
([Session 3](sessions/03-fertilizer-irrigation.md); not listed below yet).

| Product | File | Writer component |
|---------|------|----------------|
| Harmonized / gap-filled LandIQ `crops_all_years.parq` | [landiq-gapfill/data/crops_all_years_metadata.csv](../landiq-gapfill/data/crops_all_years_metadata.csv) | cadwr-landuse + landiq-gapfill |
| CDL parcel fractions | [landiq-gapfill/data/cdl_fractions_metadata.csv](../landiq-gapfill/data/cdl_fractions_metadata.csv) | landiq-gapfill |
| Upstream LandIQ (cadwr) | [crops_all_years_metadata.csv](https://github.com/ccmmf/cadwr-landuse/blob/main/data/crops_all_years_metadata.csv) | [cadwr-landuse](https://github.com/ccmmf/cadwr-landuse) |
| MSLSP parcel extract | [phenology/extract/data/mslsp_year_metadata.csv](../phenology/extract/data/mslsp_year_metadata.csv) | phenology/extract |
| NDTI parcel extract | [tillage/extract/data/ndti_year_metadata.csv](../tillage/extract/data/ndti_year_metadata.csv) | tillage/extract |
| LandIQ<->MSLSP match | [phenology/match/data/assigned_year_metadata.csv](../phenology/match/data/assigned_year_metadata.csv) | phenology/match |
| Planting events | [events/data/planting_statewide_metadata.csv](../events/data/planting_statewide_metadata.csv) | events |
| Harvest events | [events/data/harvest_statewide_metadata.csv](../events/data/harvest_statewide_metadata.csv) | events |
| Phenology events | [events/data/phenology_statewide_metadata.csv](../events/data/phenology_statewide_metadata.csv) | events |
| Tillage events | [events/data/tillage_statewide_metadata.csv](../events/data/tillage_statewide_metadata.csv) | events |

Trait lookup CSVs (`planting_lookup.csv`, `harvest_lookup.csv`) are documented in
[traits/README.md](../traits/README.md) (harvest includes `destructive`).
