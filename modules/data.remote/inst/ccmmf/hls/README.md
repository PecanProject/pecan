# HLS shared helpers

Shared tilewise framework and one-time **parcel-tile map** used by product
extracts. This folder is an index and shared library, not a second pipeline.

**Harmonized Landsat Sentinel-2 (HLS)** tiles are the spatial grid. Two extracts
reuse the same map and `R/tilewise_core.R`:

| Product | Cadence | Operator doc |
|---------|---------|--------------|
| **MSLSP** (Multi-Source Land Surface Phenology) | annual | [phenology/extract/README.md](../phenology/extract/README.md) |
| **NDTI** (Normalized Difference Tillage Index) | monthly | [tillage/extract/README.md](../tillage/extract/README.md) |

End-to-end order: [documentation/pipeline.md](../documentation/pipeline.md).

## What lives here

```
hls/
+-- build_hls_parcel_tile_map.R   # parcel -> MGRS tiles (geometry only; once)
+-- build_hls_tile_extent.R       # tile extent grid (if missing)
+-- R/                            # tilewise_core.R and shared extract helpers
```

Product-specific extract logic stays in `phenology/extract/` and `tillage/extract/`.

## Parcel-tile map (one-time)

Why: each LandIQ parcel may overlap one or more HLS Military Grid Reference
System (MGRS) tiles. Extracts need that lookup before they can pull NetCDF or
imagery. Build once from harmonized (or gap-filled) geometry; reuse every year.
Re-run only when `parcels-consolidated.gpkg` changes. Which parcels are
agricultural in a given year is decided later inside each extract's prep step
from `crops_all_years.parq`.

```bash
Rscript "$CCMMF_CODE/hls/build_hls_parcel_tile_map.R" overwrite
```

Outputs under `$CCMMF_MANAGEMENT`:

| File | Contents |
|------|----------|
| `hls_parcel_tile_map_v4.1.rds` | `parcel_id`, `tileIDs`, `n_tiles` |
| `hls_tile_to_parcels_v4.1.rds` | inverted list (tile -> parcels) |

Upstream HLS imagery and MSLSP NetCDF come from
[HLS_Phenology](https://github.com/mrinareddy/HLS_Phenology)
(`$CCMMF_ROOT/data_phen/`).
