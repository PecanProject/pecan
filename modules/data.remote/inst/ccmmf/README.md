# CCMMF statewide monitoring (`inst/ccmmf`)

California LandIQ → phenology → management-event pipeline scripts for PEcAn.

**Documentation (start here):** [documentation/README.md](documentation/README.md)  
**Environment setup:** [documentation/sessions/00-environment.md](documentation/sessions/00-environment.md)  
**PR:** https://github.com/PecanProject/pecan/pull/3913

## Layout

| Path | Role |
|------|------|
| `documentation/` | Stakeholder sessions + `pipeline.md` + `ccmmf_env.example.sh` |
| `landiq-gapfill/` | Crop/ADOY gap-fill → LandIQ v4.1.2 product |
| `mslsp-extract/` | MSLSP parcel extract (needs sibling `hls/_lib`) |
| `ndti-extract/` | NDTI parcel extract (needs sibling `hls/_lib`) |
| `hls/` | Parcel–tile map + shared tilewise `_lib` |
| `phenology/` | Match LandIQ↔MSLSP + date gap-fill |
| `traits/` | Planting/harvest trait lookups |
| `events/` | Statewide phenology/planting/harvest/tillage events |
| `tillage/` | NDTI tillage metrics helpers |
| `LandIQ_cropCode_lookup_table.csv` | Crop metadata / PFT |

## Quick start

```bash
export CCMMF_CODE="$(pwd)/modules/data.remote/inst/ccmmf"   # from PEcAn clone
cp "$CCMMF_CODE/documentation/ccmmf_env.example.sh" ~/ccmmf_env.sh
# edit paths, then:
source ~/ccmmf_env.sh
```

Set `HLS_SHARED_LIB=$CCMMF_CODE/hls/_lib` if auto-detection fails. Upstream HLS/MSLSP
NetCDF production: [HLS_Phenology](https://github.com/mrinareddy/HLS_Phenology).
