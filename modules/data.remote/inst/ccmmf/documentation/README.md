# CCMMF documentation

Canonical Management Tracking docs for the monitoring tree
(`modules/data.remote/inst/ccmmf`).

| Doc | Role |
|-----|------|
| [pipeline.md](pipeline.md) | Product map, annual update SOP, QC gates (start here) |
| [sessions/00-setup.md](sessions/00-setup.md) | Session 0 - Setup |
| [sessions/01-landiq.md](sessions/01-landiq.md) | Session 1 - LandIQ crop identity |
| [sessions/02-phenology.md](sessions/02-phenology.md) | Session 2 - HLS events (phenology and tillage) |
| [sessions/03-fertilizer-irrigation.md](sessions/03-fertilizer-irrigation.md) | Session 3 - Fertilization and irrigation |
| [sessions/sipnet-handoff.md](sessions/sipnet-handoff.md) | Appendix (unofficial) - SIPNET handoff |
| [metadata.md](metadata.md) | Column / product dictionary index |
| [setup_env.sh](setup_env.sh) | Portable env template |

Component READMEs live next to code (`traits/`, `events/`, `hls/`, `phenology/`,
`tillage/`, `landiq-gapfill/`). Parent overview: [../README.md](../README.md).

## Structural note (sessions 0-3)

Sessions are grouped as Setup, LandIQ, HLS events (MSLSP + NDTI), and
fertilizer + irrigation. There is no separate Session 4; irrigation is under
Session 3. SIPNET formatting is an unofficial appendix, not a training session.
