---
title: "Readme_FAISS"
output: html_document
---

# Global range site-selection clustering 
FAISS K-means clustering for PEcAn SDA site selection at global range.
Key difference from the existing workflow: faster in clustering and able to do full pixel table directly, so no subsampling for candidates. Also solve the potential memory limits encountered on R earlier.
Total carbon-weighted site allocation giving sites with more SOC and AGB a higher probability being selected(1.5 more likely.
Higher latitude with less weights to deal with 2D map distortions. 

## Requirements

Python >= 3.11, `faiss-cpu`, `numpy`, `pandas`;
`matplotlib` and `cartopy` if `MAKE_MAP = True`

## Inputs

One CSV (`ecoClim_global.csv` for reference), one row per candidate pixel:

| column | role |
|---|---|
| `LC` | land-cover class |
| `longitude`, `latitude` | coordinates |
| `SOC`, `AGB` |  Fro Total carbon weighting, not clustering features |
| `GEDI` | GEDI sampling-weight boost |
| `t2m, ssrd, tp, d2m, PH, N, Sand, elevation` | clustering features |



Contact: Xiaolin(Eric) Tian (akemih@bu.edu), Dietze Lab, 	Dr. Michael Dietze, (dietze at bu.edu), Boston University.