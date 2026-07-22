# LandIQ ADOY, ADOY_SEN, ADOY_EMRG — definitions and relation to MSLSP

## Actual LandIQ definitions (from metadata)

**Source:** `LandIQ_data/crops_all_years_metadata.csv` (malmborg); DWR/LandIQ shapefile metadata (2020 PDF).

| Column      | Definition (metadata) |
|------------|------------------------|
| **ADOY**   | Adjusted day of year for **peak NDVI** for crop per season (current year). |
| **ADOY_EMRG** | Adjusted DOY for **peak NDVI for emerging crop**. |
| **ADOY_SEN**  | Adjusted day of year **for senescing crop**. |

From the **2020 DWR/LandIQ PDF:**

- **EMRG_CROP** = crop that at **end of WY** (e.g. Sept 2020) is “emerging”; that crop **reaches peak NDVI in the following WY** (2021). So the *date* in ADOY_EMRG is the **peak** of that (next-year) crop, not an “emergence” or start-of-season date.
- **SEN_CROP** = crop that at **beginning of WY** (e.g. Oct 2019) is “senescing”; that crop **reached peak NDVI in the previous WY** (2019). So the *date* in ADOY_SEN is the **peak** of that (prior-year) crop, not a “senescence” or end-of-season date.

So:

- **ADOY** = peak of the **current** season’s crop (same as MSLSP Peak for that cycle).
- **ADOY_EMRG** = **peak** of the **emerging** crop (peak is in the **next** WY) — not “emergence date” (OGI analog).
- **ADOY_SEN** = DOY **for the senescing crop** (that crop’s peak was in the **previous** WY) — not “senescence date” (OGMn analog).

So **all three are peak (or peak-related) dates**, for **different temporal windows** (prior-year crop, current season, next-year crop), not three points on one curve (start / peak / end).

## How far off are SEN and EMRG from ADOY? (2021, same row)

From the parquet (rows that have ADOY and at least one of ADOY_SEN, ADOY_EMRG):

| Offset            | Mean (days) | SD   | Min  | Max  | n    |
|------------------|------------|-----|------|------|------|
| ADOY_SEN − ADOY  | **−154.7** | 133 | −456 | 90   | 21821 |
| ADOY_EMRG − ADOY | **+96.2**  | 199 | −273 | 453  | 21821 |

- **ADOY_SEN** is usually **before** ADOY (mean −155 days). That fits “peak of the crop that is now senescing” (prior year peak).
- **ADOY_EMRG** is usually **after** ADOY (mean +96 days). That fits “peak of the crop that is now emerging” (next year peak).

So SEN and EMRG are not “before peak” and “after peak” on the **same** cycle; they are peaks of **other** segments (prior/next year). That’s why they don’t line up with OGI/OGMn.

## Conclusion

- **LandIQ does not provide** “emergence” (OGI) or “senescence” (OGMn) dates in these columns.
- **ADOY_EMRG** = peak NDVI of the **emerging** (next-year) crop.
- **ADOY_SEN** = DOY for the **senescing** (prior-year) crop (that crop’s peak was last year).
- So we **cannot** treat EMRG as OGI and SEN as OGMn for the same cycle; the workflow is correct to use **ADOY vs [OGI, OGMn]** (peak-in-window) for matching.

## Using SEN/EMRG to help with multiple seasons/cycles

- **Current workflow:** When ADOY is missing we *do not* use EMRG or SEN. We assign by season priority and tie-break by mslsp_cycle (woody and non-woody). Output: `no_adoy_woody_tiebreak` when PFT is woody, else `no_adoy_recorded`.
- **Next step (not in scripts yet):** Consider using **ADOY_EMRG** in a cycle window when ADOY is missing (non-woody) to disambiguate which cycle a season maps to. Explore with `explore_sen_emrg_matching.R` and validate a sample before enabling in `match_landiq_mslsp.R`.
- **ADOY_SEN** is not used for matching (peak of prior season; rarely in cycle windows in practice).
- **When ADOY is present:** Matching is ADOY-in-window + nearest Peak.

## What to do with this (practical takeaways)

- **Current workflow:** The script uses ADOY when present; when ADOY is missing it uses tie-break only (no EMRG/SEN). Using EMRG as fallback is a possible next step, not in the code yet.
- **MULTIUSE:** D = double (per LandIQ documentation). We prioritize season 1 for D/M when assigning.
- **Woody + SEN/EMRG:** Almost all woody rows that have SEN/EMRG are T19 (Bush berries). We don’t use them for matching; the breakdown is mainly for context.
- **Bottom line:** Use the notes to interpret QC and LandIQ columns. EMRG/SEN exploration is for a future workflow change if desired.
