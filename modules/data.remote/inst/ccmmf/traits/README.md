# Planting and harvest lookups

SIPNET needs parcel-level initial C and N pool sizes at planting and, at harvest/termination, the partitioning of biomass into removed yield versus residues. Hierarchically structured lookup tables supply crop-specific SLA, allometric traits, and C:N, plus harvest fractions. `apply_planting.R` writes LAI then C/N onto the overlay. `apply_harvest.R` writes removal vs residue fractions. `make_events_statewide.R` copies SIPNET columns into event files. Harvest lookups are fractions, not EVI. Commands: [Session 2](../documentation/sessions/02-phenology.md). Event copy and skip overlap: [events/README.md](../events/README.md). Apply table columns: [data/planting_apply_metadata.csv](data/planting_apply_metadata.csv), [data/harvest_apply_metadata.csv](data/harvest_apply_metadata.csv).

## Assumptions

Both lookups use the 2021 LandIQ legend only. Agricultural parcels: `is_agricultural == TRUE`. TRY species matching uses only `AccSpeciesName` values listed in LandIQ `latin_names` (no genus fallback). Pool fallback for planting, per TraitKey: TRY subclass -> TRY class -> lit subclass -> lit class -> TRY PFT -> default PFT. Harvest: subclass -> class -> pft, keyed by PFT + `destructive`. Defaults are programmatic `source=default` rows (the pool does not fill missing numbers beyond those rows). Missing SLA stops the C/N chain (all C/N `NA`); missing stem or root traits leave those organs `NA`.

The 15% onset of greenness increase (OGI) is the effective planting date. SIPNET has no seed stage, so pools are initialized at seedling size (leaf biomass from 15% of peak greenness, stem and roots allometrically from leaf). This is not a growing-season integrator and not a harvest biomass model. SIPNET plant here is leaf + stem + root (no fruit pool). Pools are kg/m2 ground area. Carbon mass fractions are hardcoded: 0.47 of dry mass for leaf, stem, and fine root; 0.50 for coarse root. There is a lag between actual planting and the first detection of foliage.

Planting apply skips hay, woody, and PFT `other`. Harvest apply skips PFT `other` and young woody (`SPECOND=Y` or `CLASS=YP`). Hay and woody harvest is dated at OGD. Orchard clearing is not a separate PFT: `PFT=woody` and `destructive=TRUE`, dated at OGMn. CLASS-level look-ahead (LandIQ season 2, year -> year+1): when a mature woody CLASS is replaced by a different CLASS, young woody, or non-woody, emit one destructive harvest using the prior stand crop code and drop the routine woody harvest for that parcel.

`initialize_planting()` uses Mourad LAI unless the caller already passed a finite `LAI` (then EVImax is ignored). `k` is 0.15 for every mapped PFT (`row`, `rice`, `hay`, `woody`). Other / missing PFT -> LAI = `NA`. CLASS is not used in the LAI function. Negative EVI is floored at 0. No additional LAI clamp (`lai_min = 0`, `lai_max = Inf`).

## Lookups on disk


| File under `$PLANT_TRAITS_DIR` | Contents                                                                                                                                      |
| ------------------------------ | --------------------------------------------------------------------------------------------------------------------------------------------- |
| `planting_lookup.csv`          | Planting traits at subclass/class/pft (`try` / `literature` / `default`)                                                                      |
| `harvest_lookup.csv`           | Removal vs residue fractions, including woody clearing (`destructive=TRUE`); `source` = ludemann / holos / swat / ipcc / literature / default |


Builders (not the training path): `build_planting_lookup.R` (TRY allocation `.txt` + LandIQ lookup + literature rows); `build_harvest_lookup.R` (`harvest_fractions_long.csv` + in-script PFT defaults). Optional rebuild of the long file: `write_harvest_fractions_long.R`. LandIQ lookup columns required: `PFT`, `CLASS`, `SUBCLASS`, `CLASS_desc`, `SUBCLASS_desc`, `is_agricultural`, plus `latin_names` for TRY.

## Planting math: EVImax to C/N

Code: `[lai_from_mslsp.R](lai_from_mslsp.R)` then `[pool_calculations_from_lookup.R](pool_calculations_from_lookup.R)`, applied by `[apply_planting.R](apply_planting.R)`. Seasonal peak EVI2 is multiplied by 0.15 to represent EVI2 at planting (OGI is the date of 15% greenness). LAI is then estimated from that EVI2 (Mourad et al. 2020). LAI is converted to leaf biomass using SLA, and leaf biomass is the reference for the other C and N pools (leaf, stem, fine root, coarse root) through trait-based mass fractions and C:N. That keeps pool estimates linked to observed canopy conditions rather than fixed defaults.

### Trait keys and units


| TraitKey | Name                                              | Role                                               | Unit as used                                             |
| -------- | ------------------------------------------------- | -------------------------------------------------- | -------------------------------------------------------- |
| `SLA`    | specific leaf area (TRY 3115/3116/3117 collapsed) | `M_leaf = LAI / SLA`                               | m2/kg (same as mm2/mg); reject `<= 0` or `> 200`         |
| 110      | leaf weight ratio (LWR)                           | leaf / plant; with SMF gives stem:leaf             | g/g in (0, 1]; values in (1, 100] treated as percent     |
| 136      | stem mass fraction (SMF)                          | stem / plant                                       | g/g, same percent rule                                   |
| 9        | root:shoot (RS)                                   | `M_root = RS * M_shoot`; also implies RMF          | g/g; keep if in [0.01, 10]                               |
| 470      | root mass fraction (RMF)                          | root / plant; used when RS is missing              | g/g, same percent rule                                   |
| 2005     | fine-root mass fraction                           | relative fine share of **root**, not `f * M_plant` | g/g of whole plant in TRY/lit; normalized over 2005+1534 |
| 1534     | coarse-root mass fraction                         | relative coarse share of **root**                  | same                                                     |
| 1019     | coarse:fine root mass ratio                       | fills 2005/1534 when those are missing             | g/g; keep if in [0.01, 10]                               |
| 14       | leaf N                                            | `N_LEAF = M_leaf * (value * 1e-3)`                 | mg N / g DM -> kg N / kg DM                              |
| 146      | leaf C:N                                          | fallback `N_LEAF = C_LEAF / CN`                    | g/g                                                      |
| 165      | stem C:N                                          | `N_STEM = C_STEM / CN`                             | g/g                                                      |
| 1055     | root C:N                                          | stem/fine/coarse N fallback; coarse CN mix         | g/g                                                      |
| 2057     | fine-root C:N                                     | `N_FINEROOT = C_FINEROOT / CN`                     | g/g                                                      |


RS and RMF are two views of the same split. If only one is present: `RMF = RS / (1 + RS)` and `RS = RMF / (1 - RMF)` (requires RMF < 1).

### 1. EVImax -> LAI

`compute_lai_from_mslsp(mslsp_EVImax, pft)`. Mourad et al. (2020) map EVI to LAI (`a` and `b` only):

```text
a = 2.92
b = 0.43
LAI = (max(0, a * sqrt(EVI) - b))^2
```

Planting is not peak canopy, so this pipeline scales `mslsp_EVImax` first, then runs Mourad on that scaled value:

```text
k = 0.15
EVI' = max(0, mslsp_EVImax)
EVI_planting = k * EVI'
LAI = (max(0, a * sqrt(EVI_planting) - b))^2
```

`k` is the 15% greenness scale at OGI, not a Mourad coefficient. Planting dates still come from MSLSP OGI (or gap-fill). `k` only sizes LAI for pool init. To change `a`, `b`, or `k`, edit `lai_from_mslsp.R`.

LAI fallback when EVImax is missing (gap-filled dates, empty cycles): from other same-year rows that do have EVImax, take mean EVImax by CLASS x PFT, run the same LAI function, then look up LAI as CLASS+PFT -> PFT -> global mean (`lai_source = lai_fallback`). If even that is missing, the planting row is skipped. See `planting_lai_fallbacks()` in `[planting_apply.R](planting_apply.R)`.

Numeric check: one 10TEK 2023 planting row (parcel `221093`, LandIQ `G6` miscellaneous grain and hay, season 2, `lai_source = mslsp_evi`). Overlay `mslsp_EVImax = 0.564`.

```text
EVI' = 0.564
k * EVI' = 0.15 * 0.564 = 0.0846
sqrt(0.0846) ~ 0.291
a * sqrt = 2.92 * 0.291 ~ 0.850
term = 0.850 - 0.43 = 0.420
LAI = 0.420^2 ~ 0.176 m2/m2
```

That matches `assigned_year=2023_planting.parquet` for this parcel (`LAI = 0.176`). The LAI is small because planting is 15% of peak greenness, not peak canopy. The rest of the pools for this row are in the worked example after the nitrogen step.

### 2. LAI -> leaf dry mass

`M_leaf = LAI / SLA` after `as_sla_m2_kg()`. If SLA is missing, `initialize_planting()` still returns the row (with the LAI it computed) but every C and N pool is `NA`.

### 3. Stem and shoot

Stem:leaf ratio `alpha` (kg stem / kg leaf), first match that works:

1. Both SMF (136) and LWR (110) present and LWR > 0: `alpha = SMF / LWR`
2. Else SMF and RMF present and `1 - SMF - RMF > 0`: implied LWR = `1 - SMF - RMF`, `alpha = SMF / LWR`
3. Else LWR and RMF present and `1 - LWR - RMF > 0`: implied SMF = `1 - LWR - RMF`, `alpha = SMF / LWR`

Then `M_stem = alpha * M_leaf` (NA if alpha cannot be formed) and `M_shoot = M_leaf + M_stem` (NA if stem is NA).

### 4. Root mass

First match that works:

1. If shoot and RS are both known: `M_root = RS * M_shoot`
2. Else if LWR > 0 and RMF known: `M_plant = M_leaf / LWR`, then `M_root = RMF * M_plant`

When both RS and LWR/SMF exist, the two fraction sets are not forced onto one closed mass balance. The code prefers (1) for roots, so `M_leaf / (M_shoot + M_root)` may differ slightly from LWR. If stem is NA, path (1) cannot run; path (2) can still fill `M_root` from leaf + LWR + RMF, while `C_STEM` stays NA. `M_plant = M_shoot + M_root` (NA unless both pieces exist).

### 5. Fine vs coarse (split of M_root only)

TRY 2005 and 1534 are stored as whole-plant fractions. Applying them as `f * M_plant` would resize the root pool and break RS. After `M_root` is fixed, they are used only as a relative split: `fine_share = f_fine / (f_fine + f_coarse)`, `M_fine = fine_share * M_root`, `M_coarse = coarse_share * M_root`.

Filling missing 2005/1534, in order:

1. Both missing, 1019 and RMF known: `fine_of_root = 1 / (1 + 1019)`, then `f_fine = RMF * fine_of_root`, `f_coarse = RMF * (1 - fine_of_root)`
2. Else RMF known and only one of 2005/1534 known and less than RMF: the missing plant fraction is `RMF - the known one`
3. Else still missing but 1019 known (no RMF needed): use `f_fine = 1`, `f_coarse = 1019` as a ratio to normalize (`fine_share = 1/(1+1019)`)

If the split still cannot be formed, `C_FINEROOT` and `C_COARSEROOT` stay `NA` even when `M_root` is known. Rice and row have PFT `source=default` rows 2005=0.99, 1534=0.01 (nearly all fine). Woody and hay have no such default.

### 6. Carbon pools

`C_LEAF = M_leaf * 0.47`, `C_STEM = M_stem * 0.47`, `C_FINEROOT = M_fine * 0.47`, `C_COARSEROOT = M_coarse * 0.50`. Any mass that is NA stays NA in C.

### 7. Nitrogen pools

Leaf (prefer tissue N, then C:N): `Nleaf_frac = leaf_N_mg_g * 1e-3` (Trait 14; reject <= 0 or > 100 mg/g). `N_LEAF = M_leaf * Nleaf_frac` if that exists, else `C_LEAF / CN_leaf` (Trait 146, CN > 0).

Stem (C:N cascade): Trait 165, else 1055, else 146: `N_STEM = C_STEM / CN_used`.

Fine root: Trait 2057, else 1055, else 165, else 146: `N_FINEROOT = C_FINEROOT / CN_fine_use`.

Coarse root CN (`derive_CN_coarse()`), treating 2005/1534 as the mix of the root pool and requiring all of CN_root (1055), CN_fine (2057), f_fine, f_coarse:

```text
f_root        = f_fine + f_coarse
f_fine_root   = f_fine / f_root
f_coarse_root = f_coarse / f_root
# 1/CN_root = f_fine_root/CN_fine + f_coarse_root/CN_coarse
CN_coarse = f_coarse_root / (1/CN_root - f_fine_root/CN_fine)
```

If that mix is not finite and positive, fall back CN_root -> CN_fine -> CN_stem (165) -> CN_leaf (146). Then `N_COARSEROOT = C_COARSEROOT / CN_coarse`.

### Worked example (G6 parcel)

Same row as the EVI check: parcel `221093`, `G6`, `LAI = 0.176`. Lookup values actually used (TRY subclass unless noted): SLA = 26.64 m2/kg, LWR (110) = 0.430, SMF (136) = 0.230, leaf N (14) = 31.63 mg/g, stem C:N (165) = 72.7 (literature subclass), fine-root C:N (2057) = 49.2 (literature subclass). Root:shoot (9) = 0.145 from TRY class `G` (no subclass TRY 9). Fine/coarse (2005/1534) = 0.99 / 0.01 from PFT `source=default`.

```text
M_leaf  = 0.176 / 26.64 ~ 0.00661 kg/m2
alpha   = 0.230 / 0.430 ~ 0.534
M_stem  = 0.534 * 0.00661 ~ 0.00353
M_shoot = 0.00661 + 0.00353 ~ 0.01013
M_root  = 0.145 * 0.01013 ~ 0.00147
M_fine  = 0.99 * 0.00147 ~ 0.00146
M_coarse= 0.01 * 0.00147 ~ 0.0000147

C_LEAF       = 0.00661 * 0.47 ~ 0.00310
C_STEM       = 0.00353 * 0.47 ~ 0.00166
C_FINEROOT   = 0.00146 * 0.47 ~ 0.000686
C_COARSEROOT = 0.0000147 * 0.50 ~ 0.0000074

N_LEAF     = 0.00661 * 0.0316 ~ 0.000209
N_STEM     = 0.00166 / 72.7 ~ 0.0000228
N_FINEROOT = 0.000686 / 49.2 ~ 0.0000139
```

Those C and N values match the planting parquet row. This is typical OGI-scale biomass, not a mid-season canopy.

`diagnostics = TRUE` adds `sla_src` / `src_*` (`subclass` | `class` | `pft`) and `source_*` (`try` | `literature` | `default`), plus `lai_k`, `alpha_stem_leaf`, `lwr_used`, `smf_used`, `rs_used`, `root_split_src`.

## Harvest fractions

`initialize_harvest_from_lookup()` does not use LAI or EVI. It copies `AGB_REMOVED`, `AGB_LITTER`, `BGB_REMOVED`, `BGB_LITTER` from `harvest_lookup.csv` (subclass -> class -> pft, keyed by PFT + `destructive`). Those values are the crop-specific percent of biomass removed (harvest index) vs percent entering litter, applied to standing biomass at the harvest/termination date. For example, when an annual crop is harvested essentially all of the leaf and stem is either removed or becomes litter, while roots generally become litter (except root crops). For a perennial orchard, a much smaller fraction of each pool is removed or returned, and harvest plus litter is far less than 100%.

## Apply scripts


| Script                            | Writes                                                               |
| --------------------------------- | -------------------------------------------------------------------- |
| `apply_planting.R YEAR`           | `$MATCHED_DIR/assigned_year=Y_planting.parquet` (LAI in memory only) |
| `apply_harvest.R YEAR`            | `$MATCHED_DIR/assigned_year=Y_harvest.parquet`                       |
| `build_planting_lookup.R`         | `planting_lookup.csv`                                                |
| `build_harvest_lookup.R`          | `harvest_lookup.csv`                                                 |
| `lai_from_mslsp.R`                | `compute_lai_from_mslsp()`                                           |
| `pool_calculations_from_lookup.R` | `initialize_planting()` / `initialize_harvest_from_lookup()`         |
| `planting_apply.R`                | LAI and pool table builders                                          |


Public: `initialize_planting()` (finite `LAI` or `mslsp_EVImax`, plus LandIQ `code` or both `class` and `subclass`). Internal: `planting_pools_from_lookup()`.