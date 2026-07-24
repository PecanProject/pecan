# Session 1 - Add LandIQ 2024 and gap-fill 2023 + 2024

**Goal:** bring the **2024** LandIQ statewide crop map into the multi-year
parcel table, then gap-fill crop identity for **2023 + 2024** so every parcel
has usable main-season crop information for later sessions.

LandIQ is California's field-level crop mapping product (class, subclass, and
peak-greenness timing). It is the source of **crop identity** for CCMMF
inventory modeling. This session covers download, legend checks, geometry
harmonization, and gap-fill only. Full stage order lives in
[pipeline.md](../pipeline.md).

**Prereq:** [Session 0](00-environment.md) done.

Paths used here (set by `setup_env.sh`):

| Role | Path |
|------|------|
| LandIQ shapefile | `$CCMMF_ROOT/data_raw/cadwr_land_use/landiq_shapefiles/` |
| Harmonized geometry | `$CCMMF_LANDIQ_V4` |
| Gap-filled product | `$CCMMF_LANDIQ_GAPFILL_PRODUCT` |
| cadwr-landuse repo | `$HOME/src/cadwr-landuse` |
| landiq-gapfill package | `$LANDIQ_GAPFILL_ROOT` |

Operator docs (how the code works): [pipeline.md](../pipeline.md) |
[cadwr-landuse](https://github.com/ccmmf/cadwr-landuse) |
[landiq-gapfill/README.md](../../landiq-gapfill/README.md) |
[metadata.md](../metadata.md).

---

## 1.1 Download LandIQ 2024

This is the only manual download on the LandIQ path. Later steps expect the
shapefile under `landiq_shapefiles/`.

| Item | Value |
|------|--------|
| Portal | [CNRA - Statewide Crop Mapping](https://data.cnra.ca.gov/dataset/statewide-crop-mapping) (California Natural Resources Agency) |
| Resource | **PROVISIONAL - 2024 Statewide Crop Mapping GIS Shapefile** |
| Direct ZIP | [`i15_crop_mapping_2024_provisional.zip`](https://data.cnra.ca.gov/dataset/6c3d65e3-35bb-49e1-a51e-49d5a2cf09a9/resource/1a1c259c-4279-4868-a25f-b1f71665ca25/download/i15_crop_mapping_2024_provisional.zip) |

Prefer the **shapefile** ZIP. From the same CNRA page, also download the current
**Land Use Legend** PDF (needed for Sec. 1.2). Unpack so files land here:

```text
$CCMMF_ROOT/data_raw/cadwr_land_use/landiq_shapefiles/
  i15_Crop_Mapping_2024_Provisional_SHP/
    i15_Crop_Mapping_2024_Provisional.shp   # + .dbf .shx .prj ...
```

You can unpack by hand into that layout, or run:

```bash
source "$CCMMF_CODE/documentation/setup_env.sh"

STAGING=$CCMMF_ROOT/data_raw/cadwr_land_use/_staging_${TARGET_YEAR}
DROP=$CCMMF_ROOT/data_raw/cadwr_land_use/landiq_shapefiles
FOLDER=i15_Crop_Mapping_${TARGET_YEAR}_Provisional_SHP
STEM=i15_Crop_Mapping_${TARGET_YEAR}_Provisional

mkdir -p "$STAGING" "$DROP/$FOLDER"
cd "$STAGING"

curl -L -o i15_crop_mapping_2024_provisional.zip \
  'https://data.cnra.ca.gov/dataset/6c3d65e3-35bb-49e1-a51e-49d5a2cf09a9/resource/1a1c259c-4279-4868-a25f-b1f71665ca25/download/i15_crop_mapping_2024_provisional.zip'

unzip -o i15_crop_mapping_2024_provisional.zip -d unpack
cp -a unpack/${STEM}.* "$DROP/$FOLDER/"
test -f "$DROP/$FOLDER/${STEM}.shp" && echo "OK: $DROP/$FOLDER/${STEM}.shp"
```

Sec. 1.3 points cadwr-landuse at this same `landiq_shapefiles/` folder.

---

## 1.2 Crop class/subclass harmonization (legend QC)

LandIQ legend codes differ by era (**2014**, **2016-2020**, **2021+**). The
crop-code lookup maps each stored `(CLASS, SUBCLASS)` pair onto one harmonized
set so later years stay comparable. Compare the legend PDF from Sec. 1.1 to the
lookup and add rows only if codes changed.

**For this training run, 2024 is unchanged** - no lookup edits.

Lookup: [`LandIQ_cropCode_lookup_table.csv`](../../landiq-gapfill/data/LandIQ_cropCode_lookup_table.csv)

---

## 1.3 Harmonize geometry

cadwr-landuse assigns stable `parcel_id`s across years and writes the multi-year
crop table plus geometry. Point it at the shapefiles from Sec. 1.1. Use the
default (`main`) branch; it auto-discovers years including **2024**.

Follow the runbook in the [cadwr-landuse README](https://github.com/ccmmf/cadwr-landuse)
(and [`docs/harmonization_v0.1.md`](https://github.com/ccmmf/cadwr-landuse/blob/main/docs/harmonization_v0.1.md)).
Do not paste the full pixi step list here.

When finished, publish the cadwr `03-final/` output and point env at it:

```bash
export CCMMF_LANDIQ_V4=$CCMMF_ROOT/LandIQ-harmonized-v4.1
```

Confirm `year == $TARGET_YEAR` rows exist in `$CCMMF_LANDIQ_V4/crops_all_years.parq`
before gap-fill.

Column dictionary: [crops_all_years_metadata.csv](../../landiq-gapfill/data/crops_all_years_metadata.csv)
(cadwr base columns plus gap-fill provenance).

---

## 1.4 Gap-fill 2023 + 2024

Some parcels lack crop information (`CLASS` / `SUBCLASS`) or peak-greenness day
(`ADOY`, adjusted day of year). Gap-fill focuses on **season 2** (the main
growing season) so every parcel has a usable main-crop record for phenology and
events.

Run the prior year and the new year together
(`${PRIOR_YEAR},${TARGET_YEAR}`). The prior year can then use the new series as
neighbor context. One command does both fills and includes USDA Cropland Data
Layer (CDL) download and extract for those years:

1. **CLASS / SUBCLASS** from CDL parcel fractions plus LandIQ history.
2. **ADOY** from the same parcel in other years, or typical peak day for that
   crop and county.

```bash
$LANDIQ_GAPFILL_ROOT/run_gapfill.sh ${PRIOR_YEAR},${TARGET_YEAR}
export CCMMF_LANDIQ_V4=$CCMMF_LANDIQ_GAPFILL_PRODUCT
```

Details (flags, provenance, rebuilds):
[landiq-gapfill/README.md](../../landiq-gapfill/README.md) |
[cdl_fractions_metadata.csv](../../landiq-gapfill/data/cdl_fractions_metadata.csv)

---

## 1.5 Checklist

- [ ] Sourced `setup_env.sh` from the clone
- [ ] Downloaded and unpacked 2024 provisional shapefile under `landiq_shapefiles/`
- [ ] Confirmed 2024 legend unchanged (no lookup edit)
- [ ] Harmonized geometry; published to `$CCMMF_LANDIQ_V4`
- [ ] Confirmed `year == 2024` in `crops_all_years.parq`
- [ ] Ran `$LANDIQ_GAPFILL_ROOT/run_gapfill.sh ${PRIOR_YEAR},${TARGET_YEAR}`
- [ ] Pointed `CCMMF_LANDIQ_V4` at `$CCMMF_LANDIQ_GAPFILL_PRODUCT`

**Next:** [Session 2 - Phenology](02-phenology.md).

**Spine:** [pipeline.md](../pipeline.md).
