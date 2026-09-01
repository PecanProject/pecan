# Salinas Organic Cropping Systems (SOCs) Ensemble Benchmark Example

This example demonstrates how to evaluate the Salinas Organic Cropping Systems (SOCs) model ensemble against observational Soil Organic Carbon (SOC) stocks using the decoupled `PEcAn.benchmark` validation toolkit.

## Files

- **`ensemble_output.csv`**: Model ensemble predictions in standard EFI long format (67,200 rows: 50 members $\times$ 8 management systems $\times$ 84 monthly timesteps from 2005 to 2011 for `TotSoilCarb` and `AGB`). This is an external/generated data file produced by `extract_salinas_ensemble_output.R` (or retrieved from CCMMF data repositories) and is not tracked in git to keep the repository lightweight.
- **`observations_soc.csv`**: Observational SOC stock data aggregated per system from the Salinas block-level observations (0–30 cm depth integral). Converted to $\text{kg C m}^{-2}$ with mean, standard deviation, and standard error across replicate blocks. Source dataset: `white_salinas_2020` (White et al., 2020).
- **`run_benchmarks.R`**: Benchmark execution script that ingests model ensemble output, aligns temporal resolutions, computes statistical metrics, and renders a Quarto HTML scorecard report.
- **`extract_salinas_ensemble_output.R`**: Utility script used to extract model ensemble outputs from PEcAn model outputs into the standardized EFI long format.

---

## Data Source & Citation

The observational Soil Organic Carbon (SOC) data in `observations_soc.csv` (`dataset_id: white_salinas_2020`) is derived from the USDA-ARS Salinas Organic Cropping Systems (SOCS) long-term trial in Salinas, California.

**Primary Citation:**
> White, K. E., Brennan, E. B., Cavigelli, M. A., & Smith, R. F. (2020). Winter cover crops increase readily decomposable soil carbon, but compost drives total soil carbon during eight years of intensive, organic vegetable production in California. *PLoS ONE*, 15(2), e0228141. [https://doi.org/10.1371/journal.pone.0228141](https://doi.org/10.1371/journal.pone.0228141)

---

## CSV Schema (EFI Long Format)

Model ensemble predictions use the standard EFI long schema:
`scenario, datetime, site_id, lat, lon, pft, parameter, variable, variable_type, prediction`

- **`parameter`**: Ensemble member identifier (1–50).
- **`variable`**: `TotSoilCarb` (soil organic carbon target pool) or `AGB` (aboveground biomass).
- **`prediction`**: Predicted value in $\text{kg C m}^{-2}$.
- **`datetime`**: Monthly timesteps in ISO-8601 UTC format.
- **`site_id`**: Site identifiers (`socs_sys1` through `socs_sys8`).

---

## Execution Instructions

Execute the benchmarking pipeline using `run_benchmarks.R`. By default, the script looks for `ensemble_output.csv` in `examples/benchmarks/salinas_soc_ensemble/`, or accepts an explicit file path as a command-line argument:

```bash
# Default execution (uses examples/benchmarks/salinas_soc_ensemble/ensemble_output.csv if present)
Rscript examples/benchmarks/salinas_soc_ensemble/run_benchmarks.R

# Explicit model CSV path argument
Rscript examples/benchmarks/salinas_soc_ensemble/run_benchmarks.R /path/to/ensemble_output.csv
```

---

## Temporal Alignment & Metric Evaluation

1. **Filtering Target Variable**: Subset model predictions to `variable == "TotSoilCarb"`.
2. **Monthly to Annual Alignment**: Model predictions are output at **monthly** intervals, while SOC observations are measured **annually**. `PEcAn.benchmark::align_by_time()` aligns monthly predictions to annual observation dates within a specified tolerance window (e.g., 6 months).
3. **Metric Calculation**:
   - **Ensemble Mean Metrics**: Point accuracy metrics ($R^2$, RMSE, MAE, BIAS) evaluate ensemble mean predictions against treatment observation means.
   - **Ensemble Spread Metrics**: Probabilistic skill metrics (90% Prediction Interval Coverage and Continuous Ranked Probability Score `CRPS`) evaluate the full 50-member ensemble distribution.
4. **Visualization & Scorecard Rendering**: `metric_timeseries_plot()` renders 90% prediction interval ribbons, ensemble member spaghetti lines, observation error bars, and per-site PMU validation status. `generate_validation_report()` compiles all metrics into an HTML Quarto report (`Salinas_SOC_Validation_Report.html`).
