# AmeriFlux Benchmarking Example

This example demonstrates how to evaluate model outputs against AmeriFlux tower observations using the `PEcAn.benchmark` toolkit.

## Observations Source

Observations are retrieved directly from the `ccmmf/cal-val-data` repository:
- URL: `https://raw.githubusercontent.com/ccmmf/cal-val-data/refs/heads/main/data/observations.csv`

Until release `v0.1.0` of `cal-val-data` is published, reading from this URL requires repository access permissions. The script incorporates a graceful fallback if the URL is unreachable.

## Execution

```bash
Rscript examples/benchmarks/ameriflux/run_ameriflux_benchmark.R
```
