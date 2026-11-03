# SIPNET restart workflow for events

This is a demonstration of a workaround for running SIPNET with event files that include crop changes/rotations.

## Execution

1. Install PEcAn locally (`make install`, etc.).
2. Inspect the `config.yml` and add/modify paths or other values as needed.
3. Run the numbered R scripts in order.

## Organization

- `config.yml` --- Configuration file for use with the [`config`](https://rstudio.github.io/config/index.html) package. Mostly used for setting machine-specific paths to various inputs. This is pre-populated with inputs for @ashiklom 's local machine and the BU GEO cluster (under the `default` profile).

- `01-prepare-events.R` --- Read raw events data (in parquet format) and write out ensembles of PEcAn `event.json` files and SIPNET-specific versions thereof
- `02-prepare-settings.R` --- Generate a PEcAn `settings.xml` file populated with machine-specific paths and appropriate configurations.
- `03-run-sipnet.R` --- Run the workflow sequentially.
    - Almost all of the functionality is implemented in `utils.R`.
- `04-plot-outputs.R` --- Load outputs (using `PEcAn.utils::read.output`) and visualize
