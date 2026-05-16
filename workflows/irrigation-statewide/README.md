# Statewide irrigation events workflow

This generates PEcAn event files for irrigation events across all of California.
The spatial unit is harmonized LandIQ parcels.

The workflow uses `targets` for reproducibility, scalability, and incremental execution.

# Setup

This code assumes that you are running from the PEcAn root directory.
To ensure discovery of the targets script and store directories, you will need to set the `TAR_CONFIG` environment variable to point to the `_targets.yaml` file in this directory.
One way to do this is to create a `.Renviron` file in the PEcAn project root with the following contents:

```
TAR_CONFIG=workflows/irrigation-statewide/_targets.yaml
```

If you are running this code on the BU SCC, all the paths have already been preconfigured for you.
If you are running the code on another system, or if the paths have changed, modify the `config_paths.yml` file accordingly.

# Execution

This code ships with three different configurations (defined in `config.yml`) of the irrigation pipeline:

- `small` (default) --- 1000 randomly selected parcels split into batches of 100 parcels each. The code will run locally, using as many CPUs as are defined by the `NSLOTS` environment variable (or 1 CPU, if `NSLOTS` is unset).
- `medium` --- 10,000 randomly selected parcels split into batches of 1000 parcels each. This will run across `n_remote_workers` (set to 15) SGE array jobs.
- `all` --- This will run all (~600,000) parcels in California in batches of 5000 parcels each. This will run on 60 SGE array jobs.

These values can be modified by modifying the `config.yml` file.

To run a specific configuration, set the `TAR_PROJECT` environment variable accordingly and run `Rscript -e 'targets::tar_make()'`.
For example, to run the full statewide pipeline (all 600K parcels), you can use a command like:

```
TAR_PROJECT=all Rscript -e "targets::tar_make()"
```
