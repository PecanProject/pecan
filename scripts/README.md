# PEcAn Scripts Directory

This folder contains helper scripts used for building, testing, running, and managing the PEcAn ecosystem.

---

## Build and Installation Scripts

- `compile.sh` – Compilation helper
- `quickbuild.R` – Loads all PEcAn packages in `dev_mode()` using `devtools`
- `install_pecan.sh` – Full PEcAn installation script
- `install_shiny_deps.R` – Installs required dependencies for Shiny apps
- `ncsa-build.sh` – Builds PEcAn on the NCSA system

---

## Documentation and Dependencies

- `dependencies.R` – Generates package dependency graph
- `generate_dependencies.R` – Advanced dependency analysis tool
- `confirm_deps.R` – Confirms package dependencies
- `updateVersion.sh` – Bumps version numbers
- `check_with_errors.R` – Verifies documentation or build errors
- `get_orphaned_functions.py` – Detects unused functions

---

##  Workflow and Test Scripts

- `workflow.bm.R` – Benchmark workflow
- `workflow.pda.R` – Parameter data assimilation workflow
- `workflow.pda.recover.r` – Recovery workflow for PDA
- `workflow.treering.R` – Tree-ring data assimilation
- `workflow.wcr.assim.R` – Assimilation example for WCR
- `Rfcn.R` – Execute R functions remotely
- `cleansettings.R` – Script for sanitizing settings files
- `efi_data_process.R` – EFI data workflow
- `EFI_metprocess.R` – EFI met processing
- `EFI_workflow.R` – Full EFI workflow demo
- `HARV_metdownload_efi.R` – Met download script for Harvard Forest (EFI)

---

##  Database and Data Scripts

- `add.data.sh` – Loads input data into the BETY database
- `add.models.sh` – Loads model metadata into BETY
- `add.util.sh` – Helper functions used by add scripts
- `backup.bety.sh` – Backs up the BETY database

---

##  Metadata and Configuration

- `create-hooks.sh` – Sets up git hooks
- `sshkey.sh` – SSH key configuration
- `syncgit.sh` – Git sync helper
- `time.sh` – Benchmarking script
- `docker_rebuild.sh` – Rebuilds Docker containers
- `cron.sh` – Cron job runner
- `thredds.sh` – THREDDS data publishing script
- `find.string.sh` – Searches for strings in files

---

##  Data Preparation and Examples

- `dataprep_10_site.csv` – Example CSV for preparing 10 sites
- `dataprep_10_sites.csv` – Duplicate of above (maybe remove one?)

---

##  Other

- `.gitignore` – Git ignore rules
- `README.md` – This file

---

##  Note

If you remove or rename any scripts in the future, **please update this README** to keep documentation in sync.

---
