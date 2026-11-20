# Neon soil data mining directory

This folder contains scripts for comparing SDA outputs to NEON data and finding functional relationships between soil respiration and other variables


## Data Analysis Scripts

`soilcarbon_validation.Rmd` - compares organic soil carbon from SDA output and NEON data

`soilrespiration_validation.Rmd` - compares soil respiration from SDA output and neonSoilFlux package

`soiltemperature_validation.Rmd` - compares soil temperature from SDA output and neonSoilFlux package

`soilmoist_validation.Rmd` - compares soil moisture from SDA output and neonSoilFlux package

`Root_validation.Rmd` - compares coarse and fine root carbon from SDA output and NEON data


Note: The soil respiration file needs to be ran before temperature, moisture, or root for full validation

`soil_resp_markdown.Rmd` - This is an old version of soil respiration file



## Script to pull SDA model output
`neon_model_processing.R`


## Description

- Each file has basic error calculations and validations between the SDA and NEON
- Temperature and moisture validation scripts look at the functional relationship between soil respiration and each variable

## To Do

- I had started looking at validating the input values and the bias, but didn't finish that
- Root and soil carbon validations do not yet include functional relationship equations

