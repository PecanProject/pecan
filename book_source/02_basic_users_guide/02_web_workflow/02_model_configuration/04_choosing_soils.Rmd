### Selecting a soils product

Many models have requirements for soils information, which may include: site-specific soil texture and depth information; soil biogeochemical initial conditions (e.g. soil carbon and nitrogen pools); soil moisture initial conditions; and soil thermal initial conditions.

As with [Choosing initial vegetation], we eventually hope to develop data standards, soils workflows, and spin-up tools, but at the moment model requirements need to be met by [inserting Input data](../developers_guide/How-to-insert-new-Input-data.html) into the database or using files that have already been uploaded.

#### Soil texture, depth, and physical parameters

A PEcAn-standard netCDF file format exists for soil texture, depth, and physical parameters, using PEcAn standard names that are largely a direct extention of the CF standard. A table of standard names and units can be listed using `PEcAn.data.land::soil.units()` with no arguments.

```{r, echo = FALSE, eval = FALSE}
knitr::kable(PEcAn.data.land::soil.units())
```

More detailed information on how PEcAn processes inputs can be found on our [Input Conversion] page.
