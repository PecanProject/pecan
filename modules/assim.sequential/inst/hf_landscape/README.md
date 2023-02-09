## README: Harvard Forest landscape-scale data assimilation

1. SiteSelection.Rmd: used to classify a landscape into patches. Outputs patches.csv, a summary table of patch statistics, and patches.tif, a map of the patches.

TODO:

2. Build a *default pecan.xml settings* file at the patch level

3. Code to build *initial input files* at the patch-level

4. Code to tweak automated *iterative input file* generation to account for patches

5. Code to tweak automated/iterative *data constraint* processing to account for patches, including H (observation operator) and cross-patch R (observation error covariance)

6. Code to tweak *iterative SDA*

7. Code for visualization and post-processing

