## downloading from ornl

nohup ionice -n 7 wget -r -nH --cut-dirs=6 ftp://nacp.ornl.gov/synthesis/2009/frescati/model_driver/cru_ncep/analysis &

## transfer from pecandev to ebi-cluster
 ionice -c2 -n7 rsync --rsync-path="ionice -c2 -n7 rsync" -routi --progress ./* ebi-cluster:met/cruncep/
