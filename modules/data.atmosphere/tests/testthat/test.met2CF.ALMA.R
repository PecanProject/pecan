if(FALSE){

## Currently just code used for internal development
## will eventually convert to a formal test
in.path    = "/fs/data4/phase1a_met_drivers_v4/PDL"
in.prefix  = ""
outfolder  = "/tmp/PaleonMet"
start_date = "1030-01-01"
end_date   = "1030-12-31"
overwrite  = TRUE 
verbose=FALSE

results = met2CF.PalEON(in.path,in.prefix,outfolder,start_date,end_date,overwrite,verbose)

## full conversion:
in.path.base = "/fs/data4/phase1a_met_drivers_v4"
sites = dir(in.path.base)
in.prefix = ""
outfolder.base = "/fs/data1/pecan.data/input/PalEON.MIP."
start_date = "850-01-01"
end_date   = "2010-12-31"
overwrite  = FALSE
verbose    = FALSE
results = list()
for(s in sites){
  results[[s]] = met2CF.PalEON(file.path(in.path.base,s),in.prefix,
                               paste0(outfolder.base,s),start_date,end_date,overwrite,verbose)
}

}