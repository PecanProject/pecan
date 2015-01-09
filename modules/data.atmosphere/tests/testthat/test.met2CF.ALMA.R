if(FALSE){

## Currently just code used for internal development
## will eventually convert to a formal test
in.path    = "/fs/data4/phase1a_met_drivers_v4/PDL"
in.prefix  = ""
outfolder  = "/tmp/PaleonMet"
start_date = "1000-01-01"
end_date   = "1001-12-31"
overwrite  = TRUE 
verbose=FALSE

results = met2CF.PalEON(in.path,in.prefix,outfolder,start_date,end_date,overwrite,verbose)

}