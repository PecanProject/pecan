#!/usr/bin/Rscript

# R interface script to talk to my python script to turn a netcdf file into
# an appropriate GDAY forcing file
#
# Martin De Kauwe, 3rd August 2015
wd <- getwd()
setwd(wd)

site = "US-NR1"
fpath = "met_data"
outfile_tag = "US-NR1"
sub_daily = "false"      # Make 30-min file vs. Day, stick with day for now
tsoil_run_mean = "false"  # Generate Tsoil from 7-day running mean or not

command = "python3"
path2script = "generate_forcing_data.py"

all_args = paste(command, path2script, site, fpath, outfile_tag, sub_daily,
                 tsoil_run_mean)
print(all_args)
system(all_args)
