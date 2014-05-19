args <- c("PEcAn.data.atmosphere", "extract.NARR", 
          "/projectnb/cheas/pecan.data/input/NARR_CF/", "NARR.", "/projectnb/cheas/pecan.data/input/NARR_CF_site_768/", 
          "38.7441", "-92.2", "2013", "2013")

in.path = fcn.args[1]
in.prefix = fcn.args[2]
outfolder = fcn.args[3]
slat = fcn.args[4]
slon = fcn.args[5]
start_year = fcn.args[6]
end_year = fcn.args[7]