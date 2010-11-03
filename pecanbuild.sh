cd ~/
R CMD build pecan
R CMD INSTALL PECAn_0.1.1.tar.gz --library='~/lib/R'
