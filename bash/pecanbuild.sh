cd ~/
R CMD build pecan
PECAn=`ls -v PECAn*tar.gz | tail -n 1`
R CMD INSTALL $PECAn
