cd ~/
R CMD build pecan
PEcAn=`ls -v PEcAn*tar.gz | tail -n 1`
R CMD INSTALL $PEcAn
