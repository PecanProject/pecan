#Automatically runs all 7 test cases in runtest-download.NOAA_GEFS.R and writes the output
#for each case out to their own separate files.
#@author Luke Dramko

runnum=1
while [ $runnum -le 7 ];
do
  flname=out$runnum.txt
  Rscript runtest.R $runnum &> $flname
  (( runnum++ ))
done
