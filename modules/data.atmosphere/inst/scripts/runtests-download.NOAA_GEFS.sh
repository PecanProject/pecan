runnum=1
while [ $runnum -le 7 ];
do
  flname=out$runnum.txt
  Rscript runtest.R $runnum &> $flname
  (( runnum++ ))
done
