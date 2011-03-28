EDIN=/home/scratch/$USER/pecan/edin
DATE=`date +%Y%m%d`
OUTDIR=/home/scratch/pecan/$USER/out$DATE
ED_RUN=$HOME/EDBRAMS/ED/run

# make new directory
mkdir --parents $OUTDIR   

# remove old config and ED2IN files
rm $ED_RUN/ED2INc*
rm $EDIN/c.* 

cd $EDIN

# unzip new config files
tar -zxf saconfigs.tgz 

# create new ED2IN file for each config file
for f in c.*
do 
  #Create ED2IN file from template
  ED2INc=$ED_RUN/ED2IN$f
  cp aED2IN $ED2INc
  sed -i 's/YYYYMMDD/'$DATE'/g' $ED2INc
  sed -i 's/ENSNAME/'$f'/g' $ED2INc
  sed -i 's/USER/'$USER'/g' $ED2INc
  sed -i 's/CONFIGFILE/'$f'/g' $ED2INc
  sed -i 's/OUTFILE/out'$f'/g' $ED2INc
  sed -i 's/outconfig./out./g' $ED2INc
  sed -i 's/HISTFILE/hist'$f'/g' $ED2INc
  sed -i 's/histconfig./hist./g' $ED2INc
done
