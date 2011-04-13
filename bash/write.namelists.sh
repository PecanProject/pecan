# remove old config and ED2IN files
rm ED2INc*
rm c.*

#Create Date
RUNTIME=`date +%Y.%m.%d-%H.%M`
mkdir $RUNTIME

# unzip new config files
tar -zxf configs.tgz 

# create new ED2IN file for each config file
for f in c.*
do 
    echo $f
  #Create ED2IN file from template
  ED2INc=ED2IN$f
  cp aED2IN $ED2INc
  sed -i 's/RUNTIME/'$RUNTIME'/g' $ED2INc
  sed -i 's/ENSNAME/'$f'/g' $ED2INc
  sed -i 's/USER/'$USER'/g' $ED2INc
  sed -i 's/CONFIGFILE/'$f'/g' $ED2INc
  sed -i 's/OUTFILE/out'$f'/g' $ED2INc
  sed -i 's/outconfig./out./g' $ED2INc
  sed -i 's/HISTFILE/hist'$f'/g' $ED2INc
  sed -i 's/histconfig./hist./g' $ED2INc
done
## Need to rewrite in R to facilitate naming out-directory from settings.pavi.xml
## or get xpath installed (???)