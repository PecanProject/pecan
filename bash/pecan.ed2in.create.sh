DATE=`date +%Y%m%d`
cat $DATE > DATE
OUTDIR=/home/scratch/pecan/$USER/out$DATE
ED_RUN=$HOME/EDBRAMS/ED/runcd $ED_RUN
rm c.*

if [ ! -d $OUTDIR ] #if [output directory] exists 
then
    mkdir $OUTDIR    #if not, make new directory
fi

tar -zxf saconfigs.tgz #unzip config files

##Make new ED2IN file for each config file
cp aED2IN ED2IN
sed -i 's/YYYYMMDD/'$DATE'/g' ED2IN

for f in c.*
do 
    cp ED2IN ED2IN$f
    sed -i 's/USER/'$USER'/g' ED2IN$f
    sed -i 's/CONFIGFILE/'$f'/g' ED2IN$f
    sed -i 's/OUTFILE/out'$f'/g' ED2IN$f 
    sed -i 's/outconfig./out./g' ED2IN$f
    sed -i 's/HISTFILE/hist'$f'/g' ED2IN$f
    sed -i 's/histconfig./hist./g' ED2IN$f
done

rm ED2IN