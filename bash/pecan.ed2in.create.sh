DATE=`date +%Y%m%d`

OUTDIR=/home/scratch/$USER/pecan/out$DATE
EDIN=/home/scratch/$USER/pecan/edin
echo $DATE > /home/scratch/$USER/pecan/DATE
if [ ! -d $OUTDIR ] #if [output directory] exists 
then
    mkdir $OUTDIR    #if not, make new directory
fi

for i in $EDIN/*c.p* 
do 
rm $i 
done


cd $EDIN

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