DATE=`date +%Y%m%d`

OUTDIR=/home/scratch/$USER/pecan/out$DATE
EDIN=/home/scratch/$USER/pecan/edin
echo $DATE > $EDIN/DATE
if [ ! -d $OUTDIR ] #if [output directory] exists 
then
    mkdir $OUTDIR    #if not, make new directory
fi

cd $EDIN
# remove old config and ED2IN files
for i in ED2IN*
do 
    if [ -a $i ] 
    then rm $i
    fi
done

for i in c.p* 
do 
    if [ -a $i ] 
    then rm $i
    fi
done

# unzip new config files
tar -zxf saconfigs.tgz 
# create new ED2IN file for each config file
cp aED2IN ED2IN
sed -i 's/YYYYMMDD/'$DATE'/g' ED2IN

for f in c.*
do 
    cp ED2IN ED2IN$f
    sed -i 's/ENSNAME/'$f'/g' ED2IN$f
    sed -i 's/USER/'$USER'/g' ED2IN$f
    sed -i 's/CONFIGFILE/'$f'/g' ED2IN$f
    sed -i 's/OUTFILE/out'$f'/g' ED2IN$f 
    sed -i 's/outconfig./out./g' ED2IN$f
    sed -i 's/HISTFILE/hist'$f'/g' ED2IN$f
    sed -i 's/histconfig./hist./g' ED2IN$f
done

rm ED2IN