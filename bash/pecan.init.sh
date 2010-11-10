###

if [ ! -d ~/lib ]
then
    mkdir ~/lib/ 
fi

if [ ! -d ~/lib/R ]
then
    mkdir ~/lib/R
fi

if [ ! -d ~/pecan ]
then
    bzr branch /home/dlebauer/dev/pecan/trunk pecan
fi

cd ~/pecan

bzr pull /home/dlebauer/dev/pecan/trunk

## set up folders on ebi-cluster
ssh ebi-cluster "if [ ! -d /home/scratch/$USER/pecan ];   then     mkdir /home/scratch/$USER/pecan;   fi;"
ssh ebi-cluster "if [ ! -d /home/scratch/$USER/pecan/out ];   then     mkdir /home/scratch/$USER/pecan/out;   fi;"

## move .my.cnf_forecast to /home/user/.my.cnf
if [ ! -f ~/.my.cnf ]
then 
    cp .my.cnf_forecast .my.cnf
fi

## make sure that alias 
if [ ! -f ~/.bash_aliases ]
then
    touch ~/.bash_aliases
fi

if ! grep kepler ~/.bash_aliases > /dev/null
then 
    KEPALIAS="alias kepler='/usr/local/Kepler-2.0/kepler.sh'"
    $KEPALIAS
    echo $KEPALIAS >> .bash_aliases
fi