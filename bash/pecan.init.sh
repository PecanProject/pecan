###Initialization file to set up system for PECAn
###new commands added to .bashrc: pullpecan buildpecan
###env vars set PECANHOME PECANOUT CLUSTERHOME EDIN 

if [ ! -d ~/lib ]
then
    mkdir ~/lib/ 
fi

if [ ! -d ~/lib/R ]
then
    mkdir ~/lib/R
fi

## check out pecan
if [ ! -d ~/pecan ]
then
    bzr branch /home/dlebauer/dev/pecan/trunk pecan
fi
## move .my.cnf_forecast to /home/user/.my.cnf
if [ ! -f ~/.my.cnf ]
then 
    cp .my.cnf_forecast .my.cnf
fi

if ! grep kepler ~/.bashrc > /dev/null
then 
    KEPALIAS='/usr/local/Kepler-2.0/kepler.sh'
    echo "alias kepler='$KEPALIAS'" >> ~/.bashrc
fi

    ##set environment variables
if ! grep env.vars.sh ~/.bashrc > /dev/null
then
    echo ". /home/$USER/pecan/bash/env.vars.sh" >> ~/.bashrc
fi

if ! grep pecanbuild ~/.bashrc > /dev/null
then
    echo "alias buildpecan='/home/$USER/pecan/bash/pecanbuild.sh'" >> ~/.bashrc
fi

if ! grep pecanpull ~/.bashrc > /dev/null
then
    echo "alias pullpecan='bzr pull /home/dlebauer/dev/pecan/trunk'" >> ~/.bashrc
fi

if ! grep edstat ~/.bashrc > /dev/null
then
    echo "alias edstat='ssh ebi-cluster qstat'" >> ~/.bashrc
fi


## set up folders on ebi-cluster
ssh -T ebi-cluster < ~/pecan/bash/pecan.init.cluster.sh 

cd ~/pecan
