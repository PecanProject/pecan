###
if [ ! -f ~/.pecan_init_indicator ]
then
    touch ~/.pecan_init_indicator

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
    
## make sure that alias 
    if [ ! -f ~/.bash_aliases ]
    then
	touch ~/.bash_aliases
    fi

    if ! grep kepler ~/.bash_aliases > /dev/null
    then 
	KEPALIAS='/usr/local/Kepler-2.0/kepler.sh'
	echo "alias kepler='$KEPALIAS'" >> .bash_aliases
    fi


## set up folders on ebi-cluster
ssh -T ebi-cluster < bash/pecan.init.cluster.sh 
fi
   
cd ~/pecan

bzr pull /home/dlebauer/dev/pecan/trunk

##set environment variables

. ./bash/env.vars.sh