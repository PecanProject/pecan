bzr pull ebi-forecast:dev/pecan/trunk
if [ ! -d /home/scratch/$USER/pecan ]  
then     
    mkdir /home/scratch/$USER/pecan   
fi
if [ ! -d /home/scratch/$USER/pecan/out ]   
then     
    mkdir /home/scratch/$USER/pecan/out   
fi
    ##set environment variables
if ! grep env.vars.sh ~/.bashrc > /dev/null
then
    echo ". /home/scratch/$USER/pecan/bash/env.vars.sh" >> ~/.bashrc
fi
