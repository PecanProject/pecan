## unzip config files, set env vars, write ED2IN files, run ED ensemble, 
ssh ebi-cluster "cd /home/$USER/EDBRAMS/ED/run; tar -zxf pecan.edrunfiles.tgz; wait; tar -zxf saconfigs.tgz; wait; chmod +x *.sh; wait; ./pecan.ed2in.create.sh; wait; ./pecan.ed.batchjobs.sh"
