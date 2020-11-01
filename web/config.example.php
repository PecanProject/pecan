<?php

# Information to connect to the BETY database
$db_bety_type="pgsql";
$db_bety_hostname="localhost";
$db_bety_port=5432;
$db_bety_username="bety";
$db_bety_password="bety";
$db_bety_database="bety";

# Information to connect to the FIA database
# leave this blank if you do not have the FIA database installed.
$db_fia_type="pgsql";
$db_fia_hostname="";
$db_fia_username="";
$db_fia_password="";
$db_fia_database="";

# browdog information
$browndog_url="";
$browndog_username="";
$browndog_password="";

# R binary
$Rbinary="/usr/bin/R";

# sshTunnel binary
$SSHtunnel=dirname(__FILE__) . DIRECTORY_SEPARATOR . "sshtunnel.sh";

# Require username/password, can set min level to 0 so nobody can run/delete.
# 4 = viewer
# 3 = creator
# 2 = manager
# 1 = administrator
$authentication=false;
$min_run_level=2;
$min_delete_level=2;

# Used for authentication, needs to be same as ruby
$REST_AUTH_SITE_KEY="thisisnotasecret";
$REST_AUTH_DIGEST_STRETCHES =10;

# anonymous access level
$anonymous_level = 99;
$anonymous_page = 99;

# name of current machine
$fqdn=exec('hostname -f');

# List of all host and options. The list should be the server pointing
# to an array. The second array contains a key value pair used to
# configure the host. Currenly the following options are available:
# - qsub       : if specified the jobs are launched using qsub, this can
#                be an empty value to indicate to use default settings.
#                If not specified jobs are run on the host itself.
# - jobid      : regex used to parse jobid, only used if qsub specified.
# - qstat      : command used to check if job submitted using qsub is
#                finished.
# - launcher   : path to modellauncher, used to for a single job that
#                consists of many smaller jobs
# - job.sh     : any special parameters to add to the job.sh file. (deprecated)
# - prerun     : any special options to add at the begging of the job.
# - postrun    : any special options to add at the end of the job.
# - folder     : folder on remote machine, will add username and the
#                workflowid to the folder name
# - models     : any special options to add to a specific model that is
#                launched. This is an array of the modeltype and
#                additional parameters for the job.sh. If the model with
#                version is found that will be used, otherwise the more
#                generic model only will be used.
# - scratchdir : folder to be used for scratchspace when running certain
#                models (such as ED)
$hostlist=array($fqdn => array(),
                "geo.bu.edu" =>
                    array("displayname" => "geo",
                          "qsub"        => "qsub -V -N @NAME@ -o @STDOUT@ -e @STDERR@ -S /bin/bash",
                          "jobid"       => "Your job ([0-9]+) .*",
                          "qstat"       => "qstat -j @JOBID@ || echo DONE",
                          "prerun"      => "module load udunits R/R-3.0.0_gnu-4.4.6",
                          "postrun"     => "sleep 60",
                          "models"      => 
                              array("ED2" =>
                                        array("prerun"  => "module load hdf5"),
                                    "ED2 (r82)" =>
                                        array("prerun"  => "module load hdf5")
                              )
                          )
                );

# Folder where PEcAn is installed
$R_library_path="/home/carya/R/library";

# Location where PEcAn is installed, not really needed anymore
$pecan_home="/home/carya/pecan/";

# Folder where the runs are stored
$output_folder="/home/carya/output/";

# Folder where the generated files are stored
$dbfiles_folder=$output_folder . "/dbfiles";

# location of BETY DB set to empty to not create links, can be both
# relative or absolute paths or full URL's. Should point to the base
# of BETYDB
$betydb="/bety";

# syncing details
$server_url="192.168.0.5";    // local test server
$client_sceret="";
$server_auth_token="";

?>
