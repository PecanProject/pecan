<?php

# Information to connect to the BETY database
$db_bety_type="pgsql";
$db_bety_hostname="localhost";
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

# R binary
$Rbinary="/usr/bin/R";

# Require username/password
$authentication=false;

# Used for authentication, needs to be same as ruby
$REST_AUTH_SITE_KEY="thisisnotasecret";
$REST_AUTH_DIGEST_STRETCHES =10;

# List of allowed hosts
$fqdn=exec('hostname -f');
$hostlist=array($fqdn);

# Folder where PEcAn is installed
$pecan_install="/home/carya/R/library";

# Location where PEcAn is installed, not really needed anymore
$pecan_home="/home/carya/pecan/";

# Folder where the runs are stored
$output_folder="/home/carya/output/";

# ED specific inputs, should come from database
$ed_veg="/home/carya/oge2OLD/OGE2_";
$ed_soil="/home/carya/faoOLD/FAO_";
$ed_inputs="/home/carya/ed_inputs/";

# location of BETY DB set to empty to not create links, can be both
# relative or absolute paths or full URL's. Should point to the base
# of BETYDB
$betydb="/bety";

# ----------------------------------------------------------------------
# SIMPLE EDITING OF BETY DATABSE
# ----------------------------------------------------------------------
# Number of items to show on a page
$pagesize = 30;

# anonymous access level
$anonymous_level = 99;
$anonymous_page = 99;

# Used for authentication, needs to be same as ruby
$REST_AUTH_SITE_KEY          = "thisisnotasecret";
$REST_AUTH_DIGEST_STRETCHES  = 10;

# Location where logs should be written
$logfile = "/home/carya/output/betydb.log";

?>
