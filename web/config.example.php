<?php

# Information to connect to the database
$db_type="pgsql";
$db_hostname="localhost";
$db_username="bety";
$db_password="bety";
$db_database="bety";

# R binary
$Rbinary="/usr/bin/R";

# Require username/password
$authentication=false;

# Used for authentication, needs to be same as ruby
$REST_AUTH_SITE_KEY="thisisnotasecret";
$REST_AUTH_DIGEST_STRETCHES =10;

# List of allowed hosts
$hostlist=array(exec('hostname -f'));

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
?>
