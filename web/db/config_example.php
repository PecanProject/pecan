<?php
# Information to connect to the database
$db_type="pgsql";
$db_hostname="localhost";
$db_username="bety";
$db_password="bety";
$db_database="bety";

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
