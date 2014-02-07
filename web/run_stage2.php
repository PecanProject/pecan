<?php
/**
 * Copyright (c) 2012 University of Illinois, NCSA.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the 
 * University of Illinois/NCSA Open Source License
 * which accompanies this distribution, and is available at
 * http://opensource.ncsa.illinois.edu/license.html
 */

# boolean parameters
$offline=isset($_REQUEST['offline']);

// runid
if (!isset($_REQUEST['workflowid'])) {
  die("Need a workflowid.");
}
$workflowid=$_REQUEST['workflowid'];

// database parameters
require("system.php");
$pdo = new PDO("${db_type}:host=${db_hostname};dbname=${db_database}", $db_username, $db_password);

// get run information
$query = "SELECT site_id, model_id, model_type, hostname, folder, advanced_edit FROM workflows, models WHERE workflows.id=$workflowid and model_id=models.id";
$result = $pdo->query($query);
if (!$result) {
	die('Invalid query: ' . error_database());
}
$workflow = $result->fetch(PDO::FETCH_ASSOC);
$folder = $workflow['folder'];

chdir($folder);
pclose(popen('R_LIBS_USER="' . ${pecan_install} . '" R CMD BATCH workflow_stage2.R &', 'r'));
if ($offline) {
	header( "Location: running_stage2.php?workflowid=$workflowid&offline=offline");
} else {
	header( "Location: running_stage2.php?workflowid=$workflowid");
}			

$pdo = null;
