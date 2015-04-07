<?php  
/**
 * Copyright (c) 2012 University of Illinois, NCSA.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the 
 * University of Illinois/NCSA Open Source License
 * which accompanies this distribution, and is available at
 * http://opensource.ncsa.illinois.edu/license.html
 */
require("common.php");
open_database();
if ($authentication) {
	if (!check_login()) {
		header( "Location: index.php");
		close_database();
		exit;
	}
}

# boolean parameters
$offline=isset($_REQUEST['offline']);
$pecan_edit=isset($_REQUEST['pecan_edit']);
$model_edit=isset($_REQUEST['model_edit']);

if (!isset($_REQUEST['workflowid'])) {
	die("Need a workflowid.");
}
$workflowid=$_REQUEST['workflowid'];

// get run information
$stmt = $pdo->prepare("SELECT folder FROM workflows WHERE workflows.id=?");
if (!$stmt->execute(array($workflowid))) {
	die('Invalid query: ' . error_database());
}
$workflow = $stmt->fetch(PDO::FETCH_ASSOC);
$folder = $workflow['folder'];
$stmt->closeCursor();
close_database();

$exec = "R_LIBS_USER=\"$pecan_install\" $Rbinary CMD BATCH";
$path = "05-running.php?workflowid=$workflowid";
if ($pecan_edit) {
  $path .= "&pecan_edit=pecan_edit";
}
if ($model_edit) {
  $path .= "&model_edit=model_edit";
}
if ($offline) {
  $path .= "&offline=offline";
}

# check if we edited pecan.xml
if (file_exists($folder . DIRECTORY_SEPARATOR . "STATUS")) {
  # add end of advanced edit
  $fh = fopen($folder . DIRECTORY_SEPARATOR . "STATUS", 'a') or die("can't open file");
  fwrite($fh, "\t" . date("Y-m-d H:i:s") . "\tDONE\t\n");
  fclose($fh);

  $exec .= " --continue workflow.R workflow2.Rout";
} else {
  if ($model_edit) {
    $exec .= " --advanced";
  }
  $exec .= " workflow.R";
}

# start the workflow again
chdir($folder);
pclose(popen("$exec &", 'r'));

#done
header("Location: $path");
?>

