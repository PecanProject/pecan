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
$offline=isset($_REQUEST['offline']) ? "&offline=offline" : "";

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

# add end of advanced edit
$fh = fopen($folder . DIRECTORY_SEPARATOR . "STATUS", 'a') or die("can't open file");
fwrite($fh, "\t" . date("Y-m-d H:i:s") . "\tDONE\t\n");
fclose($fh);

# start the workflow again
chdir($folder);
pclose(popen('R_LIBS_USER="' . $pecan_install . '" ' . $Rbinary . ' CMD BATCH --continue workflow.R &', 'r'));

#done
header("Location: 05-running.php?workflowid=${workflowid}${offline}");
?>

