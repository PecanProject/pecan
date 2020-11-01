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
  if (get_page_acccess_level() > $min_run_level) {
    header( "Location: history.php");
    close_database();
    exit;
  }
}

# boolean parameters
$offline=isset($_REQUEST['offline']);
$pecan_edit=isset($_REQUEST['pecan_edit']);
$model_edit=isset($_REQUEST['model_edit']);

// workflowid
if (!isset($_REQUEST['workflowid'])) {
  die("Need a workflowid.");
}
$workflowid=$_REQUEST['workflowid'];

// hostname
if (!isset($_REQUEST['hostname'])) {
  die("Need a hostname.");
}
$hostname=$_REQUEST['hostname'];
if (!array_key_exists($hostname, $hostlist)) {
  die("${hostname} is not an approved host");
}
$hostoptions = $hostlist[$hostname];

// get run information
$stmt = $pdo->prepare("SELECT folder FROM workflows WHERE workflows.id=?");
if (!$stmt->execute(array($workflowid))) {
	die('Invalid query: ' . error_database());
}
$workflow = $stmt->fetch(PDO::FETCH_ASSOC);
$folder = $workflow['folder'];
$stmt->closeCursor();
close_database();

$path = "05-running.php?workflowid=$workflowid&hostname=${hostname}";
if ($pecan_edit) {
  $path .= "&pecan_edit=pecan_edit";
}
if ($model_edit) {
  $path .= "&model_edit=model_edit";
}
if ($offline) {
  $path .= "&offline=offline";
}

# setup umask so group has write as well
umask(0002);

# check if we edited pecan.xml
if (file_exists($folder . DIRECTORY_SEPARATOR . "STATUS")) {
  # add end of advanced edit
  $fh = fopen($folder . DIRECTORY_SEPARATOR . "STATUS", 'a') or die("can't open file");
  fwrite($fh, "\t" . date("Y-m-d H:i:s") . "\tDONE\t\n");
  fclose($fh);
}

# start the workflow again
if (array_key_exists("rabbitmq_uri", $hostoptions)) {
  $rabbitmq_uri = $hostoptions['rabbitmq_uri'];
  if (isset($hostoptions['rabbitmq_queue'])) {
    $rabbitmq_queue = $hostoptions['rabbitmq_queue'];
  } else {
    $rabbitmq_queue = "pecan";
  }

  $message = '{"folder": "' . $folder . '", "workflowid": "' . $workflowid . '"';
  if (file_exists($folder . DIRECTORY_SEPARATOR . "STATUS")) {
    $message .= ', "continue": true';
  } else if ($model_edit) {
    $message .= ', "modeledit": true';
  }
  $message .= '}';
  send_rabbitmq_message($message, $rabbitmq_uri, $rabbitmq_queue);
} else {
  chdir($folder);

  $exec = "R_LIBS_USER=\"$R_library_path\" $Rbinary CMD BATCH";
  if (file_exists($folder . DIRECTORY_SEPARATOR . "STATUS")) {
    $exec .= " --continue workflow.R workflow2.Rout";
  } else {
    if ($model_edit) {
      $exec .= " --advanced";
    }
    $exec .= " workflow.R";
  }
  
  pclose(popen("$exec &", 'r'));
}

#done
header("Location: $path");
?>
