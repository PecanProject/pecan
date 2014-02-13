<?php
/**
 * Copyright (c) 2012 University of Illinois, NCSA.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the 
 * University of Illinois/NCSA Open Source License
 * which accompanies this distribution, and is available at
 * http://opensource.ncsa.illinois.edu/license.html
 */
// runid
if (!isset($_REQUEST['workflowid'])) {
  die("Need a workflowid.");
}
$workflowid=$_REQUEST['workflowid'];
if (!isset($_REQUEST['name'])) {
	die("Need name.");
}
$name = $_REQUEST['name'];
if (!isset($_REQUEST['data'])) {
	die("Need data.");
}
$data = $_REQUEST['data'];

// database parameters
require("system.php");
$pdo = new PDO("${db_type}:host=${db_hostname};dbname=${db_database}", $db_username, $db_password);

// get run information
$query = "SELECT folder FROM workflows WHERE workflows.id=${workflowid}";
$result = $pdo->query($query);
if (!$result) {
	die('Invalid query: ' . error_database());
}
$run = $result->fetch(PDO::FETCH_ASSOC);
$folder = str_replace("//", "/", $run['folder']);

$file = realpath("$folder/$name");
if (substr($file, 0, strlen($folder)) != $folder) {
	die("Invalid file name specified.");			
}

if (!file_exists($file)) {
	die("Can't create new files.");
}
if (!file_exists($file . ".orig")) {
	copy($file, $file . ".orig");
}
file_put_contents($file, $data);

$pdo = null;

//print("data is saved to $file");
?>
