<?php	
/**
 * Copyright (c) 2012 University of Illinois, NCSA.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the 
 * University of Illinois/NCSA Open Source License
 * which accompanies this distribution, and is available at
 * http://opensource.ncsa.illinois.edu/license.html
 */

// Check login
require("common.php");
open_database();
if ($authentication) {
	if (!check_login()) {
		close_database();
		header('HTTP/1.1 403 Unauthorized');
		exit;
	}
}

// Start XML file, create parent node
$dom = new DOMDocument("1.0");
$node = $dom->createElement("markers");
$parnode = $dom->appendChild($node); 

if (isset($_REQUEST['model']) && ($_REQUEST['model'] != "")) {
	$result = $pdo->query("SELECT * FROM models WHERE id='" . $_REQUEST['model'] . "'");
	$model = $result->fetch(PDO::FETCH_ASSOC);
	$modeltype = $model["model_type"];
	$result->closeCursor();
} else {
	$model = "";
	$modeltype = "";
}

$query = "SELECT sites.* FROM sites";
if (isset($_REQUEST['host']) && ($_REQUEST['host'] != "")) {
	if ($modeltype == "ED2") {
		$query  = "SELECT DISTINCT sites.* FROM sites, inputs, dbfiles, machines WHERE dbfiles.container_id = inputs.file_id AND inputs.site_id=sites.id";
		$query .= " AND machines.hostname='{$_REQUEST['host']}' AND dbfiles.machine_id=machines.id";
		$query .= " AND inputs.format_id=12";
	} else if ($modeltype == "SIPNET") {
		$query  = "SELECT DISTINCT sites.* FROM sites, inputs, dbfiles, machines WHERE dbfiles.container_id = inputs.file_id AND inputs.site_id=sites.id";
		$query .= " AND machines.hostname='{$_REQUEST['host']}' AND dbfiles.machine_id=machines.id";
		$query .= " AND inputs.format_id=24";
	}
}

// Select all the rows in the markers table
$result = $pdo->query($query);
if (!$result) {
	die('Invalid query: ' . error_database());
} 

// Iterate through the rows, adding XML nodes for each
while ($row = @$result->fetch(PDO::FETCH_ASSOC)){ 
	$node = $dom->createElement("marker");
	$newnode = $parnode->appendChild($node);	 
	$newnode->setAttribute("siteid",$row['id']);
	$newnode->setAttribute("city", $row['city']);
	$newnode->setAttribute("country", $row['country']);	
	$newnode->setAttribute("lat", $row['lat']);	
	$newnode->setAttribute("lon", $row['lon']);
	if ($row['sitename'] != "") {
		$newnode->setAttribute("sitename", $row['sitename']);
	} else {	
		$newnode->setAttribute("sitename", $row['id'] . " - " . $row['city']);
	}
} 

echo $dom->saveXML();

close_database();
?>
