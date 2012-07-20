<?php	
/**
 * Copyright (c) 2012 University of Illinois, NCSA.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the 
 * University of Illinois/NCSA Open Source License
 * which accompanies this distribution, and is available at
 * http://opensource.ncsa.illinois.edu/license.html
 */
require("dbinfo.php");
$connection = open_database();

// Start XML file, create parent node
$dom = new DOMDocument("1.0");
$node = $dom->createElement("markers");
$parnode = $dom->appendChild($node); 

$query = "SELECT sites.* FROM sites";
if (isset($_REQUEST['host']) && ($_REQUEST['host'] != "")) {
	$query  = "SELECT DISTINCT sites.* FROM sites, inputs, dbfiles, machines WHERE dbfiles.file_id = inputs.file_id AND inputs.site_id=sites.id";
	$query .= " AND machines.hostname='{$_REQUEST['host']}' AND dbfiles.machine_id=machines.id";

	if (isset($_REQUEST['model']) && ($_REQUEST['model'] != "")) {
		$model = strtolower($_REQUEST['model']);
		if (preg_match('/^ed/', $model)) {
			$query .= " AND dbfiles.format_id=12";
		} else if (preg_match('/^sipnet/', $model)) {
			$query .= " AND dbfiles.format_id=26";
		}
	}
}


// Select all the rows in the markers table
$result = mysql_query($query);
if (!$result) {
	die('Invalid query: ' . mysql_error());
} 

// Iterate through the rows, adding XML nodes for each
while ($row = @mysql_fetch_assoc($result)){ 
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

close_database($connection);
?>
