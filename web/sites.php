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
	$query  = "SELECT sites.* FROM sites, inputs, input_files, machines WHERE input_files.file_id = inputs.file_id AND inputs.site_id=sites.id";
	$query .= " AND machines.hostname='{$_REQUEST['host']}' AND input_files.machine_id=machines.id";

	if (isset($_REQUEST['model']) && ($_REQUEST['model'] != "")) {
		$model = strtolower($_REQUEST['model']);
		if (preg_match('/^ed/', $model)) {
			$query .= " AND input_files.format_id=12";
		} else if (preg_match('/^sipnet/', $model)) {
			$query .= " AND input_files.format_id=24";
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
	$newnode->setAttribute("sitename", $row['sitename']);	
} 

echo $dom->saveXML();

close_database($connection);
?>
