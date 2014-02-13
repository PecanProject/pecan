<?php	
/**
 * Copyright (c) 2012 University of Illinois, NCSA.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the 
 * University of Illinois/NCSA Open Source License
 * which accompanies this distribution, and is available at
 * http://opensource.ncsa.illinois.edu/license.html
 */
require("system.php");
$pdo = new PDO("${db_type}:host=${db_hostname};dbname=${db_database}", $db_username, $db_password);

// Start XML file, create parent node
$dom = new DOMDocument("1.0");
$node = $dom->createElement("models");
$parnode = $dom->appendChild($node); 

// only run this if we have a host
if (isset($_REQUEST['host']) && ($_REQUEST['host'] != "")) {
	// check for models
	$query = "SELECT models.* FROM models WHERE models.model_path LIKE '{$_REQUEST['host']}:%' ORDER BY models.model_name DESC, models.revision DESC";
	
	// Select all the rows in the models table
	$result = $pdo->query($query);
	if (!$result) {
		die('Invalid query: ' . error_database());
	} 
	
	// Iterate through the rows, adding XML nodes for each
	while ($row = @$result->fetch(PDO::FETCH_ASSOC)){ 
		$node = $dom->createElement("model");
		$newnode = $parnode->appendChild($node);	 
		$newnode->setAttribute("id",$row['id']);
		$newnode->setAttribute("name", $row['model_name']);
		$newnode->setAttribute("revision", $row['revision']);	
	} 
}

echo $dom->saveXML();

$pdo = null;
?>
