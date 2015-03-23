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
$node = $dom->createElement("models");
$parnode = $dom->appendChild($node); 

// only run this if we have a host
if (isset($_REQUEST['host']) && ($_REQUEST['host'] != "")) {
	// check for models
	$query = "SELECT models.* FROM models, dbfiles, machines";
	$query .= " WHERE machines.hostname=?";
	$query .= "   AND dbfiles.container_id = models.id AND dbfiles.machine_id=machines.id AND dbfiles.container_type='Model'";
	$query .= " ORDER BY models.model_name DESC, models.revision DESC";
	
	// Select all the rows in the models table
  $stmt = $pdo->prepare($query);
  if (!$stmt->execute(array($_REQUEST['host']))) {
    die('Invalid query: ' . error_database());
  }
	
	// Iterate through the rows, adding XML nodes for each
	while ($row = @$stmt->fetch(PDO::FETCH_ASSOC);){ 
		$node = $dom->createElement("model");
		$newnode = $parnode->appendChild($node);	 
		$newnode->setAttribute("id",$row['id']);
		$newnode->setAttribute("name", $row['model_name']);
		$newnode->setAttribute("revision", $row['revision']);	
	} 
  $stmt->closeCursor();
}

echo $dom->saveXML();

close_database();
?>
