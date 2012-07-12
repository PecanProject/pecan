<?php	
require("dbinfo.php");
$connection = open_database();

// Start XML file, create parent node
$dom = new DOMDocument("1.0");
$node = $dom->createElement("models");
$parnode = $dom->appendChild($node); 

// only run this if we have a host
if (isset($_REQUEST['host']) && ($_REQUEST['host'] != "")) {
	// check for models
	$query = "SELECT models.* FROM models WHERE models.model_path LIKE '{$_REQUEST['host']}%' ORDER BY models.model_name DESC, models.revision DESC";
	
	// Select all the rows in the models table
	$result = mysql_query($query);
	if (!$result) {
		die('Invalid query: ' . mysql_error());
	} 
	
	// Iterate through the rows, adding XML nodes for each
	while ($row = @mysql_fetch_assoc($result)){ 
		$node = $dom->createElement("model");
		$newnode = $parnode->appendChild($node);	 
		$newnode->setAttribute("id",$row['id']);
		$newnode->setAttribute("name", $row['model_name']);
		$newnode->setAttribute("revision", $row['revision']);	
	} 
}

echo $dom->saveXML();

close_database($connection);
?>
