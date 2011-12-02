<?php  
require("dbinfo.php"); 

// Start XML file, create parent node
$dom = new DOMDocument("1.0");
$node = $dom->createElement("markers");
$parnode = $dom->appendChild($node); 

// Opens a connection to a MySQL server
$connection=mysql_connect ($hostname, $username, $password);
if (!$connection) {  die('Not connected : ' . mysql_error());} 

// Set the active MySQL database
$db_selected = mysql_select_db($database, $connection);
if (!$db_selected) {
  die ('Can\'t use db : ' . mysql_error());
} 

// check for valid MET data
if (isset($_REQUEST['met']) && ($_REQUEST['met'] == "true")) {
  $query = "SELECT sites.* FROM sites, inputs where inputs.format_id=9 and inputs.site_id=sites.id";
} else {
  $query = "SELECT sites.* FROM sites";
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
  $newnode->setAttribute("id",$row['id']);
  $newnode->setAttribute("city",$row['city']);
  $newnode->setAttribute("country", $row['country']);  
  $newnode->setAttribute("lat", $row['lat']);  
  $newnode->setAttribute("lon", $row['lon']);  
  $newnode->setAttribute("sitename", $row['sitename']);  
} 

echo $dom->saveXML();
?>
