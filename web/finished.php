<?php

// runid
if (!isset($_REQUEST['runid'])) {
  die("Need a runid.");
}
$runid=$_REQUEST['runid'];

// database parameters
require("dbinfo.php");

// Opens a connection to a MySQL server
$connection=mysql_connect ($hostname, $username, $password);
if (!$connection) {
	die('Not connected : ' . mysql_error());
}

// Set the active MySQL database
$db_selected = mysql_select_db($database, $connection);
if (!$db_selected) {
	die ('Can\'t use db : ' . mysql_error());
}

// get run information
$query = "SELECT start_time, finish_time FROM runs WHERE runs.id=$runid";
$result = mysql_query($query);
if (!$result) {
	die('Invalid query: ' . mysql_error());
}
$run = mysql_fetch_assoc($result);
$year = substr($run['start_time'], 0, 4);
$end = substr($run['finish_time'], 0, 4);

echo "<h1>Results</h1>";
while($year <= $end) {
	echo "<h2>$year</h2>";
	echo "<ul>";
	echo "<li><a href=\"dataset.php?runid=$runid&year=$year&type=tower\">Tower Data</a></li>";
	echo "<li><a href=\"dataset.php?runid=$runid&year=$year&type=pecan\">pecan.xml</a></li>";
	echo "</ul>";
	echo "<img src=\"dataset.php?runid=$runid&year=$year&type=GPP\"><br>";
	echo "<img src=\"dataset.php?runid=$runid&year=$year&type=Reco\"><br>";
	echo "<img src=\"dataset.php?runid=$runid&year=$year&type=NPP\"><br>";
	echo "<img src=\"dataset.php?runid=$runid&year=$year&type=NEE\"><br>";
	$year++;
}

?>
