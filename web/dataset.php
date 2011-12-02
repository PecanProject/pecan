<?php
// runid
if (!isset($_REQUEST['runid'])) {
  die("Need a runid.");
}
$runid=$_REQUEST['runid'];

if (!isset($_REQUEST['year']) || !is_numeric($_REQUEST['year'])) {
  die("Need year.");
}
$year=$_REQUEST['year'];

if (!isset($_REQUEST['type'])) {
  die("Need type.");
}
$type=$_REQUEST['type'];

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
$query = "SELECT outdir FROM runs WHERE runs.id=$runid";
$result = mysql_query($query);
if (!$result) {
	die('Invalid query: ' . mysql_error());
}
$run = mysql_fetch_assoc($result);
$folder = $run['outdir'];

// return dataset
switch ($type) {
	case "tower":
		$files=array_values(array_filter(scandir("$folder/out"), function ($item) {
			global $year;
			return preg_match("/.*-T-$year-.*.h5/", $item);
		}));
		if (count($files) < 1) {
			die("Could not find a tower file.");
		}
		header('Content-type: application/octet-stream');
		header('Content-Disposition: attachment; filename='.basename($files[0]));
		readfile("$folder/out/{$files[0]}");
		break;
	case "GPP":
	case "Reco":
	case "NPP":
	case "NEE":
		header('Content-type: image/png');
		readfile("$folder/$year-$type-year.png");
		break;
}
?>
