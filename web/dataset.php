<?php

// runid
if (!isset($_REQUEST['runid'])) {
  print("Need a runid.");
}
$runid=$_REQUEST['runid'];

if (!isset($_REQUEST['year']) || !is_numeric($_REQUEST['year'])) {
  print("Need year.");
}
$year=$_REQUEST['year'];

if (!isset($_REQUEST['type'])) {
  print("Need type.");
}
$type=$_REQUEST['type'];

if (isset($_REQUEST['name'])) {
	$name=$_REQUEST['name'];
	if (!preg_match('/[a-zA-Z0-9\-]+/', $name)) {
		print("Invalid name specified.");
		exit(0);
	}
} else {
	$name="unknown";
}

// database parameters
require("dbinfo.php");

// Opens a connection to a MySQL server
$connection=mysql_connect ($hostname, $username, $password);
if (!$connection) {
	print('Not connected : ' . mysql_error());
}

// Set the active MySQL database
$db_selected = mysql_select_db($database, $connection);
if (!$db_selected) {
	print ('Can\'t use db : ' . mysql_error());
}

// get run information
$query = "SELECT outdir FROM runs WHERE runs.id=$runid";
$result = mysql_query($query);
if (!$result) {
	print('Invalid query: ' . mysql_error());
}
$run = mysql_fetch_assoc($result);
$folder = $run['outdir'];

// return dataset
switch ($type) {
	case "pecan":
		$mime = "text/xml";
		$file="$folder/pecan.xml";
		break;
#	case "ed2in":
#		$mime = "text/plain";
#		$file="$folder/out/$name";
#		break;
	case "tower":
		header('Content-Disposition: attachment; filename='.basename($file));
		$mime="application/octet-stream";
		$files=array_values(array_filter(scandir("$folder/out"), function ($item) {
			global $year;
			return preg_match("/.*-T-$year-.*.h5/", $item);
		}));
		if (count($files) < 1) {
			die("Could not find a tower file.");
		}
		$file="$folder/out/{$files[0]}";
		break;
	case "GPP":
	case "Reco":
	case "NPP":
	case "NEE":
		$mime = "image/png";
		$file = "$folder/$year-$type-year.png";
		break;
	default:
		$file="unknown.file";
		break;
}

if (!file_exists($file)) {
	print("Could not find data $file.\n");
	return;
}
header('Content-type: $mime');
readfile($file);
?>

