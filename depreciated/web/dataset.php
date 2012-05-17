<?php

// database parameters
require("dbinfo.php");
require("system.php");

// runid
if (!isset($_REQUEST['runid'])) {
  die("Need a runid.");
}
$runid=$_REQUEST['runid'];

if (!isset($_REQUEST['type'])) {
  die("Need type.");
}
$type=$_REQUEST['type'];

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
	case "file":
		if (!isset($_REQUEST['name'])) {
			die("Need name.");
		}
		$name = $_REQUEST['name'];
		
		$file = realpath("$folder/$name");
		if (substr($file, 0, strlen($folder)) != $folder) {
			die("Invalid file name specified.");			
		}
		
		if (substr($name, -4) === ".xml") {
			$mime = "text/xml";
		} else if (substr($name, -4) === ".log") {
			$mime = "text/plain";					
		} else {
			$mime = "application/octet-stream";
			header('Content-Disposition: attachment; filename='.basename($name));			
		}
		break;
		
	case "plot":
		if (!isset($_REQUEST['year']) || !is_numeric($_REQUEST['year'])) {
			die("Need year.");
		}
		$year=$_REQUEST['year'];
		if (!isset($_REQUEST['var'])) {
			die("Need var.");
		}
		$var=$_REQUEST['var'];
		$width=600;
		if (isset($_REQUEST['width']) && ($_REQUEST['width'] > 600)) {
			$width=$_REQUEST['width'];
		}
		$height=600;
		if (isset($_REQUEST['height']) && ($_REQUEST['height'] > 600)) {
			$height=$_REQUEST['height'];
		}
		$mime = "image/png";
		$file = tempnam('','');
		shell_exec("PECANSETTINGS=$folder/pecan.xml R CMD BATCH --vanilla '--args $year $var $width $height $file' ${pecan_home}/rscripts/singleplot.R $folder/plot.out");				
		break;
		
	default:
		die("unknown type.");
}

if (!file_exists($file)) {
	die("Invalid file name specified.");			
}
if ($mime != "") {
	header("Content-type: $mime");
}
readfile($file);

if ($type == "plot") {
  unlink($file);
}
?>
