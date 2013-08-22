<?php
/**
 * Copyright (c) 2012 University of Illinois, NCSA.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the 
 * University of Illinois/NCSA Open Source License
 * which accompanies this distribution, and is available at
 * http://opensource.ncsa.illinois.edu/license.html
 */
// runid
if (!isset($_REQUEST['workflowid'])) {
  die("Need a workflowid.");
}
$workflowid=$_REQUEST['workflowid'];

if (!isset($_REQUEST['type'])) {
  die("Need type.");
}
$type=$_REQUEST['type'];

// database parameters
require("system.php");
require("dbinfo.php");
$connection=open_database();

// get run information
$query = "SELECT folder FROM workflows WHERE workflows.id=${workflowid}";
$result = mysql_query($query);
if (!$result) {
	die('Invalid query: ' . mysql_error());
}
$run = mysql_fetch_assoc($result);
$folder = str_replace("//", "/", $run['folder']);

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
		} else if (substr($name, -4) === ".txt") {
			$mime = "text/plain";					
		} else if (substr($name, -4) === ".log") {
			$mime = "text/plain";					
		} else if (substr($name, -4) === ".pdf") {
			$mime = "application/pdf";					
		} else {
			$mime = "application/octet-stream";
		}
		break;
		
	case "plot":
		if (!isset($_REQUEST['run']) || !is_numeric($_REQUEST['run'])) {
			die("Need run.");
		}
		$run=$_REQUEST['run'];
		if (!isset($_REQUEST['year']) || !is_numeric($_REQUEST['year'])) {
			die("Need year.");
		}
		$year=$_REQUEST['year'];
		if (!isset($_REQUEST['var'])) {
			die("Need var.");
		}
		$var=$_REQUEST['var'];
        $datafile=$run . "/" . $year . ".nc";
		$width=600;
		if (isset($_REQUEST['width']) && ($_REQUEST['width'] > $width)) {
			$width=$_REQUEST['width'];
		}
		$height=400;
		if (isset($_REQUEST['height']) && ($_REQUEST['height'] > $height)) {
			$height=$_REQUEST['height'];
		}
		$mime = "image/png";
		$file = tempnam('','');
		shell_exec("R_LIBS_USER='${pecan_install}' PECANSETTINGS='$folder/pecan.xml' R CMD BATCH --vanilla '--args $datafile $year $var $width $height $file' plot.netcdf.R $folder/plot.out");
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
if (isset($name)) {
	header('Content-Disposition: filename='.basename($name));
}
readfile($file);

if ($type == "plot") {
  unlink($file);
}
?>
