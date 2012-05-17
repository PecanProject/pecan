<?php
require("dbinfo.php");

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
$query = "SELECT outdir FROM runs WHERE runs.id=$runid";
$result = mysql_query($query);
if (!$result) {
	die('Invalid query: ' . mysql_error());
}
$run = mysql_fetch_assoc($result);
$folder = $run['outdir'];

// check result
$status=file("$folder/STATUS");
if ($status === FALSE) {
	$status = array();
}
if (endTime("FINISHED") != "") {
	header("Location: finished.php?runid=$runid");
}

function startTime($token) {
  global $status;
  foreach ($status as $line) {
    $data = explode("\t", $line);
    if ($data[0] == $token) {
      return $data[1];
    }
  }
  return "";
}

function endTime($token) {
  global $status;
  foreach ($status as $line) {
    $data = explode("\t", $line);
    if ($data[0] == $token && count($data) == 3) {
      return $data[2];
    }
  }
  return "";
}

function status($token) {
	global $folder;

  if (startTime($token) == "") {
    return "Waiting";
  }
  if (endTime($token) != "") {
    return "Done";
  }
	if ($token == "MODEL") {
		return exec("tail -20 `ls -1rt $folder/run/*.log` |  grep '^Simulating' | tail -1 | awk '{ print $2 }'");
	} else {
		return "Running";
	}
}

header( "refresh:5" );

?>
<html>
<head>
	<title>Site Run Setup</title>
	<link rel="stylesheet" type="text/css" href="sites.css" />
</head>
<body>
	<h1>JOB IS RUNNING</h1>

	<table border=1>
		<tr>
			<th>Stage Name</th>
			<th>Start Time</th>
			<th>End Time</th>
			<th>Status</th>
		</tr>
		<tr>
			<th>setup</th>
			<td><?=startTime("SETUP");?></td>
			<td><?=endTime("SETUP");?></td>
			<td><?=status("SETUP");?></td>
		</tr>
		<tr>
			<th>fia2ed</th>
			<td><?=startTime("FIA2ED");?></td>
			<td><?=endTime("FIA2ED");?></td>
			<td><?=status("FIA2ED");?></td>
		</tr>
		<tr>
			<th>query.bety</th>
			<td><?=startTime("BETY");?></td>
			<td><?=endTime("BETY");?></td>
			<td><?=status("BETY");?></td>
		</tr>
		<tr>
			<th>meta.analysis</th>
			<td><?=startTime("META");?></td>
			<td><?=endTime("META");?></td>
			<td><?=status("META");?></td>
		</tr>
		<tr>
			<th>write.config</th>
			<td><?=startTime("CONFIG");?></td>
			<td><?=endTime("CONFIG");?></td>
			<td><?=status("CONFIG");?></td>
		</tr>
		<tr>
			<th>model</th>
			<td><?=startTime("MODEL");?></td>
			<td><?=endTime("MODEL");?></td>
			<td><?=status("MODEL");?></td>
		</tr>
		<tr>
			<th>plots</th>
			<td><?=startTime("PLOTS");?></td>
			<td><?=endTime("PLOTS");?></td>
			<td><?=status("PLOTS");?></td>
		</tr>
		<tr>
			<th>finished</th>
			<td><?=startTime("FINISHED");?></td>
			<td><?=endTime("FINISHED");?></td>
			<td><?=status("FINISHED");?></td>
		</tr>
	</table>
</body>

