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
		header( "Location: index.php");
		close_database();
		exit;
	}
}

# boolean parameters
if (isset($_REQUEST['offline'])) {
	$offline="&offline=offline";
} else {
	$offline="";
}

// runid
if (!isset($_REQUEST['workflowid'])) {
  die("Need a workflowid.");
}
$workflowid=$_REQUEST['workflowid'];

// get run information
$stmt = $pdo->prepare("SELECT folder, params FROM workflows WHERE workflows.id=?");
if (!$stmt->execute(array($workflowid))) {
	die('Invalid query: ' . error_database());
}
$workflow = $stmt->fetch(PDO::FETCH_ASSOC);
$stmt->closeCursor();

$folder = $workflow['folder'];
$params = eval("return ${workflow['params']};");

// check result
if (file_exists($folder . DIRECTORY_SEPARATOR . "STATUS")) {
	$status=file($folder . DIRECTORY_SEPARATOR . "STATUS");
} else {
	$status=array();
}

// quick checks for error and finished
foreach ($status as $line) {
	$data = explode("\t", $line);
	if ((count($data) >= 4) && ($data[3] == 'ERROR')) {
		if (isset($params['email'])) {
			$url = (isset($_SERVER['HTTPS']) ? "https://" : "http://");
			$url .= $_SERVER['HTTP_HOST'] . ':' . $_SERVER['SERVER_PORT'];
			$url .= str_replace("05-running.php", "failurealert.php", $_SERVER["SCRIPT_NAME"]);
			if ($offline) {
				$url .= "?workflowid=${workflowid}&offline=offline";
			} else {
				$url .= "?workflowid=${workflowid}";
			}
			mail($params['email'], "Workflow has failed", "You can find the results on $url");
		}
		$pdo->query("UPDATE workflows SET finished_at=NOW() WHERE id=${workflowid} AND finished_at IS NULL");
		close_database();
		header( "Location: failurealert.php?workflowid=${workflowid}${offline}");
		exit;
	}
   	if ($data[0] == "EDIT" && count($data) < 3) {
		header( "Location: 06-edit.php?workflowid=${workflowid}${offline}");
		close_database();
		exit;
    }
   	if ($data[0] == "FINISHED" && count($data) >= 3) {
		header( "Location: 08-finished.php?workflowid=${workflowid}${offline}");
		close_database();
		exit;
    }
}
close_database();

# get read for refresh
header( "refresh:5" );

?>
<!DOCTYPE html>
<html>
<head>
<title>PEcAn Running</title>
<meta name="viewport" content="width=device-width, initial-scale=1.0, user-scalable=no" />
<meta http-equiv="content-type" content="text/html; charset=UTF-8" />
<link rel="stylesheet" type="text/css" href="sites.css" />
<script type="text/javascript" src="jquery-1.7.2.min.js"></script>
<script type="text/javascript">
	function prevStep() {
		$("#formprev").submit();
	}

	function nextStep() {
		$("#formnext").submit();
	}
</script>
</head>
<body>
<div id="wrap">
	<div id="stylized">
		<h1>Job is running</h1>
		<p>Job is currently executing, please wait.</p>
		
		<form id="formprev" method="POST" action="02-modelsite.php">
<?php if ($offline != "") { ?>
			<input name="offline" type="hidden" value="offline">
<?php } ?>
		</form>
		
		<form id="formnext" method="POST" action="07-finished.php">
<?php if ($offline != "") { ?>
			<input name="offline" type="hidden" value="offline">
<?php } ?>
			<input type="hidden" name="workflowid" value="<?=$workflowid?>" />
		</form>

		<span id="error" class="small">&nbsp;</span>
		<input id="prev" type="button" value="Start Over" onclick="prevStep();" />
		<input id="next" type="button" value="Results" onclick="nextStep();" />
		<div class="spacer"></div>
<?php
	if (check_login()) {
		echo "<p></p>";
		echo "Logged in as " . get_user_name();
		echo "<a href=\"index.php?logout\" id=\"logout\">logout</a>";
	}
?>		
	</div>
	<div id="output">
	<h2>Execution Status</h2>
	<table border=1>
		<tr>
			<th>Stage Name</th>
			<th>Start Time</th>
			<th>End Time</th>
			<th>Status</th>
		</tr>
		<tr>
			<th>fia2ed</th>
			<td><?=startTime("FIA2ED");?></td>
			<td><?=endTime("FIA2ED");?></td>
			<td><?=status("FIA2ED");?></td>
		</tr>
		<tr>
			<th>query.trait</th>
			<td><?=startTime("TRAIT");?></td>
			<td><?=endTime("TRAIT");?></td>
			<td><?=status("TRAIT");?></td>
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
			<th>advanced.edit</th>
			<td><?=startTime("EDIT");?></td>
			<td><?=endTime("EDIT");?></td>
			<td><?=status("EDIT");?></td>
		</tr>
		<tr>
			<th>model</th>
			<td><?=startTime("MODEL");?></td>
			<td><?=endTime("MODEL");?></td>
			<td><?=status("MODEL");?></td>
		</tr>
		<tr>
			<th>output.conversion</th>
			<td><?=startTime("OUTPUT");?></td>
			<td><?=endTime("OUTPUT");?></td>
			<td><?=status("OUTPUT");?></td>
		</tr>
		<tr>
			<th>ensemble.analysis</th>
			<td><?=startTime("ENSEMBLE");?></td>
			<td><?=endTime("ENSEMBLE");?></td>
			<td><?=status("ENSEMBLE");?></td>
		</tr>
		<tr>
			<th>sensitivity.analysis</th>
			<td><?=startTime("SENSITIVITY");?></td>
			<td><?=endTime("SENSITIVITY");?></td>
			<td><?=status("SENSITIVITY");?></td>
		</tr>
		<tr>
			<th>finished</th>
			<td><?=startTime("FINISHED");?></td>
			<td><?=endTime("FINISHED");?></td>
			<td><?=status("FINISHED");?></td>
		</tr>
	</table>
	</div>
	<div id="footer">
		The <a href="http://pecanproject.org">PEcAn project</a> is supported by the National Science Foundation
		(ABI #1062547, ARC #1023477) and the <a href="http://www.energybiosciencesinstitute.org/">Energy
		Biosciences Institute</a>.
	</div>
</div>
</body>
</html>

<?php 
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
    if ($data[0] == $token && count($data) >= 3) {
      return $data[2];
    }
  }
  return "";
}

function status($token) {
  global $folder;
  global $status;

  foreach ($status as $line) {
    $data = explode("\t", $line);
    if ($data[0] == $token) {
      if (count($data) >= 4) {
        return $data[3];
      }
      if ($token == "MODEL") {
		foreach(scandir("$folder/out") as $runid) {
			if (!is_dir("$folder/out/$runid") || ($runid == ".") || ($runid == "..")) {
				continue;
			}
			if (file_exists("$folder/out/$runid/logfile.txt")) {
				$running = "$runid - " . exec("awk '/Simulating/ { print $3 }' $folder/out/$runid/logfile.txt | tail -1");
			}
		}
		return $running;
      }
      return "Running";
    }
  }
  return "";
}
?>
