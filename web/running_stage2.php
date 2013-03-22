<?php
/**
 * Copyright (c) 2012 University of Illinois, NCSA.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the 
 * University of Illinois/NCSA Open Source License
 * which accompanies this distribution, and is available at
 * http://opensource.ncsa.illinois.edu/license.html
 */

# boolean parameters
$offline=isset($_REQUEST['offline']);

// runid
if (!isset($_REQUEST['workflowid'])) {
  die("Need a workflowid.");
}
$workflowid=$_REQUEST['workflowid'];

// database parameters
require("dbinfo.php");
$connection=open_database();

// get run information
$query = "SELECT site_id, model_id, model_type, hostname, folder FROM workflows, models WHERE workflows.id=$workflowid and model_id=models.id";
$result = mysql_query($query);
if (!$result) {
	die('Invalid query: ' . mysql_error());
}
$workflow = mysql_fetch_assoc($result);
$folder = $workflow['folder'];

// check result
$status=file($folder . DIRECTORY_SEPARATOR . "STATUS");
if ($status === FALSE) {
	$status = array();
}

// check the global status
switch(checkStatus("MODEL")) {
	case 0:
		$nextenabled="disabled=\"disabled\"";
		header( "refresh:5" );
		break;		
	case 1:
		$nextenabled="disabled=\"disabled\"";
		chdir($folder);
		pclose(popen('R_LIBS_USER="' . $pecan_install . '" R CMD BATCH workflow_stage3.R &', 'r'));
		if ($offline) {
			header( "Location: running_stage3.php?workflowid=$workflowid&offline=offline");
		} else {
			header( "Location: running_stage3.php?workflowid=$workflowid");
		}
		break;
	case 2:
		$nextenabled="";
		if ($offline) {
			header( "Location: finished.php?workflowid=$workflowid&offline=offline");
		} else {
			header( "Location: finished.php?workflowid=$workflowid");
		}
		mysql_query("UPDATE workflows SET finished_at=NOW() WHERE id=${workflowid} AND finished_at IS NULL");
		break;
}

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
	window.onresize = resize;
	window.onload = resize;
	
	function resize() {
    	$("#stylized").height($(window).height() - 5);
    	$("#output").height($(window).height() - 1);
    	$("#output").width($(window).width() - $('#stylized').width() - 5);

    	$('#log').scrollTop($('#log')[0].scrollHeight);
	}

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
		
		<form id="formprev" method="POST" action="selectdata.php">
<?php if ($offline) { ?>
			<input name="offline" type="hidden" value="offline">
<?php } ?>
				<input type="hidden" name="siteid" value="<?=$workflow['site_id']?>" />
		<input type="hidden" name="modelid" value="<?=$workflow['model_id']?>" />
		<input type="hidden" name="modeltype" value="<?=$workflow['model_type']?>" />
		<input type="hidden" name="hostname" value="<?=$workflow['hostname']?>" />
		</form>
		
		<form id="formemail" method="POST" action="sendemail.php">
		<input type="hidden" name="workflowid" value="<?=$workflowid?>" />
		</form>
		
		<form id="formnext" method="POST" action="finished.php">
<?php if ($offline) { ?>
			<input name="offline" type="hidden" value="offline">
<?php } ?>
		<input type="hidden" name="workflowid" value="<?=$workflowid?>" />
		<span id="error" class="small">&nbsp;</span>
		<input id="prev" type="button" value="Prev" onclick="prevStep();" />
		<input id="next" type="button" value="Next" onclick="nextStep();" <?=$nextenabled?>/>		
		<div class="spacer"></div>
		</form>
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
			<th>finished</th>
			<td><?=startTime("FINISHED");?></td>
			<td><?=endTime("FINISHED");?></td>
			<td><?=status("FINISHED");?></td>
		</tr>
	</table>
	<hr/>
 	<h2>Output from PEcAn</h2>
 	<textarea id="log" cols="80" rows="10" readonly="readonly">
<?php
  	foreach(scandir($folder . DIRECTORY_SEPARATOR) as $file) {
  		if (preg_match("/^workflow_stage.*\.Rout$/", $file) === 1) {
  			parselog($folder . DIRECTORY_SEPARATOR . $file);
  		}
	}
?>
 	</textarea>
	</div>
</div>
</body>
</html>

<?php 
close_database($connection);

function checkStatus($token) {
  	global $status;
	foreach ($status as $line) {
		$data = explode("\t", $line);
		if ((count($data) >= 4) && ($data[3] == 'ERROR')) {
			return 2;
		}
	}
	
	if (endTime($token) != "") {
		return 1;
	}
		
	return 0;
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
        return exec("awk '/Simulating/ { print $3 }' $folder/workflow_stage2.Rout | tail -1");
      }
      return "Running";
    }
  }
  return "Waiting";
}

function parselog($filename)
{
	// Open the file
	$f = fopen($filename, "rb");
	if ($f === false) {
		return "file does not exist.";
	}

	// read the file line by line
	$check = false;
	while (($buffer = fgets($f, 4096)) !== false) {
		if ($check && ($buffer[0]==" ")) {
			print($buffer);
		} else if (stristr($buffer, "error") !== false) {
			print($buffer);
			$check = true;
		} else if (stristr($buffer, "warn") !== false) {
			print($buffer);
			$check = true;
		} else {
			$check = false;
		}
	}

	// Close file and return
	fclose($f);
}
?>
