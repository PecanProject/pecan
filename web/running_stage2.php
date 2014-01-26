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
require("system.php");
$pdo = new PDO("${db_type}:host=${db_hostname};dbname=${db_database}", ${db_username}, ${db_password});

// get run information
$query = "SELECT folder, params FROM workflows WHERE workflows.id=$workflowid";
$result = $pdo->query($query);
if (!$result) {
	die('Invalid query: ' . $pdo->errorInfo());
}
$workflow = $result->fetch(PDO::FETCH_ASSOC);
$folder = $workflow['folder'];
$params = eval("return ${workflow['params']};");

// check result
if (file_exists($folder . DIRECTORY_SEPARATOR . "STATUS")) {
	$status=file($folder . DIRECTORY_SEPARATOR . "STATUS");
} else {
	$status=array();
}

// jump to right place if need be
if (checkStatus("FINISHED") == 1) {
	if ($offline) {
		header( "Location: finished.php?workflowid=$workflowid&offline=offline");
		exit;
	} else {
		header( "Location: finished.php?workflowid=$workflowid");
		exit;
	}
}

// check the global status
switch(checkStatus("MODEL")) {
	// No ERROR, and no endtime yet
	case 0:
		$nextenabled="disabled=\"disabled\"";
		header( "refresh:5" );
		break;		
	// MODEL is complete
	case 1:
		$nextenabled="disabled=\"disabled\"";
		chdir($folder);
		pclose(popen('R_LIBS_USER="' . $pecan_install . '" R CMD BATCH workflow_stage3.R &', 'r'));
		if ($offline) {
			header( "Location: running_stage3.php?workflowid=$workflowid&offline=offline");
		} else {
			header( "Location: running_stage3.php?workflowid=$workflowid");
		}
		exit;
    // ERROR occurred
	case 2:
		if (isset($params['email'])) {
			$url = ($_SERVER['HTTPS'] ? "https://" : "http://");
			$url .= $_SERVER['HTTP_HOST'] . ':' . $_SERVER['SERVER_PORT'];
			$url .= str_replace("running_stage1.php", "failurealert.php", $_SERVER["SCRIPT_NAME"]);
			if ($offline) {
				$url .= "?workflowid=${workflowid}&offline=offline";
			} else {
				$url .= "?workflowid=${workflowid}";
			}
			mail($params['email'], "Workflow has failed", "You can find the results on $url");
		}
		$nextenabled="";
		$pdo->query("UPDATE workflows SET finished_at=NOW() WHERE id=${workflowid} AND finished_at IS NULL");
		if ($offline) {
			header( "Location: failurealert.php?workflowid=$workflowid&offline=offline");
		} else {
			header( "Location: failurealert.php?workflowid=$workflowid");
		}
		exit;
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
			<?php foreach ($params as $k => $v) {
				if (is_array($v)) {
					foreach($v as $x) {
						echo "<input type=\"hidden\" name=\"${k}[]\" value=\"${x}\" />\n";
					}
				} else {
					echo "<input type=\"hidden\" name=\"${k}\" value=\"${v}\" />\n";
				}
			} ?>
		</form>
		
		<form id="formnext" method="POST" action="finished.php">
<?php if ($offline) { ?>
			<input name="offline" type="hidden" value="offline">
<?php } ?>
			<input type="hidden" name="workflowid" value="<?=$workflowid?>" />
		</form>

		<span id="error" class="small">&nbsp;</span>
		<input id="prev" type="button" value="Prev" onclick="prevStep();" />
		<input id="next" type="button" value="Next" onclick="nextStep();" <?=$nextenabled?>/>		
		<div class="spacer"></div>
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
	</div>
</div>
</body>
</html>

<?php 
$pdo = null;

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
?>
