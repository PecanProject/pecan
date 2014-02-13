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

if (!isset($_REQUEST['workflowid'])) {
	die("Need a workflowid.");
}
$workflowid=$_REQUEST['workflowid'];

// connect to database
require("system.php");
$pdo = new PDO("${db_type}:host=${db_hostname};dbname=${db_database}", $db_username, $db_password);

// get run information
$query = "SELECT site_id, model_id, model_type, hostname, folder, advanced_edit FROM workflows, models WHERE workflows.id=$workflowid and model_id=models.id";
$result = $pdo->query($query);
if (!$result) {
	die('Invalid query: ' . error_database());
}
$workflow = $result->fetch(PDO::FETCH_ASSOC);
$folder = $workflow['folder'];

// get information about all runs
$runfolder = $folder . DIRECTORY_SEPARATOR . "run";
$runs = explode("\n", file_get_contents($runfolder . DIRECTORY_SEPARATOR . "runs.txt"));
$files = array();
foreach($runs as $run) {
	if ($run == "") continue;
	foreach(scandir($runfolder . DIRECTORY_SEPARATOR . $run) as $file) {
		if (($file == ".") or ($file == "..")) continue;
		if ($file == "README.txt") continue;
		if ($file == "sipnet.clim") continue;
		if (strstr($file, ".orig") !== false) continue;
		$files[] = $file;
	}
}
$files = array_unique($files);

?>
<!DOCTYPE html>
<html>
<head>
<title>PEcAn Advanced Edit</title>
<meta name="viewport" content="width=device-width, initial-scale=1.0, user-scalable=no" />
<meta http-equiv="content-type" content="text/html; charset=UTF-8" />
<link rel="stylesheet" type="text/css" href="sites.css" />
<script type="text/javascript" src="jquery-1.7.2.min.js"></script>
<script type="text/javascript">
	window.onresize = resize;
	window.onload = resize;
	modified = false;
	
	function resize() {
		$("#stylized").height($(window).height() - 5);
		$("#output").height($(window).height() - 1);
		$("#output").width($(window).width() - $('#stylized').width() - 5);
		$("#editor").width($("#output").width() - 20);
		$("#editor").height($("#output").height() - $("#save").height() - 30);
	}
	
	function prevStep() {
		$("#formprev").submit();
	}

	function nextStep() {
		$("#formnext").submit();
	}

	function modifiedFile() {
		$("#save").removeAttr("disabled");
		modified=true;
	}

	function saveFile() {
		var name="run/" + $('#run')[0].value + "/" + $('#file')[0].value;
		jQuery.post("savefile.php", {"name":name, "workflowid":<?=$workflowid?>, "data":$('#editor').val()}, function(data) {
			console.log(data);
			$("#save").attr("disabled", "disabled");
			modified=false;      
		});
	}

	function loadFile() {
		if (modified) {
			alert("File is modified");
		}
		$("#save").attr("disabled", "disabled");
		modified=false;      
		var name="run/" + $('#run')[0].value + "/" + $('#file')[0].value;
		jQuery.post("loadfile.php", {"name":name, "workflowid":<?=$workflowid?>}, function(data) {
			$("#editor").val(data);
		});
	}

	function endsWith(haystack, needle) {
		return (haystack.substr(haystack.length - needle.length) === needle);
	}
	
	$(document).ready(function () {
		loadFile();
	});
	
</script>
</head>
<body>
<div id="wrap">
	<div id="stylized">
		<form id="formprev" method="POST" action="selectdata.php">
<?php if ($offline) { ?>
			<input name="offline" type="hidden" value="offline">
<?php } ?>
			<input type="hidden" name="siteid" value="<?=$workflow['site_id']?>" />
			<input type="hidden" name="modelid" value="<?=$workflow['model_id']?>" />
			<input type="hidden" name="modeltype" value="<?=$workflow['model_type']?>" />
			<input type="hidden" name="hostname" value="<?=$workflow['hostname']?>" />
		</form>
		<form id="formnext" method="POST" action="run_stage2.php">
<?php if ($offline) { ?>
			<input name="offline" type="hidden" value="offline">
<?php } ?>
			<input type="hidden" name="workflowid" value="<?=$workflowid?>" />
			<h1>Advanced Edit</h1>
			<p>Select a file to edit.</p>

			<label>Run Selection</label>
			<select id="run" name="file" onChange="loadFile();">
<?php
	foreach($runs as $run) {
		if ($run == "") continue;
		print("<option value=\"$run\">$run</option>\n");
	}
?>
            </select>
            <div class="spacer"></div>

			<label>File Selection</label>
			<select id="file" name="file" onChange="loadFile();">
<?php
	foreach($files as $file) {
		print("<option value=\"$file\">$file</option>\n");
	}
?>
            </select>
            <div class="spacer"></div>

			<p></p>
			<span id="error" class="small"></span>
			<input id="prev" type="button" value="Prev" onclick="prevStep();" />
			<input id="next" type="button" value="Next" onclick="nextStep();" />		
			<div class="spacer"></div>
		</form>
	</div>
	<div id="output">
		<textarea name="editor" id="editor" onKeyPress="modifiedFile();"></textarea><br/>
		<input type="button" name="save" id="save" value="Save" onclick="saveFile();" disabled/>
	</div>
</div>
</body>
</html>

<?php 
$pdo = null;
?>
