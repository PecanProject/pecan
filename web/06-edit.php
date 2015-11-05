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
    if (get_page_acccess_level() > $min_run_level) {
       header( "Location: history.php");
       close_database();
       exit;
    }
}

# boolean parameters
$offline=isset($_REQUEST['offline']);
$pecan_edit=isset($_REQUEST['pecan_edit']);
$model_edit=isset($_REQUEST['model_edit']);

if (!isset($_REQUEST['workflowid'])) {
	die("Need a workflowid.");
}
$workflowid=$_REQUEST['workflowid'];

// get run information
$stmt = $pdo->prepare("SELECT site_id, model_id, modeltypes.name as model_type, hostname, folder, advanced_edit " .
                      "FROM workflows, models, modeltypes " .
                      "WHERE workflows.id=? and model_id=models.id and modeltypes.id=models.modeltype_id");
if (!$stmt->execute(array($workflowid))) {
  die('Invalid query: ' . error_database());
}
$workflow = $stmt->fetch(PDO::FETCH_ASSOC);
$stmt->closeCursor();
$folder = $workflow['folder'];
close_database();

# check to see if we need to edit pecan.xml or the model files
$files = array();
if ($pecan_edit) {
  $files[] = "pecan.xml";
} else {
  // get information about all runs
  $runfolder = $folder . DIRECTORY_SEPARATOR . "run";
  $runs = explode("\n", file_get_contents($runfolder . DIRECTORY_SEPARATOR . "runs.txt"));
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
	var modified=false;

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
    var runid=$('#run').val();
    if (runid == -1) {
      var name=$('#file').val();
    } else {
      var name="run/" + runid + "/" + $('#file').val();
    }     
		jQuery.post("savefile.php", {"name":name, "workflowid":<?php echo $workflowid; ?>, "data":$('#editor').val()}, function(data) {
			console.log(data);
			$("#save").attr("disabled", "disabled");
			modified=false;
		});
	}

	function loadFile() {
		if (modified) {
			alert("File is modified");
		}
    var runid=$('#run').val();
		$("#save").attr("disabled", "disabled");
		modified=false;
    if (runid == -1) {
      var name=$('#file').val();
    } else {
      var name="run/" + runid + "/" + $('#file').val();
    }		
		jQuery.post("loadfile.php", {"name":name, "workflowid":<?php echo $workflowid; ?>}, function(data) {
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
		<form id="formprev" method="POST" action="02-modelsite.php">
<?php if ($offline) { ?>
			<input name="offline" type="hidden" value="offline">
<?php } ?>
<?php if ($pecan_edit) { ?>
      <input type="hidden" name="pecan_edit" value="pecan_edit" />
<?php } ?>
<?php if ($model_edit) { ?>
      <input type="hidden" name="model_edit" value="model_edit" />
<?php } ?>
		</form>
		<form id="formnext" method="POST" action="07-continue.php">
<?php if ($offline) { ?>
      <input name="offline" type="hidden" value="offline">
<?php } ?>
<?php if ($pecan_edit) { ?>
      <input type="hidden" name="pecan_edit" value="pecan_edit" />
<?php } ?>
<?php if ($model_edit) { ?>
      <input type="hidden" name="model_edit" value="model_edit" />
<?php } ?>
      <input type="hidden" name="workflowid" value="<?php echo $workflowid; ?>" />
			<h1>Advanced Edit</h1>
			<p>Select a file to edit.</p>

<?php if ($pecan_edit) { ?>
      <input type="hidden" id="run" value="-1" />
<?php } else { ?>
			<label>Run Selection</label>
			<select id="run" name="id" onChange="loadFile();">
<?php
	foreach($runs as $run) {
		if ($run == "") continue;
		print("<option value=\"$run\">$run</option>\n");
	}
?>
            </select>
            <div class="spacer"></div>
<?php } ?>
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
			<input id="prev" type="button" value="Start Over" onclick="prevStep();" />
			<input id="next" type="button" value="Continue" onclick="nextStep();" />		
			<div class="spacer"></div>
		</form>
<?php
  if (check_login()) {
    echo "<p></p>";
    echo "Logged in as " . get_user_name();
    echo "<a href=\"index.php?logout\" id=\"logout\">logout</a>";
  }
?>    
	</div>
	<div id="output">
		<textarea name="editor" id="editor" onKeyPress="modifiedFile();"></textarea><br/>
		<input type="button" name="save" id="save" value="Save" onclick="saveFile();" disabled/>
	</div>
  <div id="footer"><?php echo get_footer(); ?></div>
</div>
</body>
</html>
