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
$userok=isset($_REQUEST['userok']);
$offline=isset($_REQUEST['offline']);
if (isset($_REQUEST['advanced_edit'])) {
	$advanced_edit = "checked";
} else {
	$advanced_edit = "";
}

if (!isset($_REQUEST['siteid'])) {
	die("Need a siteid.");
}
$siteid=$_REQUEST['siteid'];
if (!isset($_REQUEST['modelid'])) {
	die("Need a modelid.");
}
$modelid=$_REQUEST['modelid'];
if (!isset($_REQUEST['hostname'])) {
	die("Need a hostname.");
}
$hostname=$_REQUEST['hostname'];
if (isset($_REQUEST['psscss'])) {
	$psscss = $_REQUEST['psscss'];
} else {
	$psscss = "";
}

# parse original form data
$selected_pfts = array();
if (isset($_REQUEST['pft'])) {
	$selected_pfts = $_REQUEST['pft'];
}
$startdate = "2006/01/01";
if (isset($_REQUEST['start'])) { 
	$startdate=$_REQUEST['start'];
}
$enddate = "2006/12/31";
if (isset($_REQUEST['end'])) { 
	$enddate=$_REQUEST['end'];
}
$email="";
if (isset($_REQUEST['email'])) {
	$email=$_REQUEST['email'];
}

// get site information
$stmt = $pdo->prepare("SELECT * FROM sites WHERE sites.id=?");
if (!$stmt->execute(array($siteid))) {
	die('Invalid query: ' . error_database());
}
$siteinfo = $stmt->fetch(PDO::FETCH_ASSOC);
$stmt->closeCursor();

// get model info
$stmt = $pdo->prepare("SELECT * FROM models WHERE models.id=?");
if (!$stmt->execute(array($modelid))) {
	die('Invalid query: ' . error_database());
}
$model = $stmt->fetch(PDO::FETCH_ASSOC);
$stmt->closeCursor();

?>
<!DOCTYPE html>
<html>
<head>
<title>PEcAn Parameter Selection</title>
<meta name="viewport" content="width=device-width, initial-scale=1.0, user-scalable=no" />
<meta http-equiv="content-type" content="text/html; charset=UTF-8" />
<link rel="stylesheet" type="text/css" href="sites.css" />
<script type="text/javascript" src="jquery-1.7.2.min.js"></script>
<?php if (!$offline) {?>
<script type="text/javascript" src="//www.google.com/jsapi"></script>
<?php }?>
<script type="text/javascript">
	function validate() {
		// check PFTs
		if ($("#pft").val() == null) {
			$("#next").attr("disabled", "disabled");
			$("#error").html("Select a pft to continue");
			return;
		}

<?php if (($model["model_type"] == "ED2") || ($model["model_type"] == "BIOCRO")) { ?>
		// check dates
		var start = checkDate($("#start").val(), "Start");
		if (!start) {
	        	return;
		}
		var end = checkDate($("#end").val(), "End");
		if (!end) {
			return;
		}
		if (start >= end) {
			$("#next").attr("disabled", "disabled");
			$("#error").html("End date should be after start date.");
			return;
		}
<?php } ?>

<?php if (($model["model_type"] == "ED2") || ($model["model_type"] == "SIPNET")) { ?>
		// check MET
		if ($("#met").val() == null) {
			$("#next").attr("disabled", "disabled");
			$("#error").html("Select a weather file to continue");
			return;
		}
<?php } ?>

<?php if ($model["model_type"] == "ED2") { ?>
		// check psscss
		if ($("#psscss").val() == null) {
			$("#next").attr("disabled", "disabled");
			$("#error").html("Select site files to continue");
			return;
		}
<?php } ?>

		// all is OK
		$("#next").removeAttr("disabled");       
		$("#error").html("&nbsp;");
	}
	    
	function prevStep() {
		$("#formprev").submit();
	}

	function nextStep() {
		$("#formnext").submit();
	}
	
    function checkDate(date, field) {
    	var arr = date.match(/^(\d{4})\/(\d{1,2})\/(\d{1,2})$/);
        if (arr == null) {
	        $("#next").attr("disabled", "disabled");
	        $("#error").html(field + " date should be entered as \"YYYY/MM/DD\"");
            return false;
		}

        arr[1] = parseInt(arr[1], 10);
        arr[2] = parseInt(arr[2], 10)-1;
        arr[3] = parseInt(arr[3], 10);
        var test = new Date(arr[1], arr[2], arr[3]);

        if (arr[1] != test.getFullYear() || arr[2] != test.getMonth() || arr[3] != test.getDate()) {
	        $("#next").attr("disabled", "disabled");
	        $("#error").html(field + "  date is not a valid date.");
            return false;
        }

        return test;
    }

<?php if ($offline) { ?>
	$(document).ready(function () {
		validate();
	});
<?php } else { ?>
    google.load("maps", "3",  {other_params:"sensor=false"});
	google.setOnLoadCallback(mapsLoaded);
    
    function mapsLoaded() {
		var latlng = new google.maps.LatLng(<?php echo $siteinfo['lat']; ?>, <?php echo $siteinfo['lon']; ?>);
		var myOptions = {
			zoom: 10,
			center: latlng,
			mapTypeId: google.maps.MapTypeId.ROADMAP
		}

		var map = new google.maps.Map(document.getElementById("output"), myOptions);

		// create a marker
		var marker = new google.maps.Marker({position: latlng, map: map});

		// create the tooltip and its text
		var info="<b><?php echo $siteinfo['sitename']; ?></b><br />";
		info+="<?php echo $siteinfo['city']; ?>, <?php echo $siteinfo['state']; ?>, <?php echo $siteinfo['country']; ?>";
		var infowindow = new google.maps.InfoWindow({content: info});
		infowindow.open(map, marker);
		validate();
	}
<?php } ?>
</script>
</head>
<body>
<div id="wrap">
	<div id="stylized">
		<h1>Selected Site</h1>
		<p>Set parameters for the run.</p>

		<form id="formprev" method="POST" action="02-modelsite.php">
<?php if ($offline) { ?>
			<input name="offline" type="hidden" value="offline">
<?php } ?>
			<input type="hidden" name="hostname" value="<?php echo $hostname; ?>" />
			<input type="hidden" name="modelid" value="<?php echo $modelid; ?>" />
			<input type="hidden" name="modeltype" value="<?php echo $model["model_type"]; ?>" />
			<input type="hidden" name="siteid" value="<?php echo $siteid; ?>" />
		</form>

		<form id="formnext" method="POST" action="04-runpecan.php">
<?php if ($offline) { ?>
			<input name="offline" type="hidden" value="on">
<?php } ?>
<?php if ($userok) { ?>
			<input name="userok" type="hidden" value="on">
<?php } ?>
			<input type="hidden" name="siteid" value="<?php echo $siteid; ?>" />
			<input type="hidden" name="modelid" value="<?php echo $modelid; ?>" />
			<input type="hidden" name="modeltype" value="<?php echo $model["model_type"]; ?>" />
			<input type="hidden" name="hostname" value="<?php echo $hostname; ?>" />

			<label>PFT</label>
			<select id="pft" name="pft[]" multiple size=5 onChange="validate();">
<?php 
// show list of PFTs
if ($model["model_type"] == "ED2") {
	$result = $pdo->query("SELECT * FROM pfts WHERE name NOT LIKE 'sipnet%' AND name NOT LIKE 'biocro%' ORDER BY name;");
} else  {
	$result = $pdo->query("SELECT * FROM pfts WHERE name LIKE '" . strtolower($model["model_type"]) . "%' ORDER BY name");
}
if (!$result) {
	die('Invalid query: ' . error_database());
}
$pfts = "";
while ($row = @$result->fetch(PDO::FETCH_ASSOC)){
	$name=str_replace(strtolower($model["model_type"]) . ".", "", $row['name']);
	if (in_array($row['name'], $selected_pfts)) {
		print "				<option value='{$row['name']}' selected>$name</option>\n";
	} else {
		print "				<option value='{$row['name']}'>$name</option>\n";
	}
}
print "			</select>\n";
print "			<div class=\"spacer\"></div>\n";

if (($model["model_type"] == "ED2") || ($model["model_type"] == "BIOCRO")) {
?>
			<label>Start Date</label>
			<input type="text" name="start" id="start" value="<?php echo $startdate; ?>" onChange="validate();"/>
			<div class="spacer"></div>
			<label>End Date</label>
			<input type="text" name="end" id="end" value="<?php echo $enddate; ?>" onChange="validate();"/>
			<div class="spacer"></div>
<?php
}

# query to get a file for a host
$query="SELECT inputs.id, name, start_date, end_date" .
       " FROM inputs, dbfiles, machines" .
       " WHERE inputs.site_id=$siteid" .
       " AND dbfiles.container_type='Input' AND dbfiles.container_id=inputs.id" .
       " AND machines.hostname='${_REQUEST['hostname']}' AND machines.id=dbfiles.machine_id";

if (($model["model_type"] == "ED2") || ($model["model_type"] == "SIPNET")) {
	print "			<label>Weather Data file</label>\n";
	print "			<select id=\"met\" name=\"met\" onChange=\"validate();\">\n";

	// get met data
	
	if ($model["model_type"] == "ED2") {
		$result = $pdo->query($query . " AND inputs.format_id=12");
	} else if ($model["model_type"] == "SIPNET") {
		$result = $pdo->query($query . " AND inputs.format_id=24");
	}
	
	if (!$result) {
		die('Invalid query: ' . error_database());
	}
	while ($row = @$result->fetch(PDO::FETCH_ASSOC)){
		$row['name']="Weather " . substr($row['start_date'], 0, 4) . "-" . substr($row['end_date'], 0, 4);
		print "				<option value='{$row['id']}'>{$row['name']}</option>\n";
	}
	print "			</select>\n";
	print "			<div class=\"spacer\"></div>\n";
}

if ($model["model_type"] == "ED2") {
	print "			<label>Site files (Site/PSS/CSS)</label>\n";
	print "			<select id=\"psscss\" name=\"psscss\" onChange=\"validate();\">\n";

	// get psscss data
	$result = $pdo->query($query . " AND inputs.format_id=10");
	if (!$result) {
		die('Invalid query: ' . error_database());
	}
	while ($row = @$result->fetch(PDO::FETCH_ASSOC)){
		if ($psscss == $row['id']) {
			print "			<option value='{$row['id']}' selected>{$row['name']}</option>\n";
		} else {
			print "			<option value='{$row['id']}'>{$row['name']}</option>\n";
		}
	}
	// if ($psscss == "FIA") {
	// 	print "			<option value='FIA' selected>Use FIA</option>\n";
	// } else {
	// 	print "			<option value='FIA'>Use FIA</option>\n";
	// }
	print "			</select>\n";
	print "			<div class=\"spacer\"></div>\n";
}
?>

			<label title="Used to send email when the run is finished.">Email</label>
			<input id="email" name="email" type="text" value="<?php echo $email; ?>"/>	
			<div class="spacer"></div>

			<label title="Allows to edit files generated by PEcAn before model executions.">Advanced edit</label>
			<input id="advanced_edit" name="advanced_edit" type="checkbox" <?php echo $advanced_edit; ?>/>	
			<div class="spacer"></div>

			<p></p>
			<span id="error" class="small">&nbsp;</span>
			<input id="prev" type="button" value="Prev" onclick="prevStep();" />
			<input id="next" type="button" value="Next" onclick="nextStep();" <?php if (!$userok) echo "disabled" ?>/>		
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
		name : <b><?php echo $siteinfo["sitename"]; ?></b><br/>
		address : <?php echo $siteinfo["city"]; ?>, <?php echo $siteinfo["country"]; ?><br/>
		location : <?php echo $siteinfo["lat"]; ?>, <?php echo $siteinfo["lon"]; ?><br/>
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
close_database();
?>
