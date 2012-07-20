<?php
/**
 * Copyright (c) 2012 University of Illinois, NCSA.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the 
 * University of Illinois/NCSA Open Source License
 * which accompanies this distribution, and is available at
 * http://opensource.ncsa.illinois.edu/license.html
 */
// offline mode?
if (isset($_REQUEST['offline'])) {
	$offline=true;
} else {
	$offline=false;
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

// system parameters
require("system.php");

// database parameters
require("dbinfo.php");
$connection = open_database();

// get site information
$result = mysql_query("SELECT * FROM sites WHERE sites.id=$siteid");
if (!$result) {
	die('Invalid query: ' . mysql_error());
}
$siteinfo = mysql_fetch_assoc($result);

// get model info
$result = mysql_query("SELECT * FROM models WHERE models.id=$modelid");
if (!$result) {
	die('Invalid query: ' . mysql_error());
}
$model = mysql_fetch_assoc($result);

// split based on model info
if (preg_match('/^ed/', strtolower($model["model_name"]))) {
	$model['type'] = "ED2";
} else if (preg_match('/^sipnet/', strtolower($model["model_name"]))) {
	$model['type'] = "SIPNET";
} else {
	die("Unknown model type {$model['model_name']}");
}	

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
<script type="text/javascript" src="http://www.google.com/jsapi"></script>
<?php }?>
<script type="text/javascript">
	window.onresize = resize;
	window.onload = resize;
	
	function resize() {
		$("#stylized").height($(window).height() - 5);
		$("#map_canvas").height($(window).height() - 1);
		$("#map_canvas").width($(window).width() - $('#stylized').width() - 5);
	}
	
	function validate() {
		// check PFTs
		if ($("#pft").val() == null) {
			$("#next").attr("disabled", "disabled");
			$("#error").html("Select a pft to continue");
			return;
		}

<?php
if ($model["type"] == "ED2") {
?>
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

		// check MET
		if ($("#met").val() == null) {
			$("#next").attr("disabled", "disabled");
			$("#error").html("Select a MET file to continue");
			return;
		}

		// check psscss
		if ($("#psscss").val() == null) {
			$("#next").attr("disabled", "disabled");
			$("#error").html("Select site files to continue");
			return;
		}
<?php
} else if ($model["type"] == "SIPNET") {
?>
		// check Climate
		if ($("#climate").val() == null) {
			$("#next").attr("disabled", "disabled");
			$("#error").html("Select a climate file to continue");
			return;
		}
<?php
}
?>
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
	
<?php } else {?>
    google.load("maps", "3",  {other_params:"sensor=false"});
	google.setOnLoadCallback(mapsLoaded);
    
    function mapsLoaded() {
		var latlng = new google.maps.LatLng(<?=$siteinfo['lat']?>, <?=$siteinfo['lon']?>);
		var myOptions = {
			zoom: 10,
			center: latlng,
			mapTypeId: google.maps.MapTypeId.ROADMAP
		}

		var map = new google.maps.Map(document.getElementById("map_canvas"), myOptions);

		// create a marker
		var marker = new google.maps.Marker({position: latlng, map: map});

		// create the tooltip and its text
		var info="<b><?=$siteinfo['sitename']?></b><br />";
		info+="<?=$siteinfo['city']?>, <?=$siteinfo['state']?>, <?=$siteinfo['country']?>";
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
		<form id="formprev" method="POST" action="selectsite.php">
<?php if ($offline) { ?>
			<input name="offline" type="hidden" value="offline">
<?php } ?>
		</form>
		<form id="formnext" method="POST" action="runpecan.php">
<?php if ($offline) { ?>
			<input name="offline" type="hidden" value="offline">
<?php } ?>
			<input type="hidden" name="siteid" value="<?=$siteid?>" />
			<input type="hidden" name="modelid" value="<?=$modelid?>" />
			<input type="hidden" name="modeltype" value="<?=$model["type"]?>" />
			<input type="hidden" name="hostname" value="<?=$hostname?>" />
			<h1>Selected Site</h1>
			<p>Set parameters for the run.</p>

			<label>PFT</label>
			<select id="pft" name="pft[]" multiple size=5 onChange="validate();">
<?php 
// show list of PFTs
if ($model["type"] == "ED2") {
	$result = mysql_query("SELECT * FROM pfts WHERE name NOT LIKE 'sipnet%' ORDER BY name;");
} else if ($model["type"] == "SIPNET") {
	$result = mysql_query("SELECT * FROM pfts WHERE name LIKE 'sipnet%' ORDER BY name;");
} else  {
	$result = mysql_query("SELECT * FROM pfts ORDER BY name");
}
if (!$result) {
	die('Invalid query: ' . mysql_error());
}
$pfts = "";
while ($row = @mysql_fetch_assoc($result)){
	print "<option value='{$row['name']}'>{$row['name']}</option>\n";
}
?>
			</select>
			<div class="spacer"></div>
			
<?php
if ($model["type"] == "ED2") {
?>
                        <label>Start Date</label>
                        <input type="text" name="start" id="start" value="2006/01/01" onChange="validate();"/>
                        <div class="spacer"></div>

                        <label>End Date</label>
                        <input type="text" name="end" id="end" value="2006/12/31" onChange="validate();"/>
                        <div class="spacer"></div>

			<label>MET Data file</label>
			<select id="met" name="met" onChange="validate();">
<?php
        // setup default part of query
	$query="SELECT file_path AS file, name, start_date, end_date FROM inputs, dbfiles, machines WHERE inputs.site_id=$siteid AND inputs.file_id=dbfiles.file_id AND machines.hostname='${_REQUEST['hostname']}' AND dbfiles.machine_id=machines.id";

	// get met data
	$result = mysql_query($query . " AND dbfiles.format_id=12");
	if (!$result) {
		die('Invalid query: ' . mysql_error());
	}
	while ($row = @mysql_fetch_assoc($result)){
		$row['name']="ED " . substr($row['start_date'], 0, 4) . "-" . substr($row['end_date'], 0, 4);
		print "<option value='{$row['file']}'>{$row['name']}</option>\n";
	}
?>
			</select>
			<div class="spacer"></div>

            <label>Site files (Site/PSS/CSS)</label>
            <select id="psscss" name="psscss" onChange="validate();">
<?php 
	// get psscss data
	$result = mysql_query($query . " AND dbfiles.format_id=10");
	if (!$result) {
		die('Invalid query: ' . mysql_error());
	}
	while ($row = @mysql_fetch_assoc($result)){
		$path = substr($row['file'], 0,  1+strripos($row['file'], '/'));
		print "<option value='$path'>{$row['name']}</option>\n";
	}
	print "<option value='FIA'>Use FIA</option>\n";
?>
            </select>
            <div class="spacer"></div>

<?php
} else if ($model["type"] == "SIPNET") {
        // setup default part of query
        $query="SELECT dbfiles.file_id, name, start_date, end_date FROM inputs, dbfiles, machines WHERE inputs.site_id=$siteid AND inputs.file_id=dbfiles.file_id AND machines.hostname='${_REQUEST['hostname']}' AND dbfiles.machine_id=machines.id";
?>

                        <label>Climate Data file</label>
                        <select id="climate" name="climate" onChange="validate();">
<?php
        // get met data
        $result = mysql_query($query . " AND dbfiles.format_id=24");
        if (!$result) {
                die('Invalid query: ' . mysql_error());
        }
        while ($row = @mysql_fetch_assoc($result)){
                $row['name']="CLIMATE " . substr($row['start_date'], 0, 4) . "-" . substr($row['end_date'], 0, 4);
                print "<option value='{$row['file_id']}'>{$row['name']}</option>\n";
        }
?>
                        </select>
                        <div class="spacer"></div>
<?php
}
?>			
			<p></p>
			<span id="error" class="small"></span>
			<input id="prev" type="button" value="Prev" onclick="prevStep();" />
			<input id="next" type="button" value="Next" onclick="nextStep();" />		
			<div class="spacer"></div>
		</form>
	</div>
	<div id="map_canvas">
		name : <b><?=$siteinfo["sitename"]?></b><br/>
		address : <?=$siteinfo["city"]?>, <?=$siteinfo["country"]?><br/>
		location : <?=$siteinfo["lat"]?>, <?=$siteinfo["lon"]?><br/>
	</div>
</div>
</body>
</html>

<?php 
close_database($connection);
?>
