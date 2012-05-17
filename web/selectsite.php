<?php
if (!isset($_REQUEST['siteid'])) {
	die("Need a siteid.");
} else {
	$siteid=$_REQUEST['siteid'];
}

if (isset($_REQUEST['host']) && ($_REQUEST['host'] != "")) {
	$where="AND filepath LIKE '{$_REQUEST['host']}:%'";
} else {
	$where="";
}

// system parameters
require("system.php");

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

// get site information
$query = "SELECT * FROM sites WHERE sites.id=$siteid";
$result = mysql_query($query);
if (!$result) {
	die('Invalid query: ' . mysql_error());
}
$siteinfo = mysql_fetch_assoc($result);

// get met data
// FIXME what to do if not on this host?
$query = "SELECT SUBSTRING_INDEX(filepath, ':', -1) AS file, CONCAT(SUBSTRING(start_date, 1, 4), '-', SUBSTRING(end_date, 1, 4)) AS name FROM inputs WHERE site_id=$siteid AND format_id=12 $where";
$result = mysql_query($query);
if (!$result) {
	die('Invalid query: ' . mysql_error());
}
$mets = "";
while ($row = @mysql_fetch_assoc($result)){
	$mets = "$mets<option value='{$row['file']}'>{$row['name']}</option>\n";
}

// get psscss data
$query = "SELECT SUBSTRING_INDEX(filepath, ':', -1) AS file, name FROM inputs WHERE site_id=$siteid AND format_id=10 $where";
$result = mysql_query($query);
if (!$result) {
	die('Invalid query: ' . mysql_error());
}
$psscss = "";
while ($row = @mysql_fetch_assoc($result)){
    $path = substr($row['file'], 0,  1+strripos($row['file'], '/'));
	$psscss = "$psscss<option value='$path'>{$row['name']}</option>\n";
}
$psscss = "$psscss<option value='FIA'>Use FIA</option>\n";

// show list of PFTs
$query = "SELECT * FROM pfts ORDER BY name";
$result = mysql_query($query);
if (!$result) {
	die('Invalid query: ' . mysql_error());
}
$pfts = "";
while ($row = @mysql_fetch_assoc($result)){
	$pfts = "$pfts<option value='{$row['name']}'>{$row['name']}</option>\n";
}
?>
<!DOCTYPE html>
<html>
<head>
<title>EBI Sites</title>
<meta name="viewport" content="width=device-width, initial-scale=1.0, user-scalable=no" />
<meta http-equiv="content-type" content="text/html; charset=UTF-8" />
<link rel="stylesheet" type="text/css" href="sites.css" />
<script type="text/javascript" src="http://www.google.com/jsapi"></script>
<script type="text/javascript">
	google.load("maps", "3",  {other_params:"sensor=false"});
	google.load("jquery", "1.3.2");

        function checkDate(date, field) {
                var arr = date.match(/^(\d{4})\/(\d{1,2})\/(\d{1,2})$/);
                if (arr == null) {
                        alert(field + " date should be entered as \"YYYY/MM/DD\"");
                        return false;
                }

                arr[1] = parseInt(arr[1], 10);
                arr[2] = parseInt(arr[2], 10)-1;
                arr[3] = parseInt(arr[3], 10);
                var test = new Date(arr[1], arr[2], arr[3]);

                if (arr[1] != test.getFullYear() || arr[2] != test.getMonth() || arr[3] != test.getDate()) {
                        alert(field + " date is not a valid date.");
                        return false;
                }

                return test;
        }

	function validate(form) {
                var start = checkDate(form.start.value, "Start");
                if (!start) {
                        return false;
                }
                var end = checkDate(form.end.value, "End");
                if (!end) {
                        return false;
                }

                // see if start date is before end date
                if (start >= end) {
                        alert("End date should be after start date.");
                        return false;
                }

		// check pft
		var count = 0;
		for (i=0; i<form["pft[]"].length; i++) {
			if (form["pft[]"][i].selected) {
				count++;
			}
		}
		if (count == 0) {
			alert("Make sure at least one PFT is selected.");
			return false;
		}

		// form is valid
		return true;
	}

	function resize(){
		$("#stylized").height($(window).height() - 5);
		$("#map_canvas").height($(window).height() - 1);
		$("#map_canvas").width($(window).width() - $('#form').width() - 5);

	} 

	function initialize() {
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
	}

	window.onresize = resize;
	window.onload = resize;
	google.setOnLoadCallback(initialize);
    </script>
</head>
<body>
<div id="wrap">
	<div id="stylized">
		<form id="form" action="runsite.php" method="post" onsubmit="return validate(this);">
			<input type="hidden" name="siteid" value="<?=$siteid?>" />
			<h1>Selected Site</h1>
			<p>Set parameters for the run.</p>

			<label>MET Data file</label>
			<select name="met">
			<?=$mets?>
			</select>
			<div class="spacer"></div>

            <label>Site files (Site/PSS/CSS)</label>
            <select name="psscss">
			<?=$psscss?>
            </select>
            <div class="spacer"></div>

			<label>Start Date</label>
			<input type="text" name="start" value="2006/01/01" />
			<div class="spacer"></div>
			
			<label>End Date </label>
			<input type="text" name="end" value="2006/12/31" />
			<div class="spacer"></div>
			
			<label>PFT</label>
			<select name="pft[]" multiple size=5>
			<?=$pfts?>
			</select>
			<div class="spacer"></div>

			<button>Submit Run</button>
			<div class="spacer"></div>
			
			&nbsp;
		</form>
	</div>
	<div id="map_canvas"></div>
</div>
</body>
</html>
