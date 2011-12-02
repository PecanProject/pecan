<?php
if (!isset($_REQUEST['site'])) {
	die("Need a site.");
} else {
	$site=$_REQUEST['site'];
}

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
$query = "SELECT * FROM sites WHERE sites.id=$site";
$result = mysql_query($query);
if (!$result) {
	die('Invalid query: ' . mysql_error());
}
$siteinfo = mysql_fetch_assoc($result);

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

	function validate(form) {
		var reDate = /^\d{4}\/\d{1,2}\/\d{1,2}$/;

		// check start date
		if(form.start.value != '' && !form.start.value.match(reDate)) {
			alert("Start date should be entered as \"YYYY/MM/DD\"");
			return false;
		}

		// check end date
		if(form.end.value != '' && !form.end.value.match(reDate)) {
			alert("End date should be entered as \"YYYY/MM/DD\"");
			return false;
		}

		// see if start date is before end date
		if (Date.parse(form.start.value) >= Date.parse(form.end.value)) {
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
			<input type="hidden" name="site" value="<?=$site?>" />
			<h1>Selected Site</h1>
			<p>Set parameters for the run.</p>

			<label>MET Data file</label>
			<select name="met">
				<option value="/home/kooper/projects/EBI/ebifarm/met/ED_MET_DRIVER_HEADER">ebifarm</option>
			</select>
			<div class="spacer"></div>

			<label>Start Date</label>
			<input type="text" name="start" value="2006/01/01" />
			<div class="spacer"></div>
			
			<label>End Date </label>
			<input type="text" name="end" value="2006/12/31" />
			<div class="spacer"></div>
			
			<label>PFT</label>
			<select name="pft[]" single size=5>
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
