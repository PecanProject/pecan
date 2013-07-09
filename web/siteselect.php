<?php
/**
 * Copyright (c) 2012 University of Illinois, NCSA.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the 
 * University of Illinois/NCSA Open Source License
 * which accompanies this distribution, and is available at
 * http://opensource.ncsa.illinois.edu/license.html
 */
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

// get hosts
$query = "SELECT hostname FROM machines ORDER BY hostname";
$result = mysql_query($query);
if (!$result) {
	die('Invalid query: ' . mysql_error());
}
$hosts = "";
$hostname = gethostname();
while ($row = @mysql_fetch_assoc($result)){
	if ($hostname == $row['host']) {
		$hosts = "$hosts<option selected>{$row['hostname']}</option>\n";
	} else {
		$hosts = "$hosts<option>{$row['hostname']}</option>\n";
	}
}

// get models
$query = "SELECT hostname FROM machines ORDER BY hostname";
$result = mysql_query($query);
if (!$result) {
	die('Invalid query: ' . mysql_error());
}
$hosts = "";
$hostname = gethostname();
while ($row = @mysql_fetch_assoc($result)){
	if ($hostname == $row['host']) {
		$hosts = "$hosts<option selected>{$row['hostname']}</option>\n";
	} else {
		$hosts = "$hosts<option>{$row['hostname']}</option>\n";
	}
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
      google.load("jquery", "1.3.2");
      google.load("maps", "3",  {other_params:"sensor=false"});


      var map = null;
      var infowindow = null;
      var markersArray = [];

        function resize() {
                if ($("#stylized").height() < $(window).height()) {
                        $("#stylized").height($(window).height() - 5);
                }
                $("#map_canvas").height($(window).height() - 1);
                $("#map_canvas").width($(window).width() - $('#stylized').width() - 5);
        }

      function mapsLoaded() {
		var myLatlng = new google.maps.LatLng(40.11642, -88.243382);
		var myOptions = {
			zoom: 5,
			center: myLatlng,
			mapTypeId: google.maps.MapTypeId.ROADMAP
		}

		map = new google.maps.Map(document.getElementById("map_canvas"), myOptions);
		infowindow = new google.maps.InfoWindow({content: ""});
		loadSites();
      }

      function goHome() {
		if (navigator.geolocation) {
			navigator.geolocation.getCurrentPosition(function(position) {
				var latlng = new google.maps.LatLng(position.coords.latitude, position.coords.longitude);
				map.panTo(latlng);
				map.setZoom(12);
			}, function() {
				alert("Could not get your location, please make sure you enabled location for safari if you use an iPAD.");
			}, {maximumAge:60000, timeout: 20000});
		} else {  
			alert("I'm sorry, but geolocation services are not supported by your browser.");  
		}  
      }

      function loadSites() {
		if (markersArray) {
			for (i in markersArray) {
				markersArray[i].setMap(null);
			}
			markersArray.length = 0;
		}

		var host=$('#host')[0].value;

		if (host == "") {
			document.getElementById("next").disabled="disabled";
		} else {
			document.getElementById("next").disabled="enabled";
		}
		
		var url="sites.php?host=" + $('#host')[0].value;
		jQuery.get(url, {}, function(data) {
			jQuery(data).find("marker").each(function() {
				// create a marker
				var marker = jQuery(this);
				var latlng;
				if (marker.attr("lat") == "" || marker.attr("lon") == "") {
					console.log("Bad marker (siteid=" + marker.attr("siteid") + " site=" + marker.attr("sitename") + " lat=" + marker.attr("lat") + " lon=" + marker.attr("lon") + ")");
				} else {
					latlng = new google.maps.LatLng(parseFloat(marker.attr("lat")), parseFloat(marker.attr("lon")));
					var gmarker = new google.maps.Marker({position: latlng, map: map});
					markersArray.push(gmarker);

					// create the tooltip and its text
					gmarker.html  = '<b>' + marker.attr("sitename") + '</b><br />'
					gmarker.html += marker.attr("city") + ', ' + marker.attr("country") + '<br />';
					gmarker.html += '<a href="selectsite.php?siteid=' + marker.attr("siteid") + '&host=' + host + '">Select Site</a>';

					// add a listener to open the tooltip when a user clicks on one of the markers
					google.maps.event.addListener(gmarker, 'click', function() {
						infowindow.setContent(this.html);
						infowindow.open(map, this);
					});
				}
			});
		});
      }

      window.onresize = resize;
      window.onload = resize;
      google.setOnLoadCallback(mapsLoaded);
</script>
</head>
<body>
<div id="wrap">
	<div id="stylized">
		<form action="#" id="form">
			<h1>Select host</h1>
			<p>Based on the host selected certain sites will be available.</p>

			<label>Sites with MET data on host:</label>
			<select id="host" onChange="loadSites();">
				<option value="">All Sites</option>
				<?=$hosts?>
			</select>

			<div class="spacer"></div>

			<label>Goto current location</label>
			<input id="home" type="button" value="Home" onclick="goHome();" />
			<div class="spacer"></div>
			<div class="spacer"></div>

			<label>Workflow</label>
			<input id="prev" type="button" disabled="disabled" value="Prev" onclick="prev();" />
			<input id="next" type="button" disabled="disabled" style="float: right" value="Next" onclick="next();" />
			
			<div class="spacer"></div>
			</form>
	</div>
	<div id="map_canvas"></div>
</div>
</body>
</html>
