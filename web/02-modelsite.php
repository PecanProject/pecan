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
$offline=isset($_REQUEST['offline']);

$hostname = $fqdn;
if (isset($_REQUEST['hostname'])) {
	$hostname = $_REQUEST['hostname'];
}
$modelid = "";
if (isset($_REQUEST['modelid'])) {
	$modelid = $_REQUEST['modelid'];
}
$siteid = "";
if (isset($_REQUEST['siteid'])) {
	$siteid = $_REQUEST['siteid'];
}

// get hosts
$query = "SELECT hostname FROM machines ORDER BY hostname";
$result = $pdo->query($query);
if (!$result) {
	die('Invalid query: ' . error_database());
}
$hosts = "";
while ($row = @$result->fetch(PDO::FETCH_ASSOC)) {
	if (in_array($row['hostname'], $hostlist)) {
		if ($hostname == $row['hostname']) {
			$hosts = "$hosts<option selected>{$row['hostname']}</option>\n";
		} else {
			$hosts = "$hosts<option>{$row['hostname']}</option>\n";
		}
	}
}

?>
<!DOCTYPE html>
<html>
<head>
<title>PEcAn Site/Model Selection</title>
<meta name="viewport" content="width=device-width, initial-scale=1.0, user-scalable=no" />
<meta http-equiv="content-type" content="text/html; charset=UTF-8" />
<link rel="stylesheet" type="text/css" href="sites.css" />
<script type="text/javascript" src="jquery-1.7.2.min.js"></script>
<?php if (!$offline) {?>
<script type="text/javascript" src="//www.google.com/jsapi"></script>
<?php }?>
<script type="text/javascript">
    var markersArray = [];
    
    function validate() {
    	if ($("#siteid").val() == "") {
            $("#next").attr("disabled", "disabled");
            $("#error").html("Select a site to continue");
            return;
        }
        if ($("#modelid").val() == null || $("#modelid").val() == "") {
            $("#next").attr("disabled", "disabled");
            $("#error").html("Select a model to continue");
            return;
        }
        if ($("#hostname").val() == "") {
            $("#next").attr("disabled", "disabled");
            $("#error").html("Select a host to continue");
            return;
        }

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

	function modelSelected() {
		var curSite = $("#siteid").val();	//we'll clear this and replace it if it still exists in the new model

		// remove everything
		if (markersArray) {
			clearSites();
			markersArray.length = 0;
		}
		$("#siteid").val("");
		$("#sitename").val("");
		validate();

		// get all sites
		//console.log($('#modelid option:selected'))
		var url="sites.php?host=" + $('#hostname')[0].value + "&model=" + $('#modelid option:selected')[0].value
		jQuery.get(url, {}, function(data) {
			jQuery(data).find("marker").each(function() {
				var marker = jQuery(this);
				if (marker.attr("lat") == "" || marker.attr("lon") == "") {
					console.log("Bad marker (siteid=" + marker.attr("siteid") + " site=" + marker.attr("sitename") + " lat=" + marker.attr("lat") + " lon=" + marker.attr("lon") + ")");
				} else {
					showSite(marker, curSite);
				}
			});
			renderSites(curSite);
		});
	}

	function hostSelected() {
		var curModel = $("#modelid").val();

		// remove everything
		if (markersArray) {
			clearSites();
			markersArray.length = 0;
		}
		$('#modelid').find('option').remove();
		$('#modelid').append('<option value="">All Models</option>');
		validate();

		// get all models
		var url="models.php?host=" + $('#hostname')[0].value;
		jQuery.get(url, {}, function(data) {
			jQuery(data).find("model").each(function() {
				var model = jQuery(this);
				var name = model.attr("name");
				if (model.attr("revision") != "") {
					name += " (" + model.attr("revision") + ")";
				}
				if(model.attr("id") == curModel) {
					$('#modelid').append('<option value="' + model.attr("id") + '" selected>' +name + '</option>');	//reselect our curModel if still available
				} else {
					$('#modelid').append('<option value="' + model.attr("id") + '">' +name + '</option>');
				}
			});
			modelSelected();
		});
	}

    function siteSelected(siteid, sitename) {
		$("#siteid").val(siteid);
		$("#sitename").val(sitename);
		validate();
    }
	
<?php if ($offline) { ?>
	$(document).ready(function () {
		hostSelected();
	});

	function clearSites() {
		$("#output").html("");
	}
	
	function showSite(marker, selected) {
		markersArray.push(marker);
	}

	function renderSites(selected) {
		var sites="<form>";
		for (var i in markersArray) {
			var site = markersArray[i];
			sites = sites + "<div><input type=\"radio\" name=\"site\" value=\"" + site.attr("siteid") + "\"" + " onClick=\"siteSelected(" + site.attr("siteid") + ",'" + site.attr("sitename") + "');\"";
			if (selected == site.attr("siteid")) {
				sites = sites + " checked";
				siteSelected(site.attr("siteid"), site.attr("sitename"));
			}
			sites = sites + ">" + site.attr("sitename") + "</div>";
		}
		sites = sites + "</form>";
		$("#output").html(sites);
	}

<?php } else { ?>
    google.load("maps", "3",  {other_params:"sensor=false"});
    google.setOnLoadCallback(mapsLoaded);
	
    var map = null;
    var infowindow = null;

    function clearSites() {
		for (var i in markersArray) {
			markersArray[i].setMap(null);
		}
	}

    function mapsLoaded() {
		var myLatlng = new google.maps.LatLng(40.11642, -88.243382);
		var myOptions = {
			zoom: 5,
			center: myLatlng,
			mapTypeId: google.maps.MapTypeId.ROADMAP
		}

		map = new google.maps.Map(document.getElementById("output"), myOptions);
		infowindow = new google.maps.InfoWindow({content: ""});
		hostSelected();
	}

	function showSite(marker, selected) {
		var latlng;
		latlng = new google.maps.LatLng(parseFloat(marker.attr("lat")), parseFloat(marker.attr("lon")));
		var gmarker = new google.maps.Marker({position: latlng, map: map});
		markersArray.push(gmarker);

		// create the tooltip and its text
		gmarker.sitename = marker.attr("sitename");
		gmarker.siteid   = marker.attr("siteid");
		gmarker.html  = '<b>' + marker.attr("sitename") + '</b><br />'
		gmarker.html += marker.attr("city") + ', ' + marker.attr("country") + '<br />';

		// add a listener to open the tooltip when a user clicks on one of the markers
		google.maps.event.addListener(gmarker, 'click', function() {
			siteSelected(this.siteid, this.sitename);
			infowindow.setContent(this.html);
			infowindow.open(map, this);
		});

		if (marker.attr("siteid") == selected) {
			siteSelected(gmarker.siteid, gmarker.sitename);
			infowindow.setContent(gmarker.html);
			infowindow.open(map, gmarker);
		}
	}

	function renderSites(selected) {
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
<?php } ?>
</script>
</head>
<body>
<div id="wrap">
	<div id="stylized">
		<form id="formprev" method="POST" action="01-introduction.php">
<?php if ($offline) { ?>
			<input name="offline" type="hidden" value="offline">
<?php } ?>
		</form>

		<form id="formnext" method="POST" action="03-inputs.php">
<?php if ($offline) { ?>
			<input name="offline" type="hidden" value="offline">
<?php } ?>
			<h1>Select host</h1>
			<p>Based on the host selected certain sites and models
			will be available. In the current version you can only
			pick as host <b><?php echo $hostname; ?></b></p>

			<label>Host:</label>
			<select name="hostname" id="hostname" onChange="hostSelected();">
				<option value="">All Sites</option>
				<?php echo $hosts; ?>
			</select>
			<div class="spacer"></div>

			<label>Model:</label>
			<select name="modelid" id="modelid" onChange="modelSelected();">
				<option selected value="<?php echo $modelid; ?>"><?php echo $modelid; ?></option>
			</select>
			<div class="spacer"></div>

			<label>Site:</label>
			<input name="siteid" id="siteid" type="hidden" value="<?php echo $siteid; ?>"/>
			<input name="sitename" id="sitename" type="text" readonly value="No site selected" />
			<div class="spacer"></div>

			<p></p>
			<label>Workflow</label>
			<span id="error" class="small"></span>
			<input id="prev" type="button" value="Prev" onclick="prevStep();" />
			<input id="next" type="button" value="Next" onclick="nextStep();" />		
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
	<div id="output"></div>
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
