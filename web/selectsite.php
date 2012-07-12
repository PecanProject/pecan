<?php
// system parameters
require("system.php");

// database parameters
require("dbinfo.php");
$connection = open_database();

// get hosts
$query = "SELECT hostname FROM machines ORDER BY hostname";
$result = mysql_query($query);
if (!$result) {
	die('Invalid query: ' . mysql_error());
}
$hosts = "";
$hostname = gethostname();
while ($row = @mysql_fetch_assoc($result)){
	if ($hostname == $row['hostname']) {
		$hosts = "$hosts<option selected>{$row['hostname']}</option>\n";
	} else {
		$hosts = "$hosts<option>{$row['hostname']}</option>\n";
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
<script type="text/javascript" src="http://www.google.com/jsapi"></script>
<script type="text/javascript">
	google.load("jquery", "1.3.2");
    google.load("maps", "3",  {other_params:"sensor=false"});

    window.onresize = resize;
    window.onload = resize;
    google.setOnLoadCallback(mapsLoaded);
    
	function resize() {
    	$("#stylized").height($(window).height() - 5);
    	$("#map_canvas").height($(window).height() - 1);
    	$("#map_canvas").width($(window).width() - $('#stylized').width() - 5);
    }

    function validate() {
    	if ($("#siteid").val() == "") {
            $("#next").attr("disabled", "disabled");
            $("#error").html("Select a site to continue");
            return;
        }
        if ($("#modelid").val() == null) {
            $("#next").attr("disabled", "disabled");
            $("#error").html("Select a model to continue");
            return;
        }
        if ($("#hostname").val() != "<?=$hostname?>") {
            $("#next").attr("disabled", "disabled");
            $("#error").html("Select <?=$hostname?> to continue");
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
	
    var map = null;
    var infowindow = null;
    var markersArray = [];

    function mapsLoaded() {
		var myLatlng = new google.maps.LatLng(40.11642, -88.243382);
		var myOptions = {
			zoom: 5,
			center: myLatlng,
			mapTypeId: google.maps.MapTypeId.ROADMAP
		}

		map = new google.maps.Map(document.getElementById("map_canvas"), myOptions);
		infowindow = new google.maps.InfoWindow({content: ""});
		hostSelected();
    	validate();
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

    function hostSelected() {
        // remove everything
		if (markersArray) {
			for (i in markersArray) {
				markersArray[i].setMap(null);
			}
			markersArray.length = 0;
		}
		$('#modelid').find('option').remove();	    
		$("#siteid").val("");
		$("#sitename").val("");
    	validate();
		
		// get all sites
		var url="sites.php?host=" + $('#hostname')[0].value;
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
					gmarker.sitename = marker.attr("sitename");
					gmarker.siteid   = marker.attr("siteid");
					gmarker.html  = '<b>' + marker.attr("sitename") + '</b><br />'
					gmarker.html += marker.attr("city") + ', ' + marker.attr("country") + '<br />';

					// add a listener to open the tooltip when a user clicks on one of the markers
					google.maps.event.addListener(gmarker, 'click', function() {
						$("#siteid").val(this.siteid);
						$("#sitename").val(this.sitename);
						infowindow.setContent(this.html);
						infowindow.open(map, this);
				    	validate();
					});
				}
			});
		});

		// get all models
		var url="models.php?host=" + $('#hostname')[0].value;
		jQuery.get(url, {}, function(data) {
			jQuery(data).find("model").each(function() {
				var model = jQuery(this);
				var name = model.attr("name") + " r" + model.attr("revision");
				$('#modelid').append('<option value="' + model.attr("id") + '">' +name + '</option>')
			});
		});
    }
</script>
</head>
<body>
<div id="wrap">
	<div id="stylized">
		<form id="formprev" method="POST" action="index.php">
		</form>
		<form id="formnext" method="POST" action="selectdata.php">
			<h1>Select host</h1>
			<p>Based on the host selected certain sites and models
			will be available. In the current version you can only
			pick as host <b><?=$hostname?></b></p>

			<label>Host:</label>
			<select name="hostname" id="hostname" onChange="hostSelected();">
				<option value="">All Sites</option>
				<?=$hosts?>
			</select>
			<div class="spacer"></div>

			<label>Site:</label>
			<input name="siteid" id="siteid" type="hidden"/>
			<input name="sitename" id="sitename" type="text" readonly value="No site selected" />
			<div class="spacer"></div>

			<label>Model:</label>
			<select name="modelid" id="modelid" onChange="validate();">
			</select>
			<div class="spacer"></div>

			<p></p>
			<span id="error" class="small"></span>
			<input id="prev" type="button" value="Prev" onclick="prevStep();" />
			<input id="next" type="button" value="Next" onclick="nextStep();" />		
			<div class="spacer"></div>
		</form>
	</div>
	<div id="map_canvas"></div>
</div>
</body>
</html>

<?php 
close_database($connection);
?>
