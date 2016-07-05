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
$conversion = (isset($_REQUEST['conversion'])) ? "checked" : "";

$hostname = "";
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
$sitegroupid = "";
if (isset($_REQUEST['sitegroupid'])) {
  $sitegroupid = $_REQUEST['sitegroupid'];
}

// check if fqdn exists
$query = "SELECT id FROM machines WHERE hostname=?";
$stmt = $pdo->prepare($query);
if (!$stmt->execute(array($fqdn))) {
  die('Invalid query: ' . error_database());
}
while ($row = @$stmt->fetch(PDO::FETCH_ASSOC)) {
  $hostname = $fqdn;
}

// get sitegroups
$query = "SELECT id, name FROM sitegroups WHERE public_access OR user_id=? ORDER BY name";
$stmt = $pdo->prepare($query);
if (!$stmt->execute(array(get_userid()))) {
  die('Invalid query: ' . error_database());
}
$sitegroups = "";
while ($row = @$stmt->fetch(PDO::FETCH_ASSOC)) {
  if ($sitegroupid == $row['id']) {
    $sitegroups .= "<option value='${row['id']}' selected>${row['name']}</option>\n";
  } else {
    $sitegroups .= "<option value='${row['id']}'>${row['name']}</option>\n";    
  }
}


?>
<!DOCTYPE html>
<html>
<head>
<title>PEcAn Site/Model Selection</title>
<link rel="shortcut icon" type="image/x-icon" href="favicon.ico" />
<meta name="viewport" content="width=device-width, initial-scale=1.0, user-scalable=no" />
<meta http-equiv="content-type" content="text/html; charset=UTF-8" />
<link rel="stylesheet" type="text/css" href="sites.css" />
<style>
	body {font-size: 62.5%;}
	label, input {display:block}
	input.text { margin-bottom:12px; width:95%; padding:10px;}
	fieldset { padding:0; border:0; margin-top:25px; }
	h1 { font-size: 1.2em; margin: .6em 0; }
	.ui-dialog .ui-state-error { padding: .3em; } 
	.validateTips { border: 1px solid transparent; padding: 0.3em; } 
	#container { display: table; }
	#row { display: table-row; }
	#left, #right, #middle { display: table-cell; }
</style>
<script type="text/javascript" src="jquery-1.7.2.min.js"></script>
<?php if (!$offline) {?>
<script type="text/javascript" src="//www.google.com/jsapi"></script>
<?php }?>
<?php if (!$offline) {?>
<script type="text/javascript" src="//code.jquery.com/jquery-1.10.2.js"></script>
<?php }?>
<?php if (!$offline) {?>
<script type="text/javascript" src="//code.jquery.com/ui/1.11.4/jquery-ui.js"></script>
<?php }?>
<?php if (!$offline) {?>
<link rel="stylesheet" type="text/css" href="//code.jquery.com/ui/1.11.4/themes/smoothness/jquery-ui.css"/>
<?php }?>
<script type="text/javascript">

	$(function()  {
	var dialog, form,
		sitename = $( "#txtsitename" ),
		elevation =$( "#txtelevation" ),
		allFields = $( [] ).add( sitename ).add( elevation ),
		tips = $( ".validateTips" );

	function updateTips( t ) {
		tips
			.text( t )
			.addClass( "ui-state-highlight" );
		setTimeout( function() {
			tips.removeClass( "ui-state-highlight", 1500 );
		}, 500 );
	}

	function checkLength( o, n, min, max ) {
		if (o.val().length > max || o.val().length < min ) {
			o.addClass( "ui-state-error" );
			updateTipes ( "Length of " + n + " must be between " + min + " and " + max + "." );
			return false; 
		} else { 
			return true;
		}

	}

 	function checkRegexp ( o, regexp, n ) {
		if ( !(regexp.text( o.val() ) ) ) {
			o.addClass( "ui-state-error" );
			updateTips( n );
			return false;
		} else { 
			return true;
		}
	}

	function addSite() {
		var valid = true;
		allFields.removeClass( "ui-state-error" );

		valid = valid && checkLength ( sitename, "sitename", 3, 16);
		valid = valid && checkLength ( email, "email", 6, 80);

		valid = valid && checkRegexp ( sitename, /^[a-z]([0-9a-z_\s])+$/i, "Site name may consist of a-z, 0-9, underscores, spaces and must begin with a letter.");
		
		if ( valid ) {
			$( "#users tbody" ).append( "<tr>" +
				"<td>" + sitename.val() + "</td>" +
				"<td>" + sitename.val() + "</td>" +
			"</tr>" );
			dialog.dialog( "close" );
		}
		return valid;

	}

	dialog = $( "#dialog-form" ).dialog({
		autoOpen: false,
		height: 800,
		width: 850,
		modal: true,
		buttons: {
			"Create a site": addSite,
			Cancel: function() {
				dialog.dialog( "close" );
			}
		},
		close: function () {
			form[ 0 ].reset();
			allFields.removeClass( "ui-state-error" );
		}
	});

	form = dialog.find( "form" ).on( "submit", function( event ) {
		event.preventDefault();
		addSite();
	});

	$( "#create-site" ).button().on( "click", function() {
		dialog.dialog( "open" );
	});
	});

  var markersArray = [];
  
  function validate() {
    $("#next").removeAttr("disabled");       
    $("#error").html("&nbsp;");

    if ($("#hostname").val() == "") {
      $("#next").attr("disabled", "disabled");
      $("#error").html("Select a host to continue");
      $("#hostlabel").html("Host:")
<?php if ($betydb != "") { ?>
    } else {
      $("#hostlabel").html("Host: (Show in <a href=\"<?php echo $betydb; ?>/machines/" + $("#hostname option:selected")[0].getAttribute("data-id") + "\" target=\"BETY\">BETY</a>)");
<?php } ?>
    }
    if ($("#modelid").val() == null || $("#modelid").val() == "") {
      $("#next").attr("disabled", "disabled");
      $("#error").html("Select a model to continue");
      $("#modellabel").html("Model:");
<?php if ($betydb != "") { ?>
    } else {
      $("#modellabel").html("Model: (Show in <a href=\"<?php echo $betydb; ?>/models/" + $("#modelid").val() + "\" target=\"BETY\">BETY</a>)");
<?php } ?>
    }
<?php if ($betydb != "") { ?>
    if ($("#sitegroupid").val() == null || $("#sitegroupid").val() == "") {
      $("#sitegrouplabel").html("Site Group:");
    } else {
      $("#sitegrouplabel").html("Site Group: (Show in <a href=\"<?php echo $betydb; ?>/sitegroups/" + $("#sitegroupid").val() + "\" target=\"BETY\">BETY</a>)");
    }
<?php } ?>
    if ($("#siteid").val() == "") {
      $("#next").attr("disabled", "disabled");
      $("#error").html("Select a site to continue");
      $("#sitelabel").html("Site:");
<?php if ($betydb != "") { ?>
    } else {
      $("#sitelabel").html("Site: (Show in <a href=\"<?php echo $betydb; ?>/sites/" + $("#siteid").val() + "\" target=\"BETY\">BETY</a>)");
<?php } ?>
    }
  }

  function switchUI(busy) {
    var option = busy ? 'disabled' : false;

    $("#hostname").prop('disabled', option);
    $("#modelid").prop('disabled', option);
    $("#sitegroupid").prop('disabled', option);
    $("#conversion").prop('disabled', option);
    $("#siteid").prop('disabled', option);
  }
        
  function prevStep() {
    $("#formprev").submit();
  }

  function nextStep() {
    $("#formnext").submit();
  }

  function updateData() {
    if ($("#hostname").is(':disabled')) {
      return;
    }
    switchUI(true);

    var curHost = $("#hostname").val();
    var curModel = $("#modelid").val();
    var curSitegroup = $("#sitegroupid").val();
    var curSite = $("#siteid").val();

    // remove everything
    if (markersArray) {
      clearSites();
      markersArray.length = 0;
    }
    $('#hostname').find('option').remove();
    $('#hostname').append('<option value="">Any Host</option>');
    $('#modelid').find('option').remove();
    $('#modelid').append('<option value="">All Models</option>');
    $("#siteid").val("");
    $("#sitename").val("");
    validate();

    // get all sites
    //console.log($('#modelid option:selected'))
    var url = "hostmodelinfo.php?host=" + curHost + "&model=" + curModel + "&sitegroup=" + curSitegroup;
    if ($('#conversion').is(':checked')) {
      url = url + "&conversion=1";
    }
 
    jQuery.get(url, {}, function(data) {
      var jdata = jQuery(data);
      //console.log(data);
      // fill in host list
      jdata.find("host").each(function() {
        var host = jQuery(this);
        var hostname = host.attr("hostname");
        var option = "<option data-id='" + host.attr("id") + "'";
        if(hostname == curHost) {
          option = option + " selected";
        }
        option = option + ">" + hostname + "</option>";
        $('#hostname').append(option);
      });

      // fill in model list
      jdata.find("model").each(function() {
        var model = jQuery(this);
        var name = model.attr("name");
        if (model.attr("revision") != "") {
          name += " (" + model.attr("revision") + ")";
        }
        if(model.attr("id") == curModel) {
          $('#modelid').append('<option value="' + model.attr("id") + '" selected>' + name + '</option>');
        } else {
          $('#modelid').append('<option value="' + model.attr("id") + '">' + name + '</option>');
        }
      });

      // fill in site list      
      jdata.find("marker").each(function() {
        var marker = jQuery(this);
        if (marker.attr("lat") == "" || marker.attr("lon") == "") {
          //console.log("Bad marker (siteid=" + marker.attr("siteid") + " site=" + marker.attr("sitename") + " lat=" + marker.attr("lat") + " lon=" + marker.attr("lon") + ")");
        } else {
          showSite(marker, curSite);
        }
      });
      renderSites(curSite);

      switchUI(false);
    });
  }

  function siteSelected(siteid, sitename) {
    $("#siteid").val(siteid);
    $("#sitename").val(sitename);
    validate();
  }
  
<?php if ($offline) { ?>
  $(document).ready(function () {
    updateData();
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
  google.load("maps", "3");
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
    updateData();

    $("#sitename").keyup(function( event ) {
      var search = $("#sitename").val().toLowerCase();
      markersArray.forEach(function(m) {
        m.setVisible(m.sitename.toLowerCase().indexOf(search) > -1);
      });
    });
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
      <p>Based on the host selected certain sites and models will be
      available.</p>

      <label id="hostlabel">Host:</label>
      <select name="hostname" id="hostname" onChange="updateData();">
        <option selected><?php echo $hostname; ?></option>
      </select>
      <div class="spacer"></div>

      <label id="modellabel">Model:</label>
      <select name="modelid" id="modelid" onChange="updateData();">
        <option selected value="<?php echo $modelid; ?>"><?php echo $modelid; ?></option>
      </select>
      <div class="spacer"></div>

      <label id="sitegrouplabel">Site Group:</label>
      <select name="sitegroupid" id="sitegroupid" onChange="updateData();">
        <option value="">All Sites</option>
        <?php echo $sitegroups; ?>
      </select>
      <div class="spacer"></div>

      <label id="conversionlabel">Conversion:</label>
      <input type="checkbox" id="conversion" name="conversion" onChange="updateData();" <?php echo $conversion; ?>  /> 
      <div class="spacer"></div>

      <label id="sitelabel">Site:</label>
      <input name="siteid" id="siteid" type="hidden" value="<?php echo $siteid; ?>"/>
      <input name="sitename" id="sitename" type="text" />
<?php if ($betydb != "") { ?>
      <span class="small">Add a new site in <a href="<?php echo $betydb; ?>/sites/new" target="BETY">BETY</a>. Requires a refresh of this page after site is added.</span>
<?php } ?>
      <div class="spacer"></div>

      <p></p>
      <label>Workflow</label>
      <span id="error" class="small"></span>
      <input id="prev" type="button" value="Prev" onclick="prevStep();" />
      <input id="next" type="button" value="Next" onclick="nextStep();" />    
      <div class="spacer"></div>




      <input id="create-site" type="button" value="Add Site"/>
	
	<div id="divAddSite">
		<div id="dialog-form" class="ui-widget" title="Create new site">
			<p class="validateTips">All form fields are required.</p>
			<form>
				<fieldset>
					<div id="container">
						<div id="row">
						<label for="sitename" id="lblsitename">Site name</label>
						<input id="txtsitename" size="30" type="text"></input>
						</div>
						<div id="row">
							<div id="left">
								<label id="lblelevation">Elevation (m)</label>
								<input id="txtelevation" size="30" type="text"></input>
							</div>
							<div id="middle">
								<label id="lblmap">Mean Annual Precipitation (mm/yr)</label>
								<input id="txtmap" size="30" type="text"></input>
							</div>
							<div id="right">
								<label id="lblmat">Mean Annual Temperature (C)</label>
								<input id="txtmat" size="30" type="text"></input>
							</div>
						</div>
						<div id="row">
							<div id="left">
								<label id="lblcity">City</label>
								<input id="txtcity" size="30" type="text"></input>
							</div>
							<div id="middle">
								<label id="lblstate">State</label>
								<input id="txtstate" size="30" type="text"></input>
							</div>
							<div id="right">
								<label id="lblcountry">Country</label>
								<input id="txtcountry" size="30" type="text"></input>
							</div>
						</div>
						<div id="row">
						<!-- Column 1 -->
							<label id="lbllat">Lat</label>
							<input id="txtlat" size="30" type="text"></input>
							<label id="lbllong">Long</label>
							<input id="txtlong" size="30" type="text"></input>
							<label id="lblpctsoil">% Soil</label>
							<input id="txtpctsoil" size="30" type="text"></input>
							<label id="lblpctclay">% Clay</label>
							<input id="txtpctclay" size="30" type="text"></input>
							<label id="lblpctsand">% Sand</label>
							<input id="txtpctsand" size="30" type="text"></input>
							<label id="lblsom">SOM</label>
							<input id="txtsom" size="30" type="text"></input>
							<label id="lblgreenhouse">Greenhouse</label>
							<input id="txtgreenhouse" size="30" type="text"></input>
							<!-- Column 2 -->
							<!-- MAP HERE -->
						</div>
						<div id="row">
							<div id="left">
								<label id="lblnotes">Notes</label>
								<textarea id="txtnotes" cols="40" rows="10" type="text"></textarea>
							</div>
							<div id="middle">
								<label id="lblsoilnotes">Soil Notes</label>
								<textarea id="txtsoilnotes" cols="40" rows="10" type="text"></textarea>
							</div>
						</div>
					</div>					
					<!-- Allow form submission with keyboard without duplicating the dialog button -->
					<input type="submit" tabindex="-1" style="position:absolute; top:-1000px">
				</fieldset>
			</form>
		</div>

	<!--	<button id="create-site">Create new site</button> -->
	</div>
      <div class="spacer"></div>
    </form>
<?php whoami(); ?>    
  </div>
  <div id="output"></div>
  <div id="footer"><?php echo get_footer(); ?></div>
</div>
</body>
</html>

<?php
close_database();
?>
