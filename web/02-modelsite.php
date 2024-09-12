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
$sitegroupid = "";
if (isset($_REQUEST['sitegroupid'])) {
  $sitegroupid = $_REQUEST['sitegroupid'];
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
    $sitegroups .= "<option value='{$row['id']}' selected>{$row['name']}</option>\n";
  } else {
    $sitegroups .= "<option value='{$row['id']}'>{$row['name']}</option>\n";    
  }
}
if ($sitegroupid == "-1") {
  $sitegroups .= "<option value='-1' selected>All Sites</option>\n";
} else {
  $sitegroups .= "<option value='-1'>All Sites</option>\n";    
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
<script type="text/javascript" src="jquery-1.10.2.min.js"></script>
<?php if (!$offline) {?>
  <link rel="stylesheet" href="leaflet/leaflet.css" />
  <script src="leaflet/leaflet.js"></script>
  <link rel="stylesheet" href="leaflet/MarkerCluster.css" />
  <link rel="stylesheet" href="leaflet/MarkerCluster.Default.css" />
  <script src="leaflet/leaflet.markercluster.js"></script>
<?php }?>
  <script type="text/javascript" src="//code.jquery.com/jquery-1.10.2.js"></script>
  <script type="text/javascript" src="//code.jquery.com/ui/1.11.4/jquery-ui.js"></script>
  <link rel="stylesheet" type="text/css" href="//code.jquery.com/ui/1.11.4/themes/smoothness/jquery-ui.css"/>
<script type="text/javascript">
  $(window).load(function() {
<?php if (!$offline) { ?>
    var tiles = L.tileLayer('https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png', {
        maxZoom: 18,
        attribution: '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors, Points &copy 2012 LINZ'
      });

    lmap = L.map('output', {center: L.latLng(0.0, 0.0), zoom: 2, layers: [tiles]});
    markers = L.markerClusterGroup();
    markers.on("click", function(event) {
      siteSelected(event.layer.siteid, event.layer.sitename);
    });

    var timeout;
    $("#sitename").keyup(function( event ) {
      //if you already have a timout, clear it
      if(timeout){ clearTimeout(timeout);}

      //start new time, to perform ajax stuff in 500ms
      timeout = setTimeout(function() {
        var search = $("#sitename").val().toLowerCase();
        var showMarkers = [];
        markersArray.forEach(function(m) {
          if (m.sitename.toLowerCase().indexOf(search) > -1) {
            showMarkers.push(m);
          }      
        });
        markers.clearLayers();
        markers.addLayers(showMarkers);
      },500);
    });

    lmap.on('contextmenu',function(event) {
      $("#lat").val(event.latlng.lat);
      $("#lon").val(event.latlng.lng);
      $("#formnext").attr("action", "02a-createsite.php");
      $("#formnext").submit();
    });
<?php } ?>
    markersArray = [];

    updateData();
  });

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
  $("#lat").val("");
  $("#lon").val("");
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
  clearSites();
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
    var xmlDoc = $.parseXML(data);
    var $xml = $(xmlDoc);

    $xml.find("host").each(function() {
      var host = jQuery(this);
      var option = "<option data-id='" + host.attr("id") + "' value='" + host.attr("hostname") + "'";
      if(host.attr("hostname") == curHost) {
        option = option + " selected";
      }
      option = option + ">" + host.attr("displayname") + "</option>";
      $('#hostname').append(option);
    });

    // fill in model list
    $xml.find("model").each(function() {
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
    $xml.find("marker").each(function() {
      var marker = jQuery(this);
      if (marker.attr("lat") == "" || marker.attr("lon") == "") {
        //console.log("Bad marker (siteid=" + marker.attr("siteid") + " site=" + marker.attr("sitename") + " lat=" + marker.attr("lat") + " lon=" + marker.attr("lon") + ")");
      } else {
        showSite(marker, curSite);
      }
    });
    renderSites(curSite);

    var selected = markersArray.find(function(d) { return d.siteid == curSite; });
    if (selected) {
      markers.zoomToShowLayer(selected);
      selected.openPopup();
      siteSelected(selected.siteid, selected.sitename);
    }

    switchUI(false);
  });
}

function siteSelected(siteid, sitename) {
  $("#siteid").val(siteid);
  $("#sitename").val(sitename);
  validate();
}

<?php if ($offline) { ?>
// ----------------------------------------------------------------------
// OFFLINE CODE
// ----------------------------------------------------------------------
function clearSites() {
  markersArray = [];
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

<?php
} else {
// ----------------------------------------------------------------------
// ONLINE CODE
// ----------------------------------------------------------------------
?>

function clearSites() {
  markersArray = [];
  markers.clearLayers();
}

function showSite(marker, selected) {
  var title = marker.attr("sitename");
  var popup = '<b>' + marker.attr("sitename") + '</b><br />' +
              marker.attr("city") + ', ' + marker.attr("country") + '<br />';
  var pin = L.marker(new L.LatLng(parseFloat(marker.attr("lat")), parseFloat(marker.attr("lon"))));
  pin.options.title = title;
  pin.sitename = marker.attr("sitename");
  pin.siteid = marker.attr("siteid");
  pin.bindPopup(popup);
  markersArray.push(pin);
}

function renderSites(selected) {
  markers.addLayers(markersArray);
  lmap.addLayer(markers);
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
      <h1>Select host, model, site</h1>
      <p>Based on the host selected certain sites and models will be
      available.</p>
      
      <p>Mouse over menu headers for additional info</p>

      <span title="Server that models will be run on">
      <label id="hostlabel">Host:</label></span>
      <select name="hostname" id="hostname" onChange="updateData();">
        <option selected><?php echo $hostname; ?></option>
      </select>
      <div class="spacer"></div>

      <a href="https://pecanproject.github.io/pecan-documentation/latest/pecan-models.html" target="_blank" 
      title="Link opens model descriptions in another window">
      <label id="modellabel">Model:</label></a>
      <select name="modelid" id="modelid" onChange="updateData();">
        <option selected value="<?php echo $modelid; ?>"><?php echo $modelid; ?></option>
      </select>
      <div class="spacer"></div>

      <span title="Filter map by networks of sites">
      <label id="sitegrouplabel">Site Group:</label></span>
      <select name="sitegroupid" id="sitegroupid" onChange="updateData();">
       	<?php echo $sitegroups; ?>
      </select>
      <div class="spacer"></div>

      <span title="Click to add sites to the map that PEcAn can automatically process model inputs for. Default is to show just sites where a model already has all required inputs installed">
      <div>
      <label id="conversionlabel" for="conversion">Conversion:
      <input type="checkbox" id="conversion" name="conversion" onChange="updateData();" <?php echo $conversion; ?>  /> 
      </div></label></span>
      <div class="spacer"></div>

      <span title="Type here to search for sites by name. Click on map to select site">
      <label id="sitelabel">Site:</label></span>
      <input name="lat" id="lat" type="hidden" value=""/>
      <input name="lon" id="lon" type="hidden" value=""/>
      <input name="siteid" id="siteid" type="hidden" value="<?php echo $siteid; ?>"/>
      <input name="sitename" id="sitename" type="text" />
<?php if ($betydb != "") { ?>
      <span class="small">Add new site, right click map</span>
      <span class="small"><a href="<?php echo $betydb; ?>/sites/new" target="BETY">Remove Pins</a></span>
<?php } ?>
      <div class="spacer"></div>

      <p></p>
      <label>Workflow</label>
      <span id="error" class="small"></span>
      <input id="prev" type="button" value="Prev" onclick="prevStep();" />
      <input id="next" type="button" value="Next" onclick="nextStep();" />    
      <div class="spacer"></div>
    </form>
    <?php left_footer(); ?>
  </div>
  <div id="output"></div>
  <div id="footer"><?php echo get_footer(); ?></div>
</div>
</body>
</html>

<?php
close_database();
?>
