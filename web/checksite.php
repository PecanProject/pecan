<?php
/**
 * Copyright (c) 2012 University of Illinois, NCSA.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the 
 * University of Illinois/NCSA Open Source License
 * which accompanies this distribution, and is available at
 * http://opensource.ncsa.illinois.edu/license.html
 */

// what site is earth
$earth=1118;

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

// check site
if ($hostname != "" && $modelid != "" && $siteid != "") {
  $results = "";

  // Get list of all formats needed for model
  $stmt = $pdo->prepare("SELECT format_id, mime_type, name FROM modeltypes_formats, models, formats" .
                        " WHERE modeltypes_formats.modeltype_id=models.modeltype_id" .
                        " AND models.id=? AND formats.id=format_id;");
  if (!$stmt->execute(array($modelid))) {
    die('Invalid query: ' . error_database());
  }
  $formats=$stmt->fetchAll(PDO::FETCH_ASSOC);
  $stmt->closeCursor();

  // Find all formats that are in world
  $stmt = $pdo->prepare("SELECT DISTINCT format_id FROM inputs, dbfiles, machines" .
                        " WHERE inputs.site_id=${earth} AND inputs.id=dbfiles.container_id" .
                        " AND dbfiles.container_type='Input' and dbfiles.machine_id=machines.id" .
                        " AND machines.hostname=? GROUP BY format_id;");
  if (!$stmt->execute(array($hostname))) {
    die('Invalid query: ' . error_database());
  }
  $world = $stmt->fetchAll(PDO::FETCH_COLUMN, 0);
  $stmt->closeCursor();

  // print all formats found in the world
  $header = false;
  $leftover = array();
  foreach($formats as $row) {
    if (in_array($row['format_id'], $world)) {
      if (! $header) {
        $results .= "<label>World Provided</label><div class=\"spacer\"></div>\n";
        $header = true;
      }
      $results .= "${row['format_id']} - ${row['name']} (${row['mime_type']})<br/>\n";
    } else {
      array_push($leftover, $row);
    }
  }
  $formats = $leftover;

  // Get list of all sites, formats
  $stmt = $pdo->prepare("SELECT DISTINCT format_id FROM inputs, dbfiles, machines" .
                        " WHERE inputs.site_id=? AND inputs.id=dbfiles.container_id" .
                        " AND dbfiles.container_type='Input' and dbfiles.machine_id=machines.id" .
                        " AND machines.hostname=? GROUP BY format_id;");
  if (!$stmt->execute(array($siteid, $hostname))) {
    die('Invalid query: ' . error_database());
  }
  $site = $stmt->fetchAll(PDO::FETCH_COLUMN, 0);
  $stmt->closeCursor();

  // print all formats found in the site
  $header = false;
  $leftover = array();
  foreach($formats as $row) {
    if (in_array($row['format_id'], $site)) {
      if (! $header) {
        $results .= "<label>Site Provided</label><div class=\"spacer\"></div>\n";
        $header = true;
      }
      $results .= "${row['format_id']} - ${row['name']} (${row['mime_type']})<br/>\n";
    } else {
      array_push($leftover, $row);
    }
  }
  $formats = $leftover;

  // list the formats needed for the model
  if (count($formats) != 0) {
    $temp = $results;

    $results = "<h1>Check Failed</h1>\n" .
               "<p>Not all inputs are found.</p>";
    $results .= "<label>Missing Inputs</label><div class=\"spacer\"></div>\n";
    foreach($formats as $row) {
      $results .= "${row['format_id']} - ${row['name']} (${row['mime_type']})<br/>\n";
    }
    $results .= $temp;

  } else {
    if ($results == "") {
      $results = "<h1>Check OK</h1>\n" .
                 "<p>This combination requires no inputs</p>";
    } else {
      $results = "<h1>Check OK</h1>\n" .
                 "<p>This combination requires inputs which are statosfied</p>\n" .
                 "${results}\n";
    }
  }
}

// get hosts
$query = "SELECT hostname FROM machines ORDER BY hostname";
$result = $pdo->query($query);
if (!$result) {
  die('Invalid query: ' . error_database());
}
$hosts = "";
while ($row = @$result->fetch(PDO::FETCH_ASSOC)) {
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
<title>PEcAn Site Checker</title>
<meta name="viewport" content="width=device-width, initial-scale=1.0, user-scalable=no" />
<meta http-equiv="content-type" content="text/html; charset=UTF-8" />
<link rel="stylesheet" type="text/css" href="sites.css" />
<script type="text/javascript" src="jquery-1.7.2.min.js"></script>
<script type="text/javascript" src="//www.google.com/jsapi"></script>
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

  function getSites() {
    var curSite = $("#siteid").val(); //we'll clear this and replace it if it still exists in the new model

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
    var url="sites.php";
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
          $('#modelid').append('<option value="' + model.attr("id") + '" selected>' +name + '</option>'); //reselect our curModel if still available
        } else {
          $('#modelid').append('<option value="' + model.attr("id") + '">' +name + '</option>');
        }
      });
    });
  }

  function siteSelected(siteid, sitename) {
    $("#siteid").val(siteid);
    $("#sitename").val(sitename);
    validate();
  }
  
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
    getSites();
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
</script>
</head>
<body>
<div id="wrap">
  <div id="stylized">
    <form id="formprev" method="POST" action="#">
    </form>

    <form id="formnext" method="POST" action="checksite.php">
      <h1>Check Site</h1>
      <p>This form will check if a model can be run on a specific host on a specific site.</p>

      <label>Host:</label>
      <select name="hostname" id="hostname" onChange="hostSelected();">
        <option value="">All Sites</option>
        <?php echo $hosts; ?>
      </select>
      <div class="spacer"></div>

      <label>Model:</label>
      <select name="modelid" id="modelid" onChange="validate();">
        <option selected value="<?php echo $modelid; ?>"><?php echo $modelid; ?></option>
      </select>
      <div class="spacer"></div>

      <label>Site:</label>
      <input name="siteid" id="siteid" type="hidden" value="<?php echo $siteid; ?>"/>
      <input name="sitename" id="sitename" type="text" readonly value="No site selected" />
      <div class="spacer"></div>

      <label>&nbsp;</label>
      <input id="next" type="button" value="Check" onclick="nextStep();" />    
      <div class="spacer"></div>
      <p></p>
<?php
  if (isset($results)) {
    echo $results;
  }
?>
    </form>
<?php whoami(); ?>    
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
