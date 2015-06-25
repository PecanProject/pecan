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
}

# boolean parameters
$userok=isset($_REQUEST['userok']);
$offline=isset($_REQUEST['offline']);
$pecan_edit = (isset($_REQUEST['pecan_edit'])) ? "checked" : "";
$model_edit = (isset($_REQUEST['model_edit'])) ? "checked" : "";
$browndog = (isset($_REQUEST['browndog'])) ? "checked" : "";
$qsub = (isset($_REQUEST['qsub'])) ? "checked" : "";

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
$stmt = $pdo->prepare("SELECT sitename, city, state, country, ST_X(ST_CENTROID(sites.geometry)) AS lon, ST_Y(ST_CENTROID(sites.geometry)) AS lat FROM sites WHERE sites.id=?");
if (!$stmt->execute(array($siteid))) {
  die('Invalid query: ' . error_database());
}
$siteinfo = $stmt->fetch(PDO::FETCH_ASSOC);
$stmt->closeCursor();

// get list of inputs
$stmt = $pdo->prepare("SELECT tag, required, formats.name" .
                      " FROM modeltypes_formats, models, formats" .
                      " WHERE models.id=? AND modeltypes_formats.modeltype_id=models.modeltype_id" .
                      " AND modeltypes_formats.format_id=formats.id AND modeltypes_formats.input" .
                      " ORDER BY formats.name;");
if (!$stmt->execute(array($modelid))) {
  die('Invalid query: ' . error_database());
}
$inputs = array();
while ($row = @$stmt->fetch(PDO::FETCH_ASSOC)) {
  $row['files'] = array();
  $inputs[$row['tag']] = $row;
} 
$stmt->closeCursor();

// get list of files
$stmt = $pdo->prepare("SELECT tag, inputs.id, dbfiles.file_name, sites.sitename, inputs.start_date, inputs.end_date" .
                      " FROM sites, inputs, dbfiles, machines, modeltypes_formats, models, formats" .
                      " WHERE (inputs.site_id=${earth} OR inputs.site_id=?)" .
                      " AND inputs.id=dbfiles.container_id AND dbfiles.container_type='Input'" .
                      " AND dbfiles.machine_id=machines.id AND machines.hostname=?" .
                      " AND inputs.format_id=modeltypes_formats.format_id AND inputs.site_id=sites.id" .
                      " AND modeltypes_formats.modeltype_id=models.modeltype_id AND models.id=?" .
                      " AND modeltypes_formats.input AND formats.id=inputs.format_id;");
if (!$stmt->execute(array($siteid, $hostname, $modelid))) {
  die('Invalid query: ' . error_database());
}
while ($row = @$stmt->fetch(PDO::FETCH_ASSOC)) {
  if ($row['tag'] == 'met') {
    $row['name']="Weather " . substr($row['start_date'], 0, 4) . "-" . substr($row['end_date'], 0, 4);
  } else if ($row['file_name'] == '') {
    $row['name']=$row['sitename'];
  } else {
    $row['name']=$row['file_name'];
  }
  $inputs[$row['tag']]['files'][] = $row;
}
$stmt->closeCursor();

// add special inputs based on conversions
if (isset($db_fia_database) && ($db_fia_database != "")) {
  foreach($inputs as &$x) {
    if ($x['tag'] == "pss") {
      $x['files'][] = array("id"=>"fia.pss", "name"=>"Use FIA");
    }
    if ($x['tag'] == "css") {
      $x['files'][] = array("id"=>"fia.css", "name"=>"Use FIA");
    }
    if ($x['tag'] == "site") {
      $x['files'][] = array("id"=>"fia.site", "name"=>"Use FIA");
    }
  }
}

$stmt = $pdo->prepare("SELECT modeltypes.name FROM modeltypes, models" .
                      " WHERE modeltypes.id=models.modeltype_id" .
                      " AND models.id=?;");
if (!$stmt->execute(array($modelid))) {
  die('Invalid query: ' . error_database());
}
$modeltypes=$stmt->fetchAll(PDO::FETCH_COLUMN, 0);
$stmt->closeCursor();
foreach($modeltypes as $type) {
  foreach($inputs as &$x) {
    if ($x['tag'] == "met") {
      if (preg_match("/ \(US-.*\)$/", $siteinfo["sitename"])) {
        $x['files'][] = array("id"=>"Ameriflux." . $type, "name"=>"Use Ameriflux");
      }
      $x['files'][] = array("id"=>"NARR." . $type, "name"=>"Use NARR");
    }
  }
}

// get list of pfts
$pfts = array();
$stmt = $pdo->prepare("SELECT pfts.id, name FROM pfts, models" .
                      " WHERE models.id=? AND pfts.modeltype_id=models.modeltype_id" .
                      " ORDER BY name");
if (!$stmt->execute(array($modelid))) {
  die('Invalid query: ' . error_database());
}
while ($row = @$stmt->fetch(PDO::FETCH_ASSOC)) {
  $row["selected"] = in_array($row['name'], $selected_pfts) ? "selected" : "";
  $pfts[] = $row;
}
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
    $("#next").removeAttr("disabled");       
    $("#error").html("&nbsp;");

    // ensemble 
    if ($("#ensemble").val().length < 1 || $("#ensemble").val() < 1 || !/^[0-9]+$/.test($("#ensemble").val())) {
        $("#next").attr("disabled", "disabled");
        $("#error").html("The ensemble should be a positive integer value.");
    }
  }
      
  function prevStep() {
    $("#formprev").submit();
  }

  function nextStep() {
    $("#formnext").submit();
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
    info+="<?php echo $siteinfo['city']; ?>, <?php echo $siteinfo['state']; ?>, <?php echo $siteinfo['country']; ?><br/>";
    info+="<?php echo $startdate; ?> - <?php echo $enddate; ?>";
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

    <form id="formprev" method="POST" action="03-input.php">
<?php if ($offline) { ?>
      <input name="offline" type="hidden" value="offline">
<?php } ?>
      <input type="hidden" name="siteid" value="<?php echo $siteid; ?>" />
      <input type="hidden" name="modelid" value="<?php echo $modelid; ?>" />
      <input type="hidden" name="hostname" value="<?php echo $hostname; ?>" />
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
      <input type="hidden" name="hostname" value="<?php echo $hostname; ?>" />

      <div class="spacer"></div>
      <label>Ensemble<sup>*</sup></label>
      <input type="text" name="ensemble" id="ensemble" value="<?php echo 1; ?>" onChange="validate();"/>
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
  <div id="footer"><?php echo get_footer(); ?></div>
</div>

<script type="text/javascript" src="js/browndog.js"></script>
<script type="text/javascript">
  $('#browndog').click(function(){
    if($(this).is(':checked')){
      browndog_add();
    } else {
      browndog_del();      
    }
  });
</script>
</body>
</html>

<?php 
close_database();
?>
