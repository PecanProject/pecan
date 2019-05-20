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
$adv_setup = (isset($_REQUEST['adv_setup'])) ? "checked" : "";

# parameters needed
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

if (!isset($_REQUEST['pft'])) {
  die("Need a hostname.");
}
$selected_pfts = $_REQUEST['pft'];

if (!isset($_REQUEST['start'])) { 
  die("Need a start date.");
}
$startdate=$_REQUEST['start'];

if (!isset($_REQUEST['end'])) { 
  die("Need a end date.");
}
$enddate=$_REQUEST['end'];

$username=isset($_REQUEST['username']) ? $_REQUEST['username'] : "";
$password=isset($_REQUEST['password']) ? $_REQUEST['password'] : "";
$tunnel=isset($_REQUEST['tunnel']) ? $_REQUEST['tunnel'] : "";

// get site information
$stmt = $pdo->prepare("SELECT sitename, city, state, country, ST_X(ST_CENTROID(sites.geometry)) AS lon, ST_Y(ST_CENTROID(sites.geometry)) AS lat FROM sites WHERE sites.id=?");
if (!$stmt->execute(array($siteid))) {
  die('Invalid query: ' . error_database());
}
$siteinfo = $stmt->fetch(PDO::FETCH_ASSOC);
$stmt->closeCursor();

// get model information
$stmt = $pdo->prepare("SELECT * FROM models WHERE id=?");
if (!$stmt->execute(array($modelid))) {
  die('Invalid query: ' . error_database());
}
$modelinfo = $stmt->fetch(PDO::FETCH_ASSOC);
$stmt->closeCursor();

if (isset($modelinfo['revision'])) {
  $modelname = $modelinfo['model_name'] . " (" . $modelinfo['revision'] . ")";
} else {
  $modelname = $modelinfo['model_name'];
}

?>
<!DOCTYPE html>
<html>
<head>
<title>PEcAn Parameter Selection</title>
<link rel="shortcut icon" type="image/x-icon" href="favicon.ico" />
<meta name="viewport" content="width=device-width, initial-scale=1.0, user-scalable=no" />
<meta http-equiv="content-type" content="text/html; charset=UTF-8" />
<link rel="stylesheet" type="text/css" href="sites.css" />
<script type="text/javascript" src="jquery-1.10.2.min.js"></script>
<?php if (!$offline) {?>
<link rel="stylesheet" href="leaflet/leaflet.css" />
<script src="leaflet/leaflet.js"></script>
<?php }?>
<script type="text/javascript">
  function validate() {
    $("#next").removeAttr("disabled");       
    $("#error").html("&nbsp;");
  }
      
  function prevStep() {
    $("#formprev").submit();
  }

  function nextStep() {
    $("#formnext").submit();
  }

  $(document).ready(function () {
    // create the tooltip and its text
    var info="<b><?php echo $siteinfo['sitename']; ?></b><br />";
    info+="<?php echo $siteinfo['city']; ?>, <?php echo $siteinfo['state']; ?>, <?php echo $siteinfo['country']; ?><br/>";
    info+="PFTs: <?php echo implode(",",$selected_pfts);?><br/>";
    info+="Dates: <?php echo $startdate; ?> - <?php echo $enddate; ?><br/>";
    info+="Model: <?php echo $modelname; ?>"

<?php if (!$offline) { ?>
    var tiles = L.tileLayer('https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png', {
        maxZoom: 18,
        attribution: '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors, Points &copy 2012 LINZ'
      }),
      latlng = L.latLng(<?php echo $siteinfo['lat']; ?>, <?php echo $siteinfo['lon']; ?>);

    map = L.map('output', {center: latlng, zoom: 10, layers: [tiles]});

    var marker = L.marker([<?php echo $siteinfo['lat']; ?>, <?php echo $siteinfo['lon']; ?>]);
    marker.title = "<?php echo $siteinfo['sitename']; ?>";
    marker.bindPopup(info);
    marker.addTo(map);
    marker.openPopup();
<?php } else { ?>
    $("#output").html(info);
<?php } ?>
    validate();
  });

</script>
</head>
<body>
<div id="wrap">
  <div id="stylized">
    <h1>Host Setup</h1>
    <p>Set host parameters for the run.</p>

    <form id="formprev" method="POST" action="<?php echo ($adv_setup ? "07-analysis.php" : "03-inputs.php"); ?>">
<?php
  foreach($_REQUEST as $key => $value) {
    if (is_array($value)) {
      foreach($value as $v) {
        echo "<input name=\"${key}[]\" id=\"${key}[]\" type=\"hidden\" value=\"${v}\"/>";
      }
    } else {
      echo "<input name=\"${key}\" id=\"${key}\" type=\"hidden\" value=\"${value}\"/>";
    }
  }
?>
    </form>

    <form id="formnext" method="POST" action="04-runpecan.php">
<?php
  foreach($_REQUEST as $key => $value) {
    if (is_array($value)) {
      foreach($value as $v) {
        echo "<input name=\"${key}[]\" id=\"${key}[]\" type=\"hidden\" value=\"${v}\"/>";
      }
    } else {
      echo "<input name=\"${key}\" id=\"${key}\" type=\"hidden\" value=\"${value}\"/>";
    }
  }
?>
      <label>Connection information:</label>

<!--
      <input id="login" name="login" type="radio" value="username" checked="on">
-->
      <label>Username:</label>
      <input id="username" name="username" type="text" value="<?php echo $username; ?>"/>
      <label>Password:</label>
      <input id="password" name="password" type="password" value="<?php echo $password; ?>"/>

<!--
      <input id="login" name="login" type="radio" value="tunnel">
      <label>Tunnel:</label>
      <input id="tunnel" name="tunnel" type="text" value="<?php echo $tunnel; ?>"/>
-->

<!--
      <input id="login" name="login" type="radio" value="key">Private Key<br>
      <input id="key" name="key" type="text" value="<?php echo $key; ?>"/>
-->

      <p></p>
      <span id="error" class="small">&nbsp;</span>
      <input id="prev" type="button" value="Prev" onclick="prevStep();" />
      <input id="next" type="button" value="Next" onclick="nextStep();" />    
      <div class="spacer"></div>
    </form>
    <?php left_footer(); ?>    
  </div>
  <div id="output">
  </div>
  <div id="footer"><?php echo get_footer(); ?></div>
</div>
</body>
</html>

<?php 
close_database();
?>
