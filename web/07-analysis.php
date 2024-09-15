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
$userok=isset($_REQUEST['userok']);
$offline=isset($_REQUEST['offline']);
$pecan_edit = (isset($_REQUEST['pecan_edit'])) ? "checked" : "";
$adv_setup = (isset($_REQUEST['adv_setup'])) ? "checked" : "";
$model_edit = (isset($_REQUEST['model_edit'])) ? "checked" : "";
$ensemble_analysis = (isset($_REQUEST['ensemble_analsysis'])) ? "checked" : "";
$sensitivity_analysis = (isset($_REQUEST['sensitivity'])) ? "checked" : "";

if (!isset($_REQUEST['siteid'])) {
  die("Need a siteid.");
}
$siteid=$_REQUEST['siteid'];

if (!isset($_REQUEST['modelid'])) {
  die("Need a modelid.");
}
$modelid=$_REQUEST['modelid'];

// hostname
if (!isset($_REQUEST['hostname'])) {
  die("Need a hostname.");
}
$hostname=$_REQUEST['hostname'];
if (!array_key_exists($hostname, $hostlist)) {
  die("{$hostname} is not an approved host");
}
$hostoptions = $hostlist[$hostname];

# parse original form data
if (!isset($_REQUEST['pft'])) {
  die("Need a pft.");
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

$met= "";
if (isset($_REQUEST['input_met'])) { 
  $met=$_REQUEST['input_met'];
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

    // ensemble 
    if (($("#runs").val().length < 1 || $("#runs").val() < 1 || !/^[0-9]+$/.test($("#runs").val()))) {
        $("#next").attr("disabled", "disabled");
        $("#error").html("The ensemble should be a positive integer value.");
    }
    //make sure variable field is populated if ensemble analysis is checked
    if ($("#variables").val().length < 1) {
        $("#next").attr("disabled", "disabled");
        $("#error").html("The need to set a varaible value.");
    }
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
    <h1>Selected Site</h1>
    <p>Set parameters for the run.</p>

    <form id="formprev" method="POST" action="03-inputs.php">
<?php if ($offline) { ?>
      <input name="offline" type="hidden" value="offline">
<?php } ?>
<?php foreach($_REQUEST as $key => $value){
        if(is_array($value)) {
          foreach($value as $v) {
            echo "<input name=\"{$key}[]\" id=\"{$key}[]\" type=\"hidden\" value=\"{$v}\"/>";
          }   
        } else {
          if(strcmp($key, "notes") == 0) {
            $str = htmlentities($value, ENT_QUOTES);
            echo "<input name=\"{$key}\" id=\"{$key}\" type=\"hidden\" value=\"{$str}\"/>";
          } else {
            echo "<input name=\"{$key}\" id=\"{$key}\" type=\"hidden\" value=\"{$value}\"/>";
          }
        }
      }
?>
</form>

<form id="formnext" method="POST" action="<?php echo ($hostname != $fqdn ? '04-remote.php' : '04-runpecan.php'); ?>">
<?php if ($offline) { ?>
<input name="offline" type="hidden" value="on">
<?php } ?>
<?php if ($userok) { ?>
<input name="userok" type="hidden" value="on">
<?php } ?>
<?php foreach($_REQUEST as $key => $value){
	if(is_array($value)) {
	  foreach($value as $v) {
	    echo "<input name=\"{$key}[]\" id=\"{$key}[]\" type=\"hidden\" value=\"{$v}\"/>";
	  }
	} else {
          if(strcmp($key, "notes") == 0) {
            $str = htmlentities($value, ENT_QUOTES);
            echo "<input name=\"{$key}\" id=\"{$key}\" type=\"hidden\" value=\"{$str}\"/>";
          } else {
            echo "<input name=\"{$key}\" id=\"{$key}\" type=\"hidden\" value=\"{$value}\"/>";
          }
	}
      }
?>
      <div class="spacer"></div>
      <span title="Number of runs in the Ensemble Analysis">
      <label>Runs<sup>*</sup></label></span>
      <input type="text" name="runs" id="runs" value="<?php echo 1; ?>" onChange="validate();"/>
      <div class="spacer"></div>
      <a href="https://pecanproject.github.io/pecan-documentation/latest/model-output-variables.html" title="Model output variables to run analyses on. Link opens variable name table">
      <label>Variables<sup>*</sup></label></a>
      <input type="text" name="variables" id="variables" value="<?php echo "NPP"; ?>" onChange="validate();"/>
      <div class="spacer"></div>
      <label>Sampling method for parameters<sup>*</sup></label>
      <select name="parm_method" id="parm_method" onChange="validate();">
      <option value="<?php echo "uniform"; ?>">Uniform</option>
      <option value="<?php echo "halton"; ?>">Halton</option>
      <option value="<?php echo "sobol"; ?>">Sobol</option>
      <option value="<?php echo "torus"; ?>">Torus</option>
      <option value="<?php echo "lhc"; ?>">Latin hypercube</option>
     </select>
      <div class="spacer"></div>

      <div class="spacer"></div>
      <label type="hidden">Sensitivity</label>
      <input type="text" name="sensitivity" id="sensitivity" value="<?php echo "" ?>" onChange="validate();"/>
      <div class="spacer"></div>
      <label>Sets sigma (std dev) equivalent quantiles.</label>
      <label>Example, "-1,1".</label>
      <div class="spacer"></div>

      <p></p>
      <span id="error" class="small">&nbsp;</span>
      <input id="prev" type="button" value="Prev" onclick="prevStep();" />
      <input id="next" type="button" value="Next" onclick="nextStep();" <?php if (!$userok) echo "disabled" ?>/>    
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
