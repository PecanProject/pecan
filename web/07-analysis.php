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
$adv_setup = (isset($_REQUEST['adv_setup'])) ? "checked" : "";
$model_edit = (isset($_REQUEST['model_edit'])) ? "checked" : "";
$browndog = (isset($_REQUEST['browndog'])) ? "checked" : "";
$ensemble_analysis = (isset($_REQUEST['ensemble_analsysis'])) ? "checked" : "";
$sensitivity_analysis = (isset($_REQUEST['sensitivity'])) ? "checked" : "";
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
    // check if ensemble analysis or sensitivity analysis is set
    if (!($("#ensemble_analysis").is(":checked")) && !($("#sensitivity_analysis").is(":checked"))) {
        $("#next").attr("disabled", "disabled");
        $("#error").html("The ensemble and/or sensitivity analysis features should be checked.");
    }
    // ensemble 
    if ($("#ensemble_analysis").is(":checked") && ($("#runs").val().length < 1 || $("#runs").val() < 1 || !/^[0-9]+$/.test($("#runs").val()))) {
        $("#next").attr("disabled", "disabled");
        $("#error").html("The ensemble should be a positive integer value.");
    }
    //make sure variable field is populated if ensemble analysis is checked
    if (($("#ensemble_analysis").is(":checked")) && $("#variables").val().length < 1) {
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
    info+="PFTs: <?php echo implode(",",$selected_pfts);?><br/>";
    info+="Dates: <?php echo $startdate; ?> - <?php echo $enddate; ?>";
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
<?php foreach($_REQUEST as $key => $value){
	if(is_array($value)) {
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
<?php if ($offline) { ?>
      <input name="offline" type="hidden" value="on">
<?php } ?>
<?php if ($userok) { ?>
      <input name="userok" type="hidden" value="on">
<?php } ?>
<?php foreach($_REQUEST as $key => $value){
	if(is_array($value)) {
	  foreach($value as $v) {
	    echo "<input name=\"${key}[]\" id=\"${key}[]\" type=\"hidden\" value=\"${v}\"/>";
	  }
	} else {
	    echo "<input name=\"${key}\" id=\"${key}\" type=\"hidden\" value=\"${value}\"/>";
	}
      }
?>

      <div class="spacer"></div>
      <label title="Enable number of run for analysis">Ensemble analysis</label>
      <input id="ensemble_analysis" name="ensemble_analysis" type="checkbox" value="<?php echo $ensemble_analysis ?>" checked onChange="validate();"/>
      <div class="spacer"></div>
      <label>Runs<sup>*</sup></label>
      <input type="text" name="runs" id="runs" value="<?php echo 1; ?>" onChange="validate();"/>
      <div class="spacer"></div>
      <label>Variables<sup>*</sup></label>
      <input type="text" name="variables" id="variables" value="<?php echo "NPP"; ?>" onChange="validate();"/>
      <div class="spacer"></div>
      <label>Notes</label>
      <!--<input type="text" name="notes" id="notes" value="" />-->
      <textarea name="notes" id="notes" rows="4" width="184px" style="padding:4px 2px;width:184px;font-size:12px;border: solid 1px #aacfe4;text-overflow:ellipsis;"></textarea>
      <div class="spacer"></div>

      <div class="spacer"></div>
      <label title="Enable sensitivity for analysis">Enable sensitivity</label>
      <input id="sensitivity_analysis" name="sensitivity_analysis" type="checkbox" value="<?php echo $sensitivity_analysis ?>"  onChange="validate();" />
      <div class="spacer"></div>
      <label type="hidden">Sensitivity</label>
      <input type="text" name="sensitivity" id="sensitivity" value="<?php echo "" ?>" onChange="validate();"/>
      <div class="spacer"></div>
      <label>Sets sigma and quantiles.</label>
      <label>Example, "-1,0,1".</label>
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
