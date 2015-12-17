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
$browndog = (isset($_REQUEST['browndog'])) ? "checked" : "";

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

$startdate = "2004/01/01";
if (isset($_REQUEST['start'])) {
  $startdate=$_REQUEST['start'];
}

$enddate = "2004/12/31";
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
$stmt = $pdo->prepare("SELECT tag, inputs.name AS input_name, inputs.id, dbfiles.file_name, sites.sitename, inputs.start_date, inputs.end_date" .
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
	if (empty($row['input_name'])){
    		$row['name']="Weather " . substr($row['start_date'], 0, 4) . "-" . substr($row['end_date'], 0, 4);
	} else {
    		$row['name']= $row['input_name'];
	}
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
      // check for NARR,this is not exact since it is a conical projection
      if ($siteinfo['lat'] > 1 && $siteinfo['lat'] < 46 && $siteinfo['lon'] < -68 && $siteinfo['lon'] > -145) {
        $x['files'][] = array("id"=>"NARR." . $type, "name"=>"Use NARR");
      }
      // CRUNCEP is global
      $x['files'][] = array("id"=>"CRUNCEP." . $type, "name"=>"Use CRUNCEP");
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

    // check PFTs
    if ($("#pft").val() == null) {
      $("#next").attr("disabled", "disabled");
      $("#error").html("Select a pft to continue");
      $("#pftlabel").html("PFT<sup>*</sup>");
<?php if ($betydb != "") { ?>
    } else {
      $("#pftlabel").html("PFT<sup>*</sup> (Show in <a href=\"<?php echo $betydb; ?>/pfts/" + $("#pft option:selected")[0].getAttribute("data-id") + "\" target=\"BETY\">BETY</a>)");
<?php } ?>
    }

    // check inputs
<?php
  foreach($inputs as $input) {
    if ($input['required']) {
?>
    if ($("#<?php echo $input['tag']; ?>").val() == null) {
      $("#next").attr("disabled", "disabled");
      $("#error").html("Missing value for <?php echo $input['name']; ?>");
<?php if ($betydb != "") { ?>
    } else {
      var metlabeldiv = document.getElementById('metlabeldiv');
      metlabeldiv.innerHTML="";
	if (!(isNaN($("#met option:selected")[0].getAttribute("value")))) {
      		metlabeldiv.innerHTML = " (Show in <a href=\"<?php echo $betydb; ?>/inputs/" + $("#met option:selected")[0].getAttribute("value") + "\" target=\"BETY\">BETY</a>)";
	}
<?php } ?>
    }
<?php
    }
  }
?>

    // check dates
    if ($("#start").length != 0) {
      var start = checkDate($("#start").val(), "Start");
      var end = checkDate($("#end").val(), "End");
      if (start >= end) {
        $("#next").attr("disabled", "disabled");
        $("#error").html("End date should be after start date.");
      }
    }

    // redirect to data policy if needed
    if ($("#met").val()) {
      if ($("#met").val().startsWith("Ameriflux")) {
        $("#formnext").attr("action", "03a-ameriflux.php");
      } else if ($("#met").val().startsWith("NARR")) {
        $("#formnext").attr("action", "03a-narr.php");
      } else if ($("#adv_setup").is(':checked')){
        $("#formnext").attr("action", "07-analysis.php");
      } else if (!$("#adv_setup").is(':checked')) {
        $("#formnext").attr("action", "<?php echo ($hostname != $fqdn ? '04-remote.php' : '04-runpecan.php'); ?>");
      }
    }
  }

  function prevStep() {
    $("#formprev").submit();
  }

  function nextStep() {
    $("#formnext").submit();
  }

  function checkDate(date, field) {
    var arr = date.match(/^(\d{4})\/(\d{1,2})\/(\d{1,2})$/);
    if (arr == null) {
      $("#next").attr("disabled", "disabled");
      $("#error").html(field + " date should be entered as \"YYYY/MM/DD\"");
      return "";
    }

    arr[1] = parseInt(arr[1], 10);
    arr[2] = parseInt(arr[2], 10)-1;
    arr[3] = parseInt(arr[3], 10);
    var test = new Date(arr[1], arr[2], arr[3]);

    if (arr[1] != test.getFullYear() || arr[2] != test.getMonth() || arr[3] != test.getDate()) {
      $("#next").attr("disabled", "disabled");
      $("#error").html(field + "  date is not a valid date.");
      return "";
    }

    return test;
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

    <form id="formprev" method="POST" action="02-modelsite.php">
<?php if ($offline) { ?>
      <input name="offline" type="hidden" value="offline">
<?php } ?>
<?php if (isset($_REQUEST['conversion'])) { ?>
      <input name="conversion" type="hidden" value="on">
<?php } ?>
      <input type="hidden" name="siteid" value="<?php echo $siteid; ?>" />
      <input type="hidden" name="modelid" value="<?php echo $modelid; ?>" />
      <input type="hidden" name="hostname" value="<?php echo $hostname; ?>" />
    </form>

    <form id="formnext" method="POST" action="<?php echo ($hostname != $fqdn ? '04-remote.php' : '04-runpecan.php'); ?>">
<?php if ($offline) { ?>
      <input name="offline" type="hidden" value="on">
<?php } ?>
<?php if ($userok) { ?>
      <input name="userok" type="hidden" value="on">
<?php } ?>
      <input type="hidden" name="siteid" value="<?php echo $siteid; ?>" />
      <input type="hidden" name="modelid" value="<?php echo $modelid; ?>" />
      <input type="hidden" name="hostname" value="<?php echo $hostname; ?>" />
      <label id="pftlabel">PFT<sup>*</sup></label>
      <select id="pft" name="pft[]" multiple size=5 onChange="validate();">
<?php
foreach($pfts as $pft) {
  print "        <option data-id='{$pft['id']}' ${pft['selected']} title=\"${pft['name']}\">${pft['name']}</option>\n";
}
?>
      </select>
      <div class="spacer"></div>
      <label>Start Date<sup>*</sup></label>
      <input type="text" name="start" id="start" value="<?php echo $startdate; ?>" onChange="validate();"/>
      <div class="spacer"></div>
      <label>End Date<sup>*</sup></label>
      <input type="text" name="end" id="end" value="<?php echo $enddate; ?>" onChange="validate();"/>
      <div class="spacer"></div>
<?php

# show list of all inputs
foreach($inputs as $input) {
  $name=substr($input['name'], 0, 20);
  $tag=$input['tag'];
  if ($input['required']) {
    print "      <label id=\"metlabel\">${name}*<div id=\"metlabeldiv\"></div></label>\n";
  } else { 
    print "      <label id=\"metlabel\">${name}<div id=\"metlabeldiv\"></div></label>\n";
  }
  print "      <select id=\"${tag}\" name=\"input_${tag}\" onChange=\"validate();\">\n";
  if (!$input['required']) {
    print "      <option value='-1'></option>\n";
  }
  foreach($input['files'] as $file) {
    print "        <option value='${file['id']}'";
    if (isset($_REQUEST["input_${tag}"]) && $_REQUEST["input_${tag}"] == "${file['id']}") {
      print " selected";
    }
    print ">${file['name']}</option>\n";
  }
  print "      </select>\n";
  print "      <div class=\"spacer\"></div>\n";
}
?>
      <label title="Used to send email when the run is finished.">Email</label>
      <input id="email" name="email" type="text" value="<?php echo $email; ?>"/>
      <div class="spacer"></div>

<?php if (isset($browndog_url) && $browndog_url != "") { ?>
      <label title="Use BrownDog for conversions.">Use <a href="http://browndog.ncsa.illinois.edu/">BrownDog</a></label>
      <input id="browndog" name="browndog" type="checkbox" <?php echo $browndog; ?>/>
<?php } ?>
      <label title="Allows to pecan.xml file before workflow.">Edit pecan.xml</label>
      <input id="pecan_edit" name="pecan_edit" type="checkbox" <?php echo $pecan_edit; ?>/>
      <label title="Allows to edit files generated by PEcAn before model executions.">Edit model config</label>
      <input id="model_edit" name="model_edit" type="checkbox" <?php echo $model_edit; ?>/>
      <label title="Allows user to do advanced analysis (quantiles and sigma options) ">Advanced setup</label>
      <input id="adv_setup" name="adv_setup" type="checkbox" <?php echo $adv_setup; ?> onChange=validate(); />
      <div class="spacer"></div>

      <span class="small"><sup>*</sup> are required fields.</span>
      <p></p>
      <span id="error" class="small">&nbsp;</span>
      <input id="prev" type="button" value="Prev" onclick="prevStep();" />
      <input id="next" type="button" value="Next" onclick="nextStep();" <?php if (!$userok) echo "disabled" ?>/>
      <div class="spacer"></div>
    </form>
<?php whoami(); ?>    
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
