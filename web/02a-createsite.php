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

# parameters
if (!isset($_REQUEST['lat'])) {
  die("Need latitude.");
}
$lat=$_REQUEST['lat'];
if (!isset($_REQUEST['lon'])) {
  die("Need longitude.");
}
$lon=$_REQUEST['lon'];

?>
<!DOCTYPE html>
<html>
<head>
<title>PEcAn Create Site</title>
<link rel="shortcut icon" type="image/x-icon" href="favicon.ico" />
<meta name="viewport" content="width=device-width, initial-scale=1.0, user-scalable=no" />
<meta http-equiv="content-type" content="text/html; charset=UTF-8" />
<link rel="stylesheet" type="text/css" href="sites.css?id=<?php echo rand(); ?>" />
<script type="text/javascript" src="jquery-1.10.2.min.js"></script>
<script type="text/javascript">
  function validate() {
    var dialog, form,
    sitename = $( "#txtsitename" ),
    elevation =$( "#txtelevation" ),
    map =$( "#txtmap" ),
    mat =$( "#txtmat" ),
    city =$( "#txtcity" ),
    state =$( "#txtstate" ),
    county =$( "#txtcountry" ),
    lat =$( "#txtlat" ),
    lng =$( "#txtlong" ),
    pctclay =$( "#txtpctclay" ),
    pctsand =$( "#txtpctsand" ),
    greehouse =$( "#txtgreenhouse" ),
    notes =$( "#txtnotes" ),
    soilnotes =$( "#txtsoilnotes" ),
    allFields = $( [] ).add( sitename ).add( elevation ),
    tips = $( ".validateTips" ),
    request;
  }
      
  function prevStep() {
    $("#formprev").submit();
  }

  function nextStep() {
    $("#formnext").submit();
  }
  
  $(document).ready(function () {
    validate();
  });
</script>
</head>
<body>
<div id="wrap">
  <div id="stylized">
    <h1>Create new site</h1>
    <p>All fields in the form are required.</p>

    <form id="formprev" method="POST" action="02-modelsite.php">
<?php
  foreach($_REQUEST as $key => $value) {
    if (is_array($value)) {
      foreach($value as $v) {
        echo "<input name=\"${key}[]\" id=\"${key}[]\" type=\"hidden\" value=\"${v}\"/>";
      }
    } else {
      if(strcmp($key, "notes") == 0) {
        $str = htmlentities($value, ENT_QUOTES);
        echo "<input name=\"${key}\" id=\"${key}\" type=\"hidden\" value=\"${str}\"/>";
      } else {
        echo "<input name=\"${key}\" id=\"${key}\" type=\"hidden\" value=\"${value}\"/>";
      }
    }
  }
?>
    </form>

    <form id="formnext" method="POST" action="02-modelsite.php">
<?php
  foreach($_REQUEST as $key => $value) {
    if (is_array($value)) {
      foreach($value as $v) {
        echo "<input name=\"${key}[]\" id=\"${key}[]\" type=\"hidden\" value=\"${v}\"/>";
      }
    } else {
      if(strcmp($key, "notes") == 0) {
        $str = htmlentities($value, ENT_QUOTES);
        echo "<input name=\"${key}\" id=\"${key}\" type=\"hidden\" value=\"${str}\"/>";
      } else {
        echo "<input name=\"${key}\" id=\"${key}\" type=\"hidden\" value=\"${value}\"/>";
      }
    }
  }
?>
      <span id="error" class="small">&nbsp;</span>
      <input id="prev" type="button" value="Prev" onclick="prevStep();" />
      <input id="next" type="button" value="Add Site" onclick="nextStep();" />    
      <div class="spacer"></div>
    </form>
<?php left_footer(); ?>    
  </div>
  <div id="output">
    <form id="site">
      <fieldset>
        <legend>Site location</legend>

        <label>Site name:</label>
        <input id="sitename" size="30" type="text" name="sitename"></input>
        <div class="spacer"></div>

        <label>City:</label>
        <input id="city" size="30" type="text" name="city"></input>
        <div class="spacer"></div>

        <label>State:</label>
        <input id="state" size="30" type="text" name="state"></input>
        <div class="spacer"></div>

        <label>Country:</label>
        <input id="country" size="30" type="text" name="country"></input>
        <div class="spacer"></div>

        <label>Latitude:</label>
        <input id="lat" size="30" type="text" name="lat" value="<?php echo $lat; ?>"></input>
        <div class="spacer"></div>

        <label>Longitude:</label>
        <input id="lon" size="30" type="text" name="lon" value="<?php echo $lon; ?>"></input>
        <div class="spacer"></div>

        <label>Elevation (m):</label>
        <input id="elevation" size="30" type="text" name="elevation"></input>
        <div class="spacer"></div>
      </fieldset>

      <fieldset>
        <legend>Site Meteorological Data</legend>

        <label>Mean Annual Precipitation (mm/yr):</label>
        <input id="elevation" size="30" type="text" name="elevation"></input>
        <div class="spacer"></div>

        <label>Mean Annual Temperature (C):</label>
        <input id="elevation" size="30" type="text" name="elevation"></input>
        <div class="spacer"></div>
      </fieldset>

      <fieldset>
        <legend>Site Soil Information</legend>

        <label>% Clay:</label>
        <input id="elevation" size="30" type="text" name="elevation"></input>
        <div class="spacer"></div>

        <label>% Sand:</label>
        <input id="elevation" size="30" type="text" name="elevation"></input>
        <div class="spacer"></div>

        <label>Soil Notes:</label>
        <input id="elevation" size="30" type="text" name="elevation"></input>
        <div class="spacer"></div>

        <label>Soil Notes:</label>
        <textarea id="soilnotes" cols="40" rows="5" type="text" name="soilnotes"></textarea>
        <div class="spacer"></div>
      </fieldset>

      <fieldset>
        <legend>Site Notes</legend>

        <label>Site Notes:</label>
        <textarea id="notes" cols="40" rows="5" type="text" name="notes"></textarea>
        <div class="spacer"></div>
      </fieldset>
    </form>
  </div>
  <div id="footer"><?php echo get_footer(); ?></div>
</div>
</body>
</html>

<?php 
close_database();
?>
