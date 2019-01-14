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

// #  parameters
// if (!isset($_REQUEST['hostname'])) {
//   die("Need a hostname.");
// }
// $hostname=$_REQUEST['hostname'];
// $adv_setup = (isset($_REQUEST['adv_setup'])) ? "checked" : "";
// $fluxusername = (isset($_REQUEST['fluxusername'])) ? $_REQUEST['fluxusername'] : "";

?>
<!DOCTYPE html>
<html>
<head>
<title>BrownDog Policy</title>
<link rel="shortcut icon" type="image/x-icon" href="favicon.ico" />
<meta name="viewport" content="width=device-width, initial-scale=1.0, user-scalable=no" />
<meta http-equiv="content-type" content="text/html; charset=UTF-8" />
<link rel="stylesheet" type="text/css" href="sites.css" />
<script type="text/javascript" src="jquery-1.10.2.min.js"></script>
<script type="text/javascript">
  function validate() {
    $("#next").removeAttr("disabled");       
    $("#error").html("&nbsp;");

    if ($("#browndog_key").val() == "") {
      $("#next").attr("disabled", "disabled");
      $("#error").html("Need to provide an api key to use BrownDog");
    }
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
    <h1>BrownDog Policy</h1>
    <p>To continue you need to agree to the data policy on the right.</p>

    <form id="formprev" method="POST" action="03-inputs.php">
<?php
  foreach($_REQUEST as $key => $value) {
    if (is_array($value)) {
      foreach($value as $v) {
        echo "<input name=\"${key}[]\" id=\"${key}[]\" type=\"hidden\" value=\"${v}\"/>";
      }
    } else {
      if(strcmp($key, "notes") == 0 ) {
        $str = htmlentities($value, ENT_QUOTES);
        echo "<input name=\"${key}\" id=\"${key}\" type=\"hidden\" value=\"${str}\"/>";
      } else {
        echo "<input name=\"${key}\" id=\"${key}\" type=\"hidden\" value=\"${value}\"/>";
      }
    }
  }
?>
    </form>

<?php if ($adv_setup == "checked"){?> 
	<form id="formnext" method="POST" action="07-analysis.php">
<?php } else { ?>
  <form id="formnext" method="POST" action="<?php echo ($hostname != $fqdn ? '04-remote.php' : '04-runpecan.php'); ?>">
<?php }?>

<?php
  foreach($_REQUEST as $key => $value) {
    if (is_array($value)) {
      foreach($value as $v) {
        echo "<input name=\"${key}[]\" id=\"${key}[]\" type=\"hidden\" value=\"${v}\"/>";
      }
    } else {
      if (strcmp($key, "notes") == 0 ) {
        $str = htmlentities($value, ENT_QUOTES);
        echo "<input name=\"${key}\" id=\"${key}\" type=\"hidden\" value=\"${str}\"/>";
      } else if (strcmp($key, "fluxusername") != 0 ) {
        echo "<input name=\"${key}\" id=\"${key}\" type=\"hidden\" value=\"${value}\"/>";
      }
    }
  }
?>
      <label title="BrownDog api key">BrownDog key</label>
      <input id="browndog_key" name="browndog_key" type="text" value="<?php echo $browndog_key; ?>" onkeyup="validate()"/>
      <div class="spacer"></div>

      <span id="error" class="small">&nbsp;</span>
      <input id="prev" type="button" value="Prev" onclick="prevStep();" />
      <input id="next" type="button" value="Agree" onclick="nextStep();" />    
      <div class="spacer"></div>
    </form>
<?php whoami(); ?>    
  </div>
  <div id="output">
<h1>BrownDog Policy</h1>

<br>
<p>For more information see <a href="https://browndog.ncsa.illinois.edu/" target="_blank">https://browndog.ncsa.illinois.edu/</a>.</p>

  </div>
  <div id="footer"><?php echo get_footer(); ?></div>
</div>
</body>
</html>

<?php 
close_database();
?>
