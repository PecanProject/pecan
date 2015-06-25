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
}

# boolean parameters
$userok=isset($_REQUEST['userok']);
$offline=isset($_REQUEST['offline']);
$pecan_edit = (isset($_REQUEST['pecan_edit'])) ? "checked" : "";
$model_edit = (isset($_REQUEST['model_edit'])) ? "checked" : "";
$browndog = (isset($_REQUEST['browndog'])) ? "checked" : "";
?>
<!DOCTYPE html>
<html>
<head>
<title>PEcAn AmeriFlux Data Policy</title>
<meta name="viewport" content="width=device-width, initial-scale=1.0, user-scalable=no" />
<meta http-equiv="content-type" content="text/html; charset=UTF-8" />
<link rel="stylesheet" type="text/css" href="sites.css" />
<script type="text/javascript" src="jquery-1.7.2.min.js"></script>
<script type="text/javascript">
  function validate() {
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
    <h1>AmeriFlux Data Policy</h1>
    <p>To continue you need to agree to the data policy on the right.</p>

    <form id="formprev" method="POST" action="03-inputs.php">
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
      <span id="error" class="small">&nbsp;</span>
      <input id="prev" type="button" value="Prev" onclick="prevStep();" />
      <input id="next" type="button" value="Agree" onclick="nextStep();" />    
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
<h1>Data Policy</h1>

<br>
<p>The AmeriFlux Network data offered on this website are contributed by individual AmeriFlux scientists, who share their data openly with the global community.</p>

<h2>AmeriFlux Data Use Policy</h2>

<p>The AmeriFlux policy is that all data should be properly acknowledged, and that data contributors should have the opportunity to make an intellectual contribution to the papers that use their data and as a result have the opportunity to be a co-author.</p>

<br/>

<p>When you use AmeriFlux data:</p>

<ul>
<li>Inform the AmeriFlux scientist(s) who contributed the data how you plan to use the data and of any publication plans.</li>

<li>Initiate contact with the data contributor, so that they have the opportunity to contribute substantively and as a result to be a co-author.</li>

<li>Acknowledge AmeriFlux data by citing the relevant DOI or paper(s), and/or acknowledging funding for the site support. If the data download was not accompanied by the preferred acknowledgment language, ask the site principal investigator.</li>

<li>Acknowledge the AmeriFlux data resource as "funding for AmeriFlux data resources was provided by the U.S. Department of Energy’s Office of Science."</li>
</ul>

<br>
<p>For more information see <a href="http://ameriflux.lbl.gov/data/data-policy/" target="_blank">http://ameriflux.lbl.gov/data/data-policy/</a>.</p>

  </div>
  <div id="footer"><?php echo get_footer(); ?></div>
</div>
</body>
</html>

<?php 
close_database();
?>
