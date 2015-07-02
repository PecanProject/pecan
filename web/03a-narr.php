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
<title>PEcAn NARR Data Policy</title>
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
    <h1>NARR Data Policy</h1>
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

    <form id="formnext" method="POST" action="07-analysis.php">
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

<h4>Citation:</h4>
<ul>
<li><b>Please note:</b> If you acquire NCEP Reanalysis data products from PSD, we ask that you acknowledge us in your use of the data. This may be done by including text such as NCEP Reanalysis data provided by the NOAA/OAR/ESRL PSD, Boulder, Colorado, USA, from their Web site at http://www.esrl.noaa.gov/psd/ in any documents or publications using these data. We would also appreciate receiving a copy of the relevant publications. This will help PSD to justify keeping the NCEP Reanalysis data set freely available online in the future. Thank you!</li>
<li><a href="http://www.emc.ncep.noaa.gov/mmb/rreanl/narr.bams.Aug19.pdf">NORTH AMERICAN REGIONAL REANALYSIS:<a/> A long-term, consistent, high-resolution climate dataset for the North American domain, as a major improvement upon the earlier global reanalysis datasets in both resolution and accuracy, Fedor Mesinger et. al, submitted to BAMS 2004.</li>
</ul>

<h4>References:</h4>
<ul>
<li><a href="http://www.emc.ncep.noaa.gov/mmb/rreanl/narr.bams.Aug19.pdf">NORTH AMERICAN REGIONAL REANALYSIS:<a/> A long-term, consistent, high-resolution climate dataset for the North American domain, as a major improvement upon the earlier global reanalysis datasets in both resolution and accuracy, Fedor Mesinger et. al, submitted to BAMS 2004.</li>
</ul>

<h4>Original Source:</h4>
<ul>
<li>Dataset was originally produced at NOAA's National Center for Atmospheric Prediction (NCEP) and is available in it's original grib format through <a href="http://nomads.ncdc.noaa.gov/#narr_datasets">NOAA NCDC's access page</a>. It is also available in the same format from <a href="http://rda.ucar.edu/datasets/ds608.0/">NCAR</a>.</li>
</ul>

<h4>Contact:</h4>
Physical Sciences Division: Data Management<br>
NOAA/ESRL/PSD<br>
325 Broadway<br>
Boulder, CO 80305-3328<br>
<a href="mailto:esrl.psd.data@noaa.gov">esrl.psd.data@noaa.gov</a><br>

<br>
<p>For more information see <a href="http://www.esrl.noaa.gov/psd/data/gridded/data.narr.html" target="_blank">http://www.esrl.noaa.gov/psd/data/gridded/data.narr.html</a>.</p>
  </div>
  <div id="footer"><?php echo get_footer(); ?></div>
</div>
</body>
</html>

<?php 
close_database();
?>
