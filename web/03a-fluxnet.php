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

#  parameters
if (!isset($_REQUEST['hostname'])) {
  die("Need a hostname.");
}
$hostname=$_REQUEST['hostname'];
$adv_setup = (isset($_REQUEST['adv_setup'])) ? "checked" : "";

?>
<!DOCTYPE html>
<html>
<head>
<title>PEcAn Fluxnet Data Policy</title>
<link rel="shortcut icon" type="image/x-icon" href="favicon.ico" />
<meta name="viewport" content="width=device-width, initial-scale=1.0, user-scalable=no" />
<meta http-equiv="content-type" content="text/html; charset=UTF-8" />
<link rel="stylesheet" type="text/css" href="sites.css" />
<script type="text/javascript" src="jquery-1.10.2.min.js"></script>
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
    <h1>Fluxnet Data Policy</h1>
    <p>To continue you need to agree to the data policy on the right.</p>

    <form id="formprev" method="POST" action="03-inputs.php">
<?php
  foreach($_REQUEST as $key => $value) {
    if (is_array($value)) {
      foreach($value as $v) {
        echo "<input name=\"{$key}[]\" id=\"{$key}[]\" type=\"hidden\" value=\"{$v}\"/>";
      }
    } else {
      if(strcmp($key, "notes") == 0 ) {
        $str = htmlentities($value, ENT_QUOTES);
        echo "<input name=\"{$key}\" id=\"{$key}\" type=\"hidden\" value=\"{$str}\"/>";
      } else {
        echo "<input name=\"{$key}\" id=\"{$key}\" type=\"hidden\" value=\"{$value}\"/>";
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
        echo "<input name=\"{$key}[]\" id=\"{$key}[]\" type=\"hidden\" value=\"{$v}\"/>";
      }
    } else {
      if(strcmp($key, "notes") == 0 ) {
        $str = htmlentities($value, ENT_QUOTES);
        echo "<input name=\"{$key}\" id=\"{$key}\" type=\"hidden\" value=\"{$str}\"/>";
      } else {
        echo "<input name=\"{$key}\" id=\"{$key}\" type=\"hidden\" value=\"{$value}\"/>";
      }
    }
  }
?>
      <span id="error" class="small">&nbsp;</span>
      <input id="prev" type="button" value="Prev" onclick="prevStep();" />
      <input id="next" type="button" value="Agree" onclick="nextStep();" />    
      <div class="spacer"></div>
    </form>
    <?php left_footer(); ?>    
  </div>
  <div id="output">
<h1>Data Policy</h1>

<br>
<p>The Fluxnet data offered on this website are contributed by individual scientists, who share their data openly with the global community.</p>

<h2>Fluxnet Tier 1 Data Use Policy</h2>

<p>Tier One data are open and free for scientific and educational purposes and their use will follow the fair use policy, stated here.  Data users describe the intended use of the data when they fill out the data-download form; this intended-use statement will be emailed to the data producer(s) and posted on the Fluxdata website (https://fluxnet.fluxdata.org).  The fair use policy dictates that (1) data producers are informed of who uses the data and for what purpose (which can be satisfied by the aforementioned mechanism) and (2) that proper acknowledgment and citations are given to all data used in a peer reviewed publication, via the following protocols: The data citation will be either a per-site DOI that is provided with the data download or a citation of a publication for each site.  Every publication should use the standard FLUXNET acknowledgment given below.  It is requested that every publication specify each site used with the FLUXNET-ID, data-years used, data DOI (in preparation), and brief acknowledgment for funding (if provided by FLUXNET PI) in the text or supplementary material.  Finally, all data providers should be informed of forthcoming publications.</p>

<p>The FLUXNET2015 Dataset should be acknowledged as follows:  This work used eddy covariance data acquired and shared by the FLUXNET community, including these networks: AmeriFlux, AfriFlux, AsiaFlux, CarboAfrica, CarboEuropeIP, CarboItaly, CarboMont, ChinaFlux, Fluxnet-Canada, GreenGrass, ICOS, KoFlux, LBA, NECC, OzFlux-TERN, TCOS-Siberia, and USCCC. The FLUXNET eddy covariance data processing and harmonization was carried out by the ICOS Ecosystem Thematic Center, AmeriFlux Management Project and Fluxdata project of FLUXNET, with the support of CDIAC, and the OzFlux, ChinaFlux and AsiaFlux offices.</p>

<br>
<p>For more information see <a href="http://fluxnet.fluxdata.org/data/data-policy/" target="_blank">http://fluxnet.fluxdata.org/data/data-policy/</a>.</p>

  </div>
  <div id="footer"><?php echo get_footer(); ?></div>
</div>
</body>
</html>

<?php 
close_database();
?>
