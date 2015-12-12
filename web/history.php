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

?>
<!DOCTYPE html>
<html>
<head>
<title>PEcAn History</title>
<meta name="viewport" content="width=device-width, initial-scale=1.0, user-scalable=no" />
<meta http-equiv="content-type" content="text/html; charset=UTF-8" />
<link rel="stylesheet" type="text/css" href="sites.css" />
<script type="text/javascript" src="jquery-1.7.2.min.js"></script>
<script type="text/javascript">
  function prevStep() {
    $("#formprev").submit();
  }

  function nextStep() {
    $("#formnext").submit();
  }

  function filter() {
    $(".unknown").toggle($("#unknown").is(':checked'));
  }

  $(document).ready(function () {
    filter();
  });
</script>
</head>
<body>
  <div id="wrap">
    <div id="stylized">
      <h1>Legend</h1>
      <input type="text" readonly style="background: #BBFFBB; color: black;" value="Successful runs"/>
      <input type="text" readonly style="background: #FFBBBB; color: black;" value="Runs with errors"/>
      <input type="text" readonly style="background: #BBFFFF; color: black;" value="Ongoing runs"/>
      <input type="text" readonly style="background: #FFFFFF; color: black;" value="Runs in unknown state"/>
      <p></p>
      <label>Show runs in unknown state?</label>
      <input id="unknown" type="checkbox" onclick="filter();"/>
<?php if (!$authentication || (get_page_acccess_level() <= $min_run_level)) { ?>
      <label>Filter history by text</label>
      <form id="formnext" method="POST" action="history.php" >
      	<input id="filter-hist" type="text" name="search-box"/>
      	<input id="filter-btn" type="submit" name="search" value="Filter" onclick="nextStep();"/>
      </form>
      <p></p>
      <form id="formprev" method="POST" action="01-introduction.php"> 
      <input id="prev" type="button" value="Start Over" onclick="prevStep();"/> 
      </form>
<?php } ?>
      <div class="spacer"></div>
<?php whoami(); ?>    
    </div>
    <div id="output">
      <h2>Execution Status</h2>
      <div id="table">
        <div id="row">
          <div id="header">ID</div>
          <div id="header">Site Name</div>
          <div id="header">Model Name</div>
          <div id="header">Model Type</div>
          <div id="header">Start Date</div>
          <div id="header">End Date</div>
          <div id="header">Started</div>
          <div id="header">Finished</div>
<?php if (check_login() && (get_page_acccess_level() <= $min_delete_level)) { ?>
          <div id="header">Delete</div>
<?php } ?>
        </div>
<?php
// get run information - filtered
$filter_val = '%';
if (isset($_POST['search'])){
 $filter_val = '%'. $_POST['search-box'] . '%';
}

$query = "SELECT workflows.id, workflows.folder, workflows.start_date, workflows.end_date, workflows.started_at, workflows.finished_at, " .
         "CONCAT(coalesce(sites.sitename, ''), ', ', coalesce(sites.city, ''), ', ', coalesce(sites.state, ''), ', ', coalesce(sites.country, '')) AS sitename, " .
         "CONCAT(coalesce(models.model_name, ''), ' ', coalesce(models.revision, '')) AS modelname, modeltypes.name " .
         "FROM workflows " .
         "LEFT OUTER JOIN sites on workflows.site_id=sites.id " .
         "LEFT OUTER JOIN models on workflows.model_id=models.id " .
         "LEFT OUTER JOIN modeltypes on models.modeltype_id=modeltypes.id " .
         "WHERE CONCAT(coalesce(sites.sitename, ''), ', ', coalesce(sites.city, ''), ', ', coalesce(sites.state, ''), ', ', coalesce(sites.country, '')) LIKE :searched_val " .
         "OR CONCAT(coalesce(models.model_name, ''), ' ', coalesce(models.revision, '')) LIKE :searched_val " .
         "OR modeltypes.name LIKE :searched_val " .
         "OR workflows.notes LIKE :searched_val " .
         "ORDER BY workflows.id DESC";

$sth = $pdo->prepare($query);
$sth->bindParam(':searched_val', $filter_val, PDO::PARAM_STR);
$result = $sth->execute();

if (!$result) {
  die('Invalid query: ' . error_database());
}
//$sth->debugDumpParams();
while ($row = @$sth->fetch(PDO::FETCH_ASSOC)) {
  // check result
  $style="";
  $url="05-running.php";
  if (file_exists($row['folder'] . DIRECTORY_SEPARATOR . "STATUS")) {
    $status=file($row['folder'] . DIRECTORY_SEPARATOR . "STATUS");
    foreach ($status as $line) {
      $data = explode("\t", $line);
      if ((count($data) >= 4) && ($data[3] == 'ERROR')) {
        $style="style='background: #FFBBBB; color: black;'";
      }
    }
  } else {
    $style="style='background: #FFFFFF; color: black; display: none;' class='unknown'";
  }
  if (($style == "") && ($row['finished_at'] == "")) {
    $style="style='background: #BBFFFF; color: black'";
  }
  if ($style == "") {
    $style="style='background: #BBFFBB; color: black'";
    $url="08-finished.php";
  }
?>        

        <div id="row" <?php echo $style; ?>>
          <div id="cell"><a href="<?php echo $url; ?>?workflowid=<?php echo $row['id']; ?>"><?php echo $row['id']; ?></a></div>
          <div id="cell"><?php echo $row['sitename']; ?></div>
          <div id="cell"><?php echo $row['modelname']; ?></div>
          <div id="cell"><?php echo $row['name']; ?></div>
          <div id="cell"><?php echo $row['start_date']; ?></div>
          <div id="cell"><?php echo $row['end_date']; ?></div>
          <div id="cell"><?php echo $row['started_at']; ?></div>
          <div id="cell"><?php echo $row['finished_at']; ?></div>
<?php if (check_login() && (get_page_acccess_level() <= $min_delete_level)) { ?>
          <div id="cell"><a href="delete.php?workflowid=<?php echo $row['id']; ?>">DELETE</a></div>
<?php } ?>
        </div>
<?php
}
close_database();
?>
      </div>
    </div>
    <div id="footer">
      The <a href="http://pecanproject.org">PEcAn project</a> is supported by the National Science Foundation
      (ABI #1062547, ARC #1023477) and the <a href="http://www.energybiosciencesinstitute.org/">Energy
      Biosciences Institute</a>.
    </div>
  </div>
</body>  
