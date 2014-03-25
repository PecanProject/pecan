<?php
/**
 * Copyright (c) 2012 University of Illinois, NCSA.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the 
 * University of Illinois/NCSA Open Source License
 * which accompanies this distribution, and is available at
 * http://opensource.ncsa.illinois.edu/license.html
 */
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
  window.onresize = resize;
  window.onload = resize;
  
  function resize() {
      if ($("#stylized").height() < $(window).height()) {
         $("#stylized").height($(window).height()-5);
      } else {
        $("#stylized").height(Math.max($("#stylized").height(), $("#output").height()));
      }
      $("#output").height($("#stylized").height());
      $("#output").width($(window).width() - $('#stylized').width() - 5);
  }

  function prevStep() {
    $("#formprev").submit();
  }

  function nextStep() {
    $("#formnext").submit();
  }
</script>
</head>
<body>
  <div id="wrap">
    <div id="stylized">
      <form id="formnext" method="POST" action="index.php" />
<!--
      <h1>Filters</h1>
      <p>Filter executions showing on the right.</p>
-->
      <h1>Legend</h1>
      <input type="text" readonly style="background: #BBFFBB; color: black;" value="Successful runs"/>
      <input type="text" readonly style="background: #FFBBBB; color: black;" value="Runs with errors"/>
      <input type="text" readonly style="background: #BBFFFF; color: black;" value="Ongoing runs"/>
      <input type="text" readonly style="background: #FFFFFF; color: black;" value="Runs in unknown state"/>
      <p></p>
      <input id="prev" type="button" value="Start Over" onclick="nextStep();"/>
      <div class="spacer"></div>
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
        </div>
<?php
// database parameters
require("system.php");
$pdo = new PDO("${db_type}:host=${db_hostname};dbname=${db_database}", $db_username, $db_password);

// get run information
$query = "SELECT workflows.id, workflows.folder, workflows.start_date, workflows.end_date, workflows.started_at, workflows.finished_at, " .
         "CONCAT(coalesce(sites.sitename, ''), ', ', coalesce(sites.city, ''), ', ', coalesce(sites.state, ''), ', ', coalesce(sites.country, '')) AS sitename, " .
         "CONCAT(coalesce(models.model_name, ''), ' r', coalesce(models.revision, '')) AS modelname, models.model_type " .
         "FROM workflows " .
         "LEFT OUTER JOIN sites on workflows.site_id=sites.id " .
         "LEFT OUTER JOIN models on workflows.model_id=models.id " .
         "ORDER BY workflows.id DESC";
$result = $pdo->query($query);
if (!$result) {
  die('Invalid query: ' . error_database());
}
while ($row = @$result->fetch(PDO::FETCH_ASSOC)) {
  // check result
  $style="";
  $url="running_stage1.php";
  if (file_exists($row['folder'] . DIRECTORY_SEPARATOR . "STATUS")) {
    $status=file($row['folder'] . DIRECTORY_SEPARATOR . "STATUS");
    foreach ($status as $line) {
      $data = explode("\t", $line);
      if ((count($data) >= 4) && ($data[3] == 'ERROR')) {
        $style="style='background: #FFBBBB; color: black;'";
        $url="failurealert.php";
      }
    }
  } else {
    $style="style='background: #FFFFFF; color: black;'";
  }
  if (($style == "") && ($row['finished_at'] == "")) {
    $style="style='background: #BBFFFF; color: black'";
  }
  if ($style == "") {
    $style="style='background: #BBFFBB; color: black'";
    $url="finished.php";
  }
?>        
        <div id="row" <?=$style?>>
          <div id="cell"><a href="<?=$url?>?workflowid=<?=$row['id']?>"><?=$row['id']?></a></div>
          <div id="cell"><?=$row['sitename']?></div>
          <div id="cell"><?=$row['modelname']?></div>
          <div id="cell"><?=$row['model_type']?></div>
          <div id="cell"><?=$row['start_date']?></div>
          <div id="cell"><?=$row['end_date']?></div>
          <div id="cell"><?=$row['started_at']?></div>
          <div id="cell"><?=$row['finished_at']?></div>
        </div>
<?php
}
$pdo = null;
?>
      </div>
    </div>
  </div>
</body>  
