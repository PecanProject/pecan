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
      $("#stylized").height($(window).height() - 5);
      $("#output").height($(window).height() - 1);
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
      <h1>Filters</h1>
      <p>Filter executions showing on the right.</p>
    </div>
    <div id="output">
      <h2>Execution Status</h2>
      <table border=0>
        <tr><th>ID</th><th>site id</th><th>model id</th><th>model type</th><th>start date</th><th>end date</th><th>started</th><th>finished</th></tr>
<?php
// database parameters
require("dbinfo.php");
$connection = open_database();

// get run information
$query = "SELECT workflows.id, workflows.folder, workflows.start_date, workflows.end_date, workflows.started_at, workflows.finished_at, " .
         "CONCAT(coalesce(sites.sitename, ''), ', ', coalesce(sites.city, ''), ', ', coalesce(sites.state, ''), ', ', coalesce(sites.country, '')) AS sitename, " .
         "CONCAT(coalesce(models.model_name, ''), ' r', coalesce(models.revision, '')) AS modelname, models.model_type " .
         "FROM workflows " .
         "LEFT OUTER JOIN sites on workflows.site_id=sites.id " .
         "LEFT OUTER JOIN models on workflows.model_id=models.id";
$result = mysql_query($query);
if (!$result) {
  die('Invalid query: ' . mysql_error());
}
while ($row = @mysql_fetch_assoc($result)) {
  // check result
  $style="";
  if (file_exists($row['folder'] . DIRECTORY_SEPARATOR . "STATUS")) {
    $status=file($row['folder'] . DIRECTORY_SEPARATOR . "STATUS");
    foreach ($status as $line) {
      $data = explode("\t", $line);
      if ((count($data) >= 4) && ($data[3] == 'ERROR')) {
        $style="style='color: red;'";
      }
    }
  }
  if (($style == "") && ($row['finished_at'] == "")) {
    $style="style='color: gray;'";
  }

?>        
        <tr <?=$style?>>
          <td><a href="running_stage1.php?workflowid=<?=$row['id']?>"><?=$row['id']?></a></td>
          <td><?=$row['sitename']?></td>
          <td><?=$row['modelname']?></td>
          <td><?=$row['model_type']?></td>
          <td><?=$row['start_date']?></td>
          <td><?=$row['end_date']?></td>
          <td><?=$row['started_at']?></td>
          <td><?=$row['finished_at']?></td>
        </tr>
<?php
}
close_database($connection);
?>
      </table>
    </div>
  </div>
</body>  
