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
if (!check_login()) {
  close_database();
  header( "Location: history.php");
  exit;
}

// runid
if (!isset($_REQUEST['workflowid'])) {
  close_database();
  die("Need a workflowid.");
}
$workflowid=$_REQUEST['workflowid'];

if (isset($_REQUEST['doit'])) {
  # delete inputs_runs
  $stmt = $pdo->prepare("DELETE FROM inputs_runs USING ensembles, runs WHERE ensembles.workflow_id=? AND runs.ensemble_id=ensembles.id and inputs_runs.run_id=runs.id;");
  if (!$stmt->execute(array($workflowid))) {
    die('Invalid query: ' . error_database());
  }
  $result = $stmt->fetch(PDO::FETCH_ASSOC);
  print_r($result);
  $stmt->closeCursor();

  # delete runs
  $stmt = $pdo->prepare("DELETE FROM runs USING ensembles WHERE ensembles.workflow_id=? AND runs.ensemble_id=ensembles.id;");
  if (!$stmt->execute(array($workflowid))) {
    die('Invalid query: ' . error_database());
  }
  $result = $stmt->fetch(PDO::FETCH_ASSOC);
  print_r($result);
  $stmt->closeCursor();
  
  # delete ensembles
  $stmt = $pdo->prepare("DELETE FROM ensembles WHERE ensembles.workflow_id=?;");
  if (!$stmt->execute(array($workflowid))) {
    die('Invalid query: ' . error_database());
  }
  $result = $stmt->fetch(PDO::FETCH_ASSOC);
  print_r($result);
  $stmt->closeCursor();
  
  # delete workflow
  $stmt = $pdo->prepare("DELETE FROM workflows WHERE id=?;");
  if (!$stmt->execute(array($workflowid))) {
    die('Invalid query: ' . error_database());
  }
  $result = $stmt->fetch(PDO::FETCH_ASSOC);
  print_r($result);
  $stmt->closeCursor();

  header( "Location: history.php");
  exit;  
}

?>
<!DOCTYPE html>
<html>
<head>
<title>PEcAn Delete</title>
<meta name="viewport" content="width=device-width, initial-scale=1.0, user-scalable=no" />
<meta http-equiv="content-type" content="text/html; charset=UTF-8" />
<link rel="stylesheet" type="text/css" href="sites.css" />
<script type="text/javascript" src="jquery-1.7.2.min.js"></script>
<script type="text/javascript">
  function prevStep() {
    $("#formprev").submit();
  }

  function nextStep() {
    console.log("CLICK");
    $("#formnext").submit();
  }
</script>
</head>
<body>
  <div id="wrap">
    <div id="stylized">
      <form id="formprev" method="POST" action="history.php">
      </form>
      <form id="formnext" method="POST" action="delete.php">
        <input type="hidden" name="workflowid" value="<?php echo $workflowid; ?>" />
        <input type="hidden" name="doit" value="1" />
        <h1>Delete Workflow</h1>
        <p>Are you sure you want to delete this workflow?</p>
        <input id="prev" type="button" value="No" onclick="prevStep();"/>
        <input id="next" type="button" value="Yes" onclick="nextStep();"/>
        <div class="spacer"></div>
      </form>
<?php
  if (check_login()) {
    echo "<p></p>";
    echo "Logged in as " . get_user_name();
    echo "<a href=\"index.php?logout\" style=\"float: right;\">logout</a>";
  }
?>    
    </div>
    <div id="output">
      <h2>Execution Status</h2>
<?php
print_r($_REQUEST);
// get run information
$query = "SELECT workflows.id, workflows.folder, workflows.start_date, workflows.end_date, workflows.started_at, workflows.finished_at, " .
         "CONCAT(coalesce(sites.sitename, ''), ', ', coalesce(sites.city, ''), ', ', coalesce(sites.state, ''), ', ', coalesce(sites.country, '')) AS sitename, " .
         "CONCAT(coalesce(models.model_name, ''), ' ', coalesce(models.revision, '')) AS modelname, modeltypes.name " .
         "FROM workflows " .
         "LEFT OUTER JOIN sites on workflows.site_id=sites.id " .
         "LEFT OUTER JOIN models on workflows.model_id=models.id " .
         "LEFT OUTER JOIN modeltypes on models.modeltype_id=modeltypes.id " .
         "WHERE workflows.id=?";
$stmt = $pdo->prepare($query);
if (!$stmt->execute(array($workflowid))) {
  die('Invalid query: ' . error_database());
}
$workflow = $stmt->fetch(PDO::FETCH_ASSOC);
$stmt->closeCursor();

$query = "SELECT string_agg(distinct runtype, ', ') AS ensembles, count(runs.id) AS runs FROM ensembles " .
         "LEFT OUTER JOIN runs on ensembles.id=runs.ensemble_id ".
         "WHERE ensembles.workflow_id=?";
$stmt = $pdo->prepare($query);
if (!$stmt->execute(array($workflowid))) {
  die('Invalid query: ' . error_database());
}
$ensembles = $stmt->fetch(PDO::FETCH_ASSOC);
$stmt->closeCursor();

?>
      <div id="table">
        <div id="row">
          <div id="header">ID</div><div><?php echo $workflow['id']; ?></div>
        </div>
        <div id="row">
          <div id="header">Site Name</div><div><?php echo $workflow['sitename']; ?></div>
        </div>
        <div id="row">
          <div id="header">Model Name</div><div><?php echo $workflow['modelname']; ?></div>
        </div>
        <div id="row">
          <div id="header">Model Type</div><div><?php echo $workflow['name']; ?></div>
        </div>
        <div id="row">
          <div id="header">Run Type</div><div><?php echo $ensembles['ensembles']; ?></div>
        </div>
        <div id="row">
          <div id="header">Number of Runs</div><div><?php echo $ensembles['runs']; ?></div>
        </div>
        <div id="row">
          <div id="header">Folder</div><div><?php echo $workflow['folder']; ?></div>
        </div>
        <div id="row">
          <div id="header">Start Date</div><div><?php echo $workflow['start_date']; ?></div>
        </div>
        <div id="row">
          <div id="header">End Date</div><div><?php echo $workflow['end_date']; ?></div>
        </div>
        <div id="row">
          <div id="header">Started</div><div><?php echo $workflow['started_at']; ?></div>
        </div>
        <div id="row">
          <div id="header">Finished</div><div><?php echo $workflow['finished_at']; ?></div>
        </div>
      </div>
    </div>
    <div id="footer">
      The <a href="http://pecanproject.org">PEcAn project</a> is supported by the National Science Foundation
      (ABI #1062547, ARC #1023477) and the <a href="http://www.energybiosciencesinstitute.org/">Energy
      Biosciences Institute</a>.
    </div>
  </div>
</body>  

<?php
close_database();
?>
