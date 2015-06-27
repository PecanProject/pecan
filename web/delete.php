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
    $("#formnext").submit();
  }
</script>
</head>
<body>
  <div id="wrap">
    <div id="stylized">
      <form id="formprev" method="POST" action="history.php">
<?php if (isset($_REQUEST['doit'])) { ?>
        <h1>Delete Workflow</h1>
        <p>The workflow has been removed.</p>
        <input id="prev" type="button" value="Done" onclick="prevStep();"/>
<?php } ?>
      </form>
<?php if (! isset($_REQUEST['doit'])) { ?>
      </form>
      <form id="formnext" method="POST" action="delete.php">
        <input type="hidden" name="workflowid" value="<?php echo $workflowid; ?>" />
        <input type="hidden" name="doit" value="1" />
        <h1>Delete Workflow</h1>
        <p>Are you sure you want to delete this workflow?</p>
        <input id="prev" type="button" value="No" onclick="prevStep();"/>
        <input id="next" type="button" value="Yes" onclick="nextStep();"/>
      </form>
<?php } ?>
      <div class="spacer"></div>
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

$query = "SELECT string_agg(distinct runtype, ', ') AS ensembles, count(distinct ensembles.id) AS ensemblescount, count(runs.id) AS runs FROM ensembles " .
         "LEFT OUTER JOIN runs on ensembles.id=runs.ensemble_id ".
         "WHERE ensembles.workflow_id=?";
$stmt = $pdo->prepare($query);
if (!$stmt->execute(array($workflowid))) {
  die('Invalid query: ' . error_database());
}
$ensembles = $stmt->fetch(PDO::FETCH_ASSOC);
$stmt->closeCursor();

if (isset($_REQUEST['doit'])) {
  # delete inputs_runs
  $stmt = $pdo->prepare("DELETE FROM inputs_runs USING ensembles, runs WHERE ensembles.workflow_id=? AND runs.ensemble_id=ensembles.id and inputs_runs.run_id=runs.id;");
  if (!$stmt->execute(array($workflowid))) {
    die('Invalid query: ' . error_database());
  }
  $deleted_inputs_runs=$stmt->rowCount();
  $stmt->closeCursor();

  # delete runs
  $stmt = $pdo->prepare("DELETE FROM runs USING ensembles WHERE ensembles.workflow_id=? AND runs.ensemble_id=ensembles.id;");
  if (!$stmt->execute(array($workflowid))) {
    die('Invalid query: ' . error_database());
  }
  $deleted_runs=$stmt->rowCount();
  $stmt->closeCursor();
  
  # delete posteriors
  #$stmt = $pdo->prepare("DELETE FROM runs USING ensembles WHERE ensembles.workflow_id=? AND runs.ensemble_id=ensembles.id;");
  #if (!$stmt->execute(array($workflowid))) {
  #  die('Invalid query: ' . error_database());
  #}
  #$deleted_posteriors=$stmt->rowCount();
  #$stmt->closeCursor();
  
  # delete posteriors_ensembles
  $stmt = $pdo->prepare("DELETE FROM posteriors_ensembles USING ensembles WHERE ensembles.workflow_id=? AND posteriors_ensembles.ensemble_id=ensembles.id;");
  if (!$stmt->execute(array($workflowid))) {
    die('Invalid query: ' . error_database());
  }
  $deleted_posteriors_ensembles=$stmt->rowCount();
  $stmt->closeCursor();
  
  # delete ensembles
  $stmt = $pdo->prepare("DELETE FROM ensembles WHERE ensembles.workflow_id=?;");
  if (!$stmt->execute(array($workflowid))) {
    die('Invalid query: ' . error_database());
  }
  $deleted_ensembles=$stmt->rowCount();
  $stmt->closeCursor();
  
  # delete workflow
  $stmt = $pdo->prepare("DELETE FROM workflows WHERE id=?;");
  if (!$stmt->execute(array($workflowid))) {
    die('Invalid query: ' . error_database());
  }
  $deleted_workflows=$stmt->rowCount();
  $stmt->closeCursor();

  # remove all files if possible int the workflow folder
  if ($workflow['folder'] != "" && is_dir($workflow['folder'])) {
    $deleted_files = remove_folder($workflow['folder']);
  }
}

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
<?php if (isset($_REQUEST['doit'])) { ?>
          <div id="header">Number of Ensembles Deleted</div><div><?php echo $deleted_ensembles . "/" .  $ensembles['ensemblescount']; ?></div>
<?php } else { ?>
          <div id="header">Number of Ensembles</div><div><?php echo $ensembles['ensemblescount']; ?></div>
<?php } ?>
        </div>
        <div id="row">
<?php if (isset($_REQUEST['doit'])) { ?>
          <div id="header">Number of Runs Deleted</div><div><?php echo $deleted_runs . "/" .  $ensembles['runs'] ; ?></div>
<?php } else { ?>
          <div id="header">Number of Runs</div><div><?php echo $ensembles['runs']; ?></div>
<?php } ?>
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
<?php if (isset($_REQUEST['doit'])) { ?>
        <div id="row">
          <div id="header">Inputs-Runs Deleted</div><div><?php echo $deleted_inputs_runs; ?></div>
        </div>
        <div id="row">
          <div id="header">Posteriors-Ensembles Deleted</div><div><?php echo $deleted_posteriors_ensembles; ?></div>
        </div>
        <div id="row">
          <div id="header">Workflows Deleted</div><div><?php echo $deleted_workflows; ?></div>
        </div>
<?php } ?>
      </div>
<?php
if (isset($_REQUEST['doit'])) {
  echo "      <h2>Files/Folders removed</h2>\n";
  if (isset($deleted_files)) {
    echo "      <p>Following files/folders could not be removed.</p>\n";
    echo "      <ul>\n";
    foreach($deleted_files['kept'] as $file) {
      echo "        <li style=\"color: red;\">${file}</li>\n";
    }
    echo "      </ul>\n";
    echo "      <p>Following files/folders are removed.</p>\n";
    echo "      <ul>\n";
    foreach($deleted_files['removed'] as $file) {
      echo "        <li>${file}</li>\n";
    }
    echo "      </ul>\n";
  } else {
    echo "      <p>No files or folders were removed.</p>\n";
  }
}
?>
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

function remove_folder($dir) {
  $result = array();
  $result['removed'] = array();
  $result['kept'] = array();
  $files = array_diff(scandir($dir), array('.','..')); 
  foreach ($files as $file) {
    if (is_dir("$dir/$file")) {
      $result = array_merge_recursive($result, remove_folder("$dir/$file"));
    } else {
      if (unlink("$dir/$file")) {
        $result['removed'][] = "$dir/$file";
      } else {
        $result['kept'][] = "$dir/$file";
      }
    }
  } 
  if (rmdir("$dir")) {
    $result['removed'][] = "$dir";
  } else {
    $result['kept'][] = "$dir";
  }
  return $result;
} 
?>
