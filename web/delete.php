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
if (get_page_acccess_level() > $min_delete_level) {
  header( "Location: history.php");
  close_database();
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

# this query will return the id of the posterior iff there is another posterior that has an
# id larger than the current posterior for the same pft.
# TODO does this with multiple PFTs?
$query = "select a.id from posteriors as a, posteriors as b, ensembles, posteriors_ensembles " .
         "where ensembles.workflow_id=? and posteriors_ensembles.ensemble_id=ensembles.id and " .
         "a.id=posteriors_ensembles.posterior_id and a.pft_id=b.pft_id and b.id>a.id group by a.id;";
$stmt = $pdo->prepare($query);
if (!$stmt->execute(array($workflowid))) {
  die('Invalid query: ' . error_database());
}
$posteriors = $stmt->fetchAll(PDO::FETCH_COLUMN, 0);
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
  $deleted_files = array();
  $deleted_files['removed'] = array();
  $deleted_files['kept'] = array();
  $deleted_posteriors=0;
  foreach ($posteriors as $pid) {
    $stmt = $pdo->prepare("SELECT * FROM dbfiles WHERE container_id=? AND container_type='Posterior';");
    if (!$stmt->execute(array($pid))) {
      die('Invalid query: ' . error_database());
    }
    while ($row = @$stmt->fetch(PDO::FETCH_ASSOC)) {
      if (isset($row['file_path']) && isset($row['file_name'])) {
        $file = $row['file_path'] . DIRECTORY_SEPARATOR . $row['file_name'];
        if (file_exists($file)) {
          if (unlink("$file")) {
            $deleted_files['removed'][] = $file;
          } else {
            $deleted_files['kept'][] = $file;
          }
        } else {
          $deleted_files['kept'][] = $file;
        }
      }      
    }
    $stmt->closeCursor();
    $stmt = $pdo->prepare("DELETE FROM dbfiles WHERE container_id=? AND container_type='Posterior';");
    if (!$stmt->execute(array($pid))) {
      die('Invalid query: ' . error_database());
    }
    $delete=$stmt->rowCount();
    $stmt->closeCursor();
    $stmt = $pdo->prepare("DELETE FROM posteriors WHERE id=?;");
    if (!$stmt->execute(array($pid))) {
      die('Invalid query: ' . error_database());
    }
    $deleted_posteriors=$deleted_posteriors + $stmt->rowCount();
    $stmt->closeCursor();
  }
  
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
    $deleted_files = array_merge_recursive($deleted_files, remove_folder($workflow['folder']));
  }
}

?>
      <div class="table">
        <div class="row">
          <div class="header">ID</div><div><?php echo $workflow['id']; ?></div>
        </div>
        <div class="row">
          <div class="header">Site Name</div><div><?php echo $workflow['sitename']; ?></div>
        </div>
        <div class="row">
          <div class="header">Model Name</div><div><?php echo $workflow['modelname']; ?></div>
        </div>
        <div class="row">
          <div class="header">Model Type</div><div><?php echo $workflow['name']; ?></div>
        </div>
        <div class="row">
          <div class="header">Run Type</div><div><?php echo $ensembles['ensembles']; ?></div>
        </div>
        <div class="row">
<?php if (isset($_REQUEST['doit'])) { ?>
          <div class="header">Number of Ensembles Deleted</div><div><?php echo $deleted_ensembles . "/" .  $ensembles['ensemblescount']; ?></div>
<?php } else { ?>
          <div class="header">Number of Ensembles</div><div><?php echo $ensembles['ensemblescount']; ?></div>
<?php } ?>
        </div>
        <div class="row">
<?php if (isset($_REQUEST['doit'])) { ?>
          <div class="header">Number of Runs Deleted</div><div><?php echo $deleted_runs . "/" .  $ensembles['runs'] ; ?></div>
<?php } else { ?>
          <div class="header">Number of Runs</div><div><?php echo $ensembles['runs']; ?></div>
<?php } ?>
        </div>
        <div class="row">
<?php if (isset($_REQUEST['doit'])) { ?>
          <div class="header">Number of Posteriors Deleted</div><div><?php echo $deleted_posteriors . "/" .  count($posteriors) ; ?></div>
<?php } else { ?>
          <div class="header">Number of Posteriors</div><div><?php echo count($posteriors); ?></div>
<?php } ?>
        </div>
        <div class="row">
          <div class="header">Folder</div><div><?php echo $workflow['folder']; ?></div>
        </div>
        <div class="row">
          <div class="header">Start Date</div><div><?php echo $workflow['start_date']; ?></div>
        </div>
        <div class="row">
          <div class="header">End Date</div><div><?php echo $workflow['end_date']; ?></div>
        </div>
        <div class="row">
          <div class="header">Started</div><div><?php echo $workflow['started_at']; ?></div>
        </div>
        <div class="row">
          <div class="header">Finished</div><div><?php echo $workflow['finished_at']; ?></div>
        </div>
<?php if (isset($_REQUEST['doit'])) { ?>
        <div class="row">
          <div class="header">Inputs-Runs Deleted</div><div><?php echo $deleted_inputs_runs; ?></div>
        </div>
        <div class="row">
          <div class="header">Posteriors-Ensembles Deleted</div><div><?php echo $deleted_posteriors_ensembles; ?></div>
        </div>
<?php if (isset($delete_posteriors)) { ?>
        <div class="row">
          <div class="header">Posteriors Deleted</div><div><?php echo $delete_posteriors; ?></div>
        </div>
<?php } ?>
        <div class="row">
          <div class="header">Workflows Deleted</div><div><?php echo $deleted_workflows; ?></div>
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
    <div id="footer"><?php echo get_footer(); ?></div>
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
    $result['removed'][] = $dir;
  } else {
    $result['kept'][] = $dir;
  }
  return $result;
} 
?>
