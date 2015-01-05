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
$offline=isset($_REQUEST['offline']) ? "&offline=offline" : "";

// runid
if (!isset($_REQUEST['workflowid'])) {
  die("Need a workflowid.");
}
$workflowid=$_REQUEST['workflowid'];

// get run information
$stmt = $pdo->prepare("SELECT folder, params FROM workflows WHERE workflows.id=?");
if (!$stmt->execute(array($workflowid))) {
  die('Invalid query: ' . error_database());
}
$workflow = $stmt->fetch(PDO::FETCH_ASSOC);
$stmt->closeCursor();

$folder = $workflow['folder'];
$params = eval("return ${workflow['params']};");

// check result
if (file_exists($folder . DIRECTORY_SEPARATOR . "STATUS")) {
  $status=file($folder . DIRECTORY_SEPARATOR . "STATUS");
} else {
  $status=array();
}

// quick checks for error and finished
foreach ($status as $line) {
  $data = explode("\t", $line);
  if ((count($data) >= 4) && ($data[3] == 'ERROR')) {
    if (isset($params['email'])) {
      $url = (isset($_SERVER['HTTPS']) ? "https://" : "http://");
      $url .= $_SERVER['HTTP_HOST'] . ':' . $_SERVER['SERVER_PORT'];
      $url .= str_replace("05-running.php", "failurealert.php", $_SERVER["SCRIPT_NAME"]);
      if ($offline) {
        $url .= "?workflowid=${workflowid}&offline=offline";
      } else {
        $url .= "?workflowid=${workflowid}";
      }
      mail($params['email'], "Workflow has failed", "You can find the results on $url");
    }
    $pdo->query("UPDATE workflows SET finished_at=NOW() WHERE id=${workflowid} AND finished_at IS NULL");
    close_database();
    header( "Location: failurealert.php?workflowid=${workflowid}${offline}");
    exit;
  }
  if ($data[0] == "ADVANCED" && count($data) < 3) {
    header( "Location: 06-edit.php?workflowid=${workflowid}${offline}");
    close_database();
    exit;
  }
  if ($data[0] == "FINISHED" && count($data) >= 3) {
    header( "Location: 08-finished.php?workflowid=${workflowid}${offline}");
    close_database();
    exit;
  }
}
close_database();

# get read for refresh
header( "refresh:5" );

?>
<!DOCTYPE html>
<html>
<head>
<title>PEcAn Running</title>
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
    <h1>Job is running</h1>
    <p>Job is currently executing, please wait.</p>
    
    <form id="formprev" method="POST" action="02-modelsite.php">
<?php if ($offline != "") { ?>
      <input name="offline" type="hidden" value="offline">
<?php } ?>
    </form>
    
    <form id="formnext" method="POST" action="08-finished.php">
<?php if ($offline != "") { ?>
      <input name="offline" type="hidden" value="offline">
<?php } ?>
      <input type="hidden" name="workflowid" value="<?php echo $workflowid; ?>" />
    </form>

    <span id="error" class="small">&nbsp;</span>
    <input id="prev" type="button" value="Start Over" onclick="prevStep();" />
    <input id="next" type="button" value="Results" onclick="nextStep();" />
    <div class="spacer"></div>
<?php
  if (check_login()) {
    echo "<p></p>";
    echo "Logged in as " . get_user_name();
    echo "<a href=\"index.php?logout\" id=\"logout\">logout</a>";
  }
?>    
  </div>
  <div id="output">
  <h2>Execution Status</h2>
  <table border=1>
    <tr>
      <th>Stage Name</th>
      <th>Start Time</th>
      <th>End Time</th>
      <th>Status</th>
    </tr>
    <tr>
      <th>query.trait</th>
      <td><?php echo startTime("TRAIT");; ?></td>
      <td><?php echo endTime("TRAIT");; ?></td>
      <td><?php echo status("TRAIT");; ?></td>
    </tr>
    <tr>
      <th>meta.analysis</th>
      <td><?php echo startTime("META");; ?></td>
      <td><?php echo endTime("META");; ?></td>
      <td><?php echo status("META");; ?></td>
    </tr>
    <tr>
      <th>conversions</th>
      <td><?php echo startTime("CONVERSIONS");; ?></td>
      <td><?php echo endTime("CONVERSIONS");; ?></td>
      <td><?php echo status("CONVERSIONS");; ?></td>
    </tr>
    <tr>
      <th>write.config</th>
      <td><?php echo startTime("CONFIG");; ?></td>
      <td><?php echo endTime("CONFIG");; ?></td>
      <td><?php echo status("CONFIG");; ?></td>
    </tr>
    <tr>
      <th>advanced.edit</th>
      <td><?php echo startTime("EDIT");; ?></td>
      <td><?php echo endTime("EDIT");; ?></td>
      <td><?php echo status("EDIT");; ?></td>
    </tr>
    <tr>
      <th>model</th>
      <td><?php echo startTime("MODEL");; ?></td>
      <td><?php echo endTime("MODEL");; ?></td>
      <td><?php echo status("MODEL");; ?></td>
    </tr>
    <tr>
      <th>output.conversion</th>
      <td><?php echo startTime("OUTPUT");; ?></td>
      <td><?php echo endTime("OUTPUT");; ?></td>
      <td><?php echo status("OUTPUT");; ?></td>
    </tr>
    <tr>
      <th>ensemble.analysis</th>
      <td><?php echo startTime("ENSEMBLE");; ?></td>
      <td><?php echo endTime("ENSEMBLE");; ?></td>
      <td><?php echo status("ENSEMBLE");; ?></td>
    </tr>
    <tr>
      <th>sensitivity.analysis</th>
      <td><?php echo startTime("SENSITIVITY");; ?></td>
      <td><?php echo endTime("SENSITIVITY");; ?></td>
      <td><?php echo status("SENSITIVITY");; ?></td>
    </tr>
    <tr>
      <th>finished</th>
      <td><?php echo startTime("FINISHED");; ?></td>
      <td><?php echo endTime("FINISHED");; ?></td>
      <td><?php echo status("FINISHED");; ?></td>
    </tr>
  </table>
  </div>
  <div id="footer">
    The <a href="http://pecanproject.org">PEcAn project</a> is supported by the National Science Foundation
    (ABI #1062547, ARC #1023477) and the <a href="http://www.energybiosciencesinstitute.org/">Energy
    Biosciences Institute</a>.
  </div>
</div>
</body>
</html>

<?php 
function startTime($token) {
  global $status;
  foreach ($status as $line) {
    $data = explode("\t", $line);
    if ($data[0] == $token) {
      return $data[1];
    }
  }
  return "";
}

function endTime($token) {
  global $status;
  foreach ($status as $line) {
    $data = explode("\t", $line);
    if ($data[0] == $token && count($data) >= 3) {
      return $data[2];
    }
  }
  return "";
}

function status($token) {
  global $folder;
  global $status;

  foreach ($status as $line) {
    $data = explode("\t", $line);
    if ($data[0] == $token) {
      if (count($data) >= 4) {
        return $data[3];
      }
      if ($token == "MODEL") {
    foreach(scandir("$folder/out") as $runid) {
      if (!is_dir("$folder/out/$runid") || ($runid == ".") || ($runid == "..")) {
        continue;
      }
      if (file_exists("$folder/out/$runid/logfile.txt")) {
        $running = "$runid - " . exec("awk '/Simulating/ { print $3 }' $folder/out/$runid/logfile.txt | tail -1");
      }
    }
    return $running;
      }
      return "Running";
    }
  }
  return "";
}
?>
