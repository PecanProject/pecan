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
$finished = false;
$title = "Job Executing";
$message = "Job is currently executing, please wait.";
foreach ($status as $line) {
  $data = explode("\t", $line);
  if ((count($data) >= 4) && ($data[3] == 'ERROR')) {
    $title = "Job Failed";
    $message = "Job has finished with an error in : " . $data[0];
    $message .= "<br/>Press Finished to see the results.";
    // only send email if finished_at is not set.
    if (isset($params['email'])) {
      $url = (isset($_SERVER['HTTPS']) ? "https://" : "http://");
      $url .= $_SERVER['HTTP_HOST'] . ':' . $_SERVER['SERVER_PORT'] . $_SERVER["SCRIPT_NAME"];
      if ($offline) {
        $url .= "?workflowid=${workflowid}&offline=offline";
      } else {
        $url .= "?workflowid=${workflowid}";
      }
      mail($params['email'], "Workflow has failed", "You can find the results on $url");
    }
    $stmt = $pdo->prepare("UPDATE workflows SET finished_at=NOW() WHERE id=? AND finished_at IS NULL");
    if (!$stmt->execute(array($workflowid))) {
      die('Invalid query: ' . error_database());
    }
    $finished = true;
  }
  if ($data[0] == "ADVANCED" && count($data) < 3) {
    header( "Location: 06-edit.php?workflowid=${workflowid}${offline}");
    close_database();
    exit;
  }
  if ($data[0] == "FINISHED" && count($data) >= 3) {
    $title = "Job Finished";
    $message = "Job has finished successfully.";
    $message .= "<br/>Press Finished to see the results.";
    $finished = true;
  }
}
close_database();

# get read for refresh
if (!$finished) {
  header( "refresh:5" );
}

?>
<!DOCTYPE html>
<html>
<head>
<title><?php echo $title; ?></title>
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
    <h1><?php echo $title; ?></h1>
    <p><?php echo $message; ?></p>
    
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
<?php if ($finished) { ?>
    <input id="next" type="button" value="Finished" onclick="nextStep();" />
<?php } else { ?>
    <input id="next" type="button" value="Results" onclick="nextStep();" />
<?php } ?>
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
<?php
foreach ($status as $line) {
  $data = explode("\t", $line);
  echo "    <tr>\n";
  if ($data[0] == "BrownDog") {
    echo "      <td><a href=\"http://browndog.ncsa.illinois.edu\">";
    echo "${data[0]} <img src=\"images/browndog-small-transparent.gif\" alt=\"BrownDog\" width=\"16px\"></a></td>\n";
  } else {
    echo "      <td>${data[0]}</td>\n";    
  }
  if (count($data) >= 2) {
    echo "      <td>${data[1]}</td>\n";
  } else {
    echo "      <td></td>\n";    
  }
  if (count($data) >= 3) {
    echo "      <td>${data[2]}</td>\n";
  } else {
    echo "      <td></td>\n";    
  }
  if (count($data) >= 4) {
    echo "      <td>${data[3]}</td>\n";
  } else {
    echo "      <td>RUNNING</td>\n";        
  }
  echo "    <t/r>\n";
}
?>
  </table>
  </div>
  <div id="footer"><?php echo get_footer(); ?></div>
</div>
</body>
</html>

<?php 
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
