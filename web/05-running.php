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

// workflowid
if (!isset($_REQUEST['workflowid'])) {
  die("Need a workflowid.");
}
$workflowid=$_REQUEST['workflowid'];

// hostname
if (!isset($_REQUEST['hostname'])) {
  die("Need a hostname.");
}
$hostname=$_REQUEST['hostname'];
if (!array_key_exists($hostname, $hostlist)) {
  die("{$hostname} is not an approved host");
}
$hostoptions = $hostlist[$hostname];

// number of log lines
$loglines = isset($_REQUEST['loglines']) ? $_REQUEST['loglines'] : 20;

// get run information
$stmt = $pdo->prepare("SELECT folder, value FROM workflows LEFT OUTER JOIN attributes ON workflows.id=attributes.container_id AND attributes.container_type='workflows' WHERE workflows.id=?");
if (!$stmt->execute(array($workflowid))) {
  die('Invalid query: ' . error_database());
}
$workflow = $stmt->fetch(PDO::FETCH_ASSOC);
$stmt->closeCursor();

$folder = $workflow['folder'];
if ($workflow['value'] != '') {
  $params = json_decode($workflow['value'], true);
} else {
  $params = array();
}
  

// check result
if (file_exists($folder . DIRECTORY_SEPARATOR . "STATUS")) {
  $status=file($folder . DIRECTORY_SEPARATOR . "STATUS");
} else {
  $status=array();
}

// quick checks for error and finished
$finished = false;
$error = false;
$title = "Job Executing";
$message = "Job is currently executing, please wait.";
foreach ($status as $line) {
  $data = explode("\t", $line);
  if ((count($data) >= 4) && ($data[3] == 'ERROR')) {
    $title = "Job Failed";
    $message = "Job has finished with an error in : " . $data[0];
    $message .= "<br/>Press Finished to see the results.";
    // only send email if finished_at is not set.
    if (isset($params['email']) && ($params['email'] != "")) {
      $url = (isset($_SERVER['HTTPS']) ? "https://" : "http://");
      $url .= $_SERVER['HTTP_HOST'] . ':' . $_SERVER['SERVER_PORT'] . $_SERVER["SCRIPT_NAME"];
      $url .= "?workflowid={$workflowid}&loglines={$loglines}&hostname={$hostname}";
      if ($offline) {
        $url .= "&offline=offline";
      }
      mail($params['email'], "Workflow has failed", "You can find the results on $url");
    }
    $stmt = $pdo->prepare("UPDATE workflows SET finished_at=NOW() WHERE id=? AND finished_at IS NULL");
    if (!$stmt->execute(array($workflowid))) {
      die('Invalid query: ' . error_database());
    }
    $finished = true;
    $error = true;
  }
  if ($data[0] == "ADVANCED" && count($data) < 3) {
    header( "Location: 06-edit.php?workflowid={$workflowid}&hostname={$hostname}{$offline}");
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
<link rel="shortcut icon" type="image/x-icon" href="favicon.ico" />
<meta name="viewport" content="width=device-width, initial-scale=1.0, user-scalable=no" />
<meta http-equiv="content-type" content="text/html; charset=UTF-8" />
<link rel="stylesheet" type="text/css" href="sites.css" />
<script type="text/javascript" src="jquery-1.10.2.min.js"></script>
<script type="text/javascript">
  function prevStep() {
    $("#formprev").submit();
  }

  function nextStep() {
    $("#formnext").submit();
  }

  function refresh() {
    var url="<?php echo $_SERVER["SCRIPT_NAME"] . '?workflowid=' . $workflowid; ?>";
    url += "&hostname=<?php echo $hostname; ?>&loglines=" + $("#loglines").val();
    window.location.replace(url);
    return false;
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
    
    <form id="formnext" method="GET" action="08-finished.php">
<?php if ($offline != "") { ?>
      <input name="offline" type="hidden" value="offline">
<?php } ?>
      <input type="hidden" name="workflowid" value="<?php echo $workflowid; ?>" />
      <input type="hidden" name="hostname" value="<?php echo $hostname; ?>" />
      <input type="hidden" name="loglines" value="<?php echo $loglines; ?>" />
    </form>

    <span id="error" class="small">&nbsp;</span>
<?php if (!$authentication || (get_page_acccess_level() <= $min_run_level)) { ?>
      <input id="prev" type="button" value="Start Over" onclick="prevStep();"/>
<?php } ?>
<?php if ($finished) { ?>
    <input id="next" type="button" value="Finished" onclick="nextStep();" />
<?php } else { ?>
    <input id="next" type="button" value="Results" onclick="nextStep();" />
<?php } ?>
    <div class="spacer"></div>
    <?php left_footer(); ?>    
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
  echo "      <td>{$data[0]}</td>\n";    
  if (count($data) >= 2) {
    echo "      <td>{$data[1]}</td>\n";
  } else {
    echo "      <td></td>\n";    
  }
  if (count($data) >= 3) {
    echo "      <td>{$data[2]}</td>\n";
  } else {
    echo "      <td></td>\n";    
  }
  if (count($data) >= 4) {
    echo "      <td>{$data[3]}</td>\n";
  } else {
    $line = "RUNNING";
    if ($data[0] == "MODEL") {
      foreach(scandir("$folder/out") as $runid) {
        if (file_exists("$folder/out/$runid/logfile.txt")) {
          $line = tailCustom("$folder/out/$runid/logfile.txt");
        }
      }
    }
    echo "      <td>{$line}</td>\n";    
  }
  echo "    <t/r>\n";
}
?>
  </table>
<?php if ($error) { ?>
  <h2>ERROR</h2>
  There was an error in the execution of the workflow. Please see the log below, or see the
  full log in the finished view. Good places to look for what could have gone wrong is the
  workflow.Rout file (which can be found under PEcAn Files pull down) or at the output from
  the model (which can be found under the Outputs pull down).
<?php } ?>
  <h2>Workflow Log</h2>
  Last <select id="loglines" onchange="refresh();">
<?php
$lines=array(10, 20, 50, 100);
foreach($lines as &$v) {
  if ($v == $loglines) {
    echo "<option selected>{$v}</option>";
  } else {
    echo "<option>{$v}</option>";
  }
}
?>
  </select> lines of the workflow.Rout
  <div class="logfile">
<?php
  $lines=array();
  if (file_exists("$folder/workflow.Rout")) {
    $tail = explode("\n", tailCustom("$folder/workflow.Rout", $loglines));
    $lines = array_merge($lines, $tail);
  }
  if (file_exists("$folder/workflow2.Rout")) {
    $tail = explode("\n", tailCustom("$folder/workflow2.Rout", $loglines));
    $lines = array_merge($lines, $tail);
  }
  if (count($lines) > $loglines) {
    $lines = array_slice($lines, -$loglines);
  }  
  echo implode("<br/>\n", $lines);
?>
  </div>
  </div>
  <div id="footer"><?php echo get_footer(); ?></div>
</div>
</body>
</html>

<?php 
// see http://stackoverflow.com/a/15025877
function tailCustom($filepath, $lines = 1, $adaptive = true) {
  // Open file
  $f = @fopen($filepath, "rb");
  if ($f === false) return false;
  // Sets buffer size
  if (!$adaptive) $buffer = 4096;
  else $buffer = ($lines < 2 ? 64 : ($lines < 10 ? 512 : 4096));
  // Jump to last character
  fseek($f, -1, SEEK_END);
  // Read it and adjust line number if necessary
  // (Otherwise the result would be wrong if file doesn't end with a blank line)
  if (fread($f, 1) != "\n") $lines -= 1;
  
  // Start reading
  $output = '';
  $chunk = '';
  // While we would like more
  while (ftell($f) > 0 && $lines >= 0) {
    // Figure out how far back we should jump
    $seek = min(ftell($f), $buffer);
    // Do the jump (backwards, relative to where we are)
    fseek($f, -$seek, SEEK_CUR);
    // Read a chunk and prepend it to our output
    $output = ($chunk = fread($f, $seek)) . $output;
    // Jump back to where we started reading
    fseek($f, -strlen($chunk), SEEK_CUR);
    // Decrease our line counter
    $lines -= substr_count($chunk, "\n");
  }
  // While we have too many lines
  // (Because of buffer size we might have read too many)
  while ($lines++ < 0) {
    // Find first newline and remove all text before that
    $output = substr($output, strpos($output, "\n") + 1);
  }
  // Close file and return
  fclose($f);
  return trim($output);
}

?>
