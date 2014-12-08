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

$workflowid = $_REQUEST['workflowid'];
$offline = isset($_REQUEST['offline']);

// get run information
$query = "SELECT params, folder FROM workflows WHERE workflows.id=$workflowid";
$result = $pdo->query($query);
if (!$result) {
  die('Invalid query: ' . error_database());
}
$workflow = $result->fetch(PDO::FETCH_ASSOC);
$folder = $workflow['folder'];
$params = eval("return ${workflow['params']};");  #reassemble the array since it was stored in php code

// check result
$status=file($folder . DIRECTORY_SEPARATOR . "STATUS");
if ($status === FALSE) {
  $status = array();
}
close_database();

?>
<!DOCTYPE html>
<html>
  <head>
    <title>The workflow failed to execute</title>
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
        <h1>There was an error executing the job.</h1>
        <p>Click "Finished" if you wish to proceed to the results page regardless or "Back" to change parameters and re-run.</p>

        <form id="formprev" method="POST" action="03-inputs.php">
          <?php if ($offline) { ?>
            <input name="offline" type="hidden" value="on">
          <?php } ?>
          <?php foreach ($params as $k => $v) {
            if (is_array($v)) {
              foreach($v as $x) {
                echo "<input type=\"hidden\" name=\"${k}[]\" value=\"${x}\" />\n";
              }
            } else {
              echo "<input type=\"hidden\" name=\"${k}\" value=\"${v}\" />\n";
            }
          } ?>
        </form>
        
        <form id="formnext" method="POST" action="08-finished.php">
          <?php if ($offline) { ?>
            <input name="offline" type="hidden" value="on">
          <?php } ?>
          <input type="hidden" name="workflowid" value="<?php echo $workflowid; ?>" />
        </form>

        <span id="error" class="small">&nbsp;</span>
        <input id="prev" type="button" value="Back" onclick="prevStep();" />
        <input id="next" type="button" value="Finished" onclick="nextStep();" />
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
        <p>The partial progress of the job is shown below, along with the logfile of the last stage to execute.</p>
        <br>

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
            <th>conversions</th>
            <td><?php echo startTime("CONVERSIONS");; ?></td>
            <td><?php echo endTime("CONVERSIONS");; ?></td>
            <td><?php echo status("CONVERSIONS");; ?></td>
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
        <hr/>
         <h2>Output from PEcAn</h2>
         <textarea id="log" cols="80" rows="10" readonly="readonly">
          <?php
            parselog($folder . DIRECTORY_SEPARATOR . "workflow.Rout");
          ?>
         </textarea>
      </div>
      <div id="footer">
        The <a href="http://pecanproject.org">PEcAn project</a> is supported by the National Science Foundation
        (ABI #1062547, ARC #1023477) and the <a href="http://www.energybiosciencesinstitute.org/">Energy
        Biosciences Institute</a>.
      </div>
    </div>

    <script>
      var logtext = document.getElementById('log'); 
      logtext.scrollTop = logtext.scrollHeight;    //This forces the log scroll bar to begin at the bottom for the most recent printout
    </script>
  <body>
<html>

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

function parselog($filename)
{
  // Open the file
  $f = fopen($filename, "rb");
  if ($f === false) {
    return "file does not exist.";
  }

  // read the file line by line
  while (($buffer = fgets($f, 4096)) !== false) {
    print($buffer);
  }

  // Close file and return
  fclose($f);
}
?>
