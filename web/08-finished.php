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
$offline=isset($_REQUEST['offline']);

// runid
if (!isset($_REQUEST['workflowid'])) {
  die("Need a workflowid.");
}
$workflowid=$_REQUEST['workflowid'];

// allow for switching x-axis
$xaxis=isset($_REQUEST['xaxis']);
$xaxis=TRUE;

// get run information
$stmt = $pdo->prepare("SELECT * FROM workflows LEFT OUTER JOIN attributes ON workflows.id=attributes.container_id AND attributes.container_type='workflows' WHERE workflows.id=?");
if (!$stmt->execute(array($workflowid))) {
  die('Invalid query: ' . error_database());
}
$workflow = $stmt->fetch(PDO::FETCH_ASSOC);
$stmt->closeCursor();
$start = substr($workflow['start_date'], 0, 4);
$end = substr($workflow['end_date'], 0, 4);
$folder = $workflow['folder'];
$notes = htmlspecialchars($workflow['notes']);
if ($workflow['value'] != '') {
  $params = json_decode($workflow['value'], true);
} else {
  $params = array();
}
if (isset($params['hostname'])) {
    $hostname = "&hostname={$params['hostname']}";
}

# check to make sure all is ok
$error=false;
if (file_exists($folder . DIRECTORY_SEPARATOR . "STATUS")) {
  $status=file($folder . DIRECTORY_SEPARATOR . "STATUS");
  if ($status === FALSE) {
    $status = array();
    $error = true;
  }
  foreach ($status as $line) {
    $data = explode("\t", $line);
    if ((count($data) >= 4) && ($data[3] == 'ERROR')) {
      $error = true;
    }
  }
} else {
  $status = array();
  $error=true;
}

# check the PEcAn folder
$pecanfiles = array();
if (is_dir($folder)) {
  foreach(scandir("$folder") as $file) {
    if (is_dir("$folder/$file") || ($file[0] == ".") || ($file == "plot.out")) {
      continue;
    }
    $pecanfiles[] = $file;
  }
}
if (is_dir("$folder/ensemble")) {
  foreach(recursive_scandir("$folder/ensemble", "ensemble") as $file) {
    if ($file[0] != ".") {
      $pecanfiles[] = $file;
    }
  }
}
if (is_dir("$folder/sensitivity")) {
  foreach(recursive_scandir("$folder/sensitivity", "sensitivity") as $file) {
    if ($file[0] != ".") {
      $pecanfiles[] = $file;
    }
  }
}

# check the pft folder
$pfts = array();
if (is_dir("$folder/pft")) {
  foreach(scandir("$folder/pft") as $pft) {
    if (!is_dir("$folder/pft/$pft") || ($pft[0] == ".")) {
      continue;
    }
    $pfts[$pft] = array();
    foreach(recursive_scandir("$folder/pft/{$pft}", "") as $file) {
      if (is_dir("$folder/pft/$pft/$file")) {
        continue;
      }
      $pfts[$pft][] = $file;
    }
  }
}

# check the run folders
$inpfile = array();
$outfile = array();
$outplot = array();
if (is_dir("$folder/run")) {
  foreach(scandir("$folder/run") as $runid) {
    if (!is_dir("$folder/run/$runid") || ($runid[0] == ".")) {
      continue;
    }

    # input files
    $inpfile[$runid] = array();
    foreach(scandir("$folder/run/$runid") as $file) {
      if (is_dir("$folder/run/$runid/$file") || ($file[0] == ".")) {
        continue;
      }
      $inpfile[$runid][] = $file;
    }

    # output files
    $outfile[$runid] = array();
    $outplot[$runid] = array();
    foreach(scandir("$folder/out/$runid") as $file) {
      if (is_dir("$folder/out/$runid/$file") || ($file[0] == ".")) {
        continue;
      }
      if (preg_match('/^\d\d\d\d.nc.var$/', $file)) {
        continue;
      }
      $outfile[$runid][] = $file;
      if (preg_match('/^\d\d\d\d.nc$/', $file)) {
        $year = substr($file, 0, 4);
        $vars = explode("\n", file_get_contents("{$folder}/out/{$runid}/{$file}.var"));
        $outplot[$runid][$year] = array_filter($vars);
        sort($outplot[$runid][$year]);
      }
    }
  }
}

// quick checks for error and finished
$error = false;
$finished = false;
foreach ($status as $line) {
  $data = explode("\t", $line);
  if ((count($data) >= 4) && ($data[3] == 'ERROR')) {
    $error = true;
    $finished = true;
  }
  if ($data[0] == "FINISHED" && count($data) >= 3) {
    $finished = true;
  }
}

?>
<!DOCTYPE html>
<html>
<head>
<title>PEcAn Results</title>
<link rel="shortcut icon" type="image/x-icon" href="favicon.ico" />
<meta name="viewport" content="width=device-width, initial-scale=1.0, user-scalable=no" />
<meta http-equiv="content-type" content="text/html; charset=UTF-8" />
<link rel="stylesheet" type="text/css" href="sites.css" />
<script type="text/javascript" src="jquery-1.10.2.min.js"></script>
<script type="text/javascript">
  var pfts = new Array();
<?php
  foreach($pfts as $key => $val) {
    if (count($val) == 0) {
      continue;
    }
    $first = true;
    print "  pfts['$key'] = Array(";
    foreach($val as $x) {
      if ($first) {
        print "'$x'";
      } else {
        print ", '$x'";
      }
      $first = false;
    }
    print ");\n";
  }
?>

  var inpfile = new Array();
<?php
  foreach($inpfile as $key => $val) {
    if (count($val) == 0) {
      continue;
    }
    print " inpfile['$key'] = Array(";
    $first = true;
    foreach($val as $x) {
      if ($first) {
        print "'$x'";
      } else {
        print ", '$x'";
      }
      $first = false;
    }
    print ");\n";
  }
?>

  var outfile = new Array();
<?php
  foreach($outfile as $key => $val) {
    if (count($val) == 0) {
      continue;
    }
    print " outfile['$key'] = Array(";
    $first = true;
    foreach($val as $x) {
      if ($first) {
        print "'$x'";
      } else {
        print ", '$x'";
      }
      $first = false;
    }
    print ");\n";
  }
?>

  var outplot = new Array();
<?php
  foreach($outplot as $key => $val) {
    if (count($val) == 0) {
      continue;
    }
    print "  outplot['$key'] = Array();\n";
    foreach($val as $x => $y) {
      if (count($y) == 0) {
        continue;
      }
      print "  outplot['$key']['$x'] = {};\n";
      foreach($y as $s) {
        $kv = explode(" ", $s, 2);
        if ($kv[1] == '') $kv[1] = $kv[0];
        print "  outplot['$key']['$x']['{$kv[0]}'] = '{$kv[1]}';\n";
      }
    }
  }
?>
  function prevStep() {
    $("#formprev").submit();
  }

  function nextStep() {
    $("#formnext").submit();
  }

  function showGraph() {
    var run = $('#runid').val();
    var year = $('#graphyear').val();
    var xvar = $('#graphxvar').val();
    var yvar = $('#graphyvar').val();
    var width = $("#output").width() - 10;
    var height = $("#output").height() - 10;
<?php if ($api_url == "") {?>
    var url="dataset.php?workflowid=<?php echo $workflowid; ?>&type=plot&run=" + run + "&year=" + year + "&xvar=" + xvar + "&yvar=" + yvar + "&width=" + width + "&height=" + height;
<?php } else { ?>
    var url="<?php echo $api_url; ?>runs/" + run + "/graph/" + year + "/" + yvar + "?xvar=" + xvar +"&width=" + width + "&height=" + height;
<?php } ?>
    $("#output").html("<img src=\"" + url + "\">");
  }

  function showInputFile() {
    var run = $('#runid').val();
    var file = $('#inpfile').val();
    show("run/" + run + "/" + file);
  }

  function showOutputFile() {
    var run = $('#runid').val();
    var file = $('#outfile').val();
    show("out/" + run + "/" + file);
  }

  function showPFT() {
    var pft = $('#pft').val();
    var file = $('#pftfile').val();
    show("pft/" + pft + "/" + file);
  }

  function showPEcAnFile() {
    show($('#pecanfile').val());
  }

  function show(name) {
    var url="dataset.php?workflowid=<?php echo $workflowid; ?>&type=file&name=" + name;
    if (endsWith(url, ".xml")) {
      jQuery.get(url, {}, function(data) {
        setOuput((new XMLSerializer()).serializeToString(data));
      });
    } else if (endsWith(url, ".pdf")) {
      $("#output").html("<object data=\"" + url + "\" type=\"application/pdf\" width=\"100%\" height=\"99%\" >" +
                "alt : <a href=\"" + url + "\">Click here to download the PDF document</a>" +
                "</object>");
    } else if (endsWith(url, ".nc") || endsWith(url, ".h5") || endsWith(url, ".rdata")) {
      window.location = url;
    } else {
      jQuery.get(url, {}, setOuput);
    }
  }

  function setOuput(data) {
    data = data.replace(/&/g, "&amp;").replace(/</g,"&lt;").replace(/>/g, "&gt;").replace(/\"/g, "&quot;");
    $("#output").html("<pre>" + data + "</pre>");
  }

  function updatePFT() {
    var pft = $('#pft').val();
    $('#pftfile').empty();
    $.each(pfts[pft], function(key, value) {
         $('#pftfile')
             .append($("<option></option>")
             .text(value));
    });
  }

  function updateRun() {
    var run = $('#runid').val();

    $('#graphyear').empty();
    if (outplot[run]) {
      $.each(Object.keys(outplot[run]), function(key, value) {
           $('#graphyear')
               .append($("<option></option>")
               .text(value));
      });
      updateGraphYear();
    }

    $('#inpfile').empty();
    $.each(inpfile[run], function(key, value) {
         $('#inpfile')
             .append($("<option></option>")
             .text(value));
    });

    $('#outfile').empty();
    $.each(outfile[run], function(key, value) {
         $('#outfile')
             .append($("<option></option>")
             .text(value));
    });
  }

  function updateGraphYear() {
    var run = $('#runid').val();
    var year = $('#graphyear').val();

<?php if ($xaxis) { ?>
    $('#graphxvar').empty();
<?php } ?>
    $('#graphyvar').empty();
    if (outplot[run][year] === undefined) {
      return;
    }
    $.each(outplot[run][year], function(key, value) {
         $('#graphxvar')
             .append($("<option></option>")
                .attr("value", key)
                .text(value));
         $('#graphyvar')
             .append($("<option></option>")
                .attr("value", key)
                .text(value));
    });
<?php if ($xaxis) { ?>
    $('#graphxvar')
             .append($("<option></option>")
                .attr("value", "time")
                .text("time")
                .prop("selected", true));
<?php } ?>
    $('#graphyvar')
             .append($("<option></option>")
                .attr("value", "time")
                .text("time"));
  }

  function endsWith(haystack, needle) {
    haystack = haystack.toLowerCase();
    needle = needle.toLowerCase();
    return (haystack.substr(haystack.length - needle.length) === needle);
  }

  function startsWith(haystack, needle) {
    haystack = haystack.toLowerCase();
    needle = needle.toLowerCase();
    return (haystack.substr(0, needle.length) === needle);
  }
</script>
</head>
<body>
<div id="wrap">
  <div id="stylized">
    <form action="#" id="form">
      <h1>Results</h1>
<?php
 if (count($inpfile) < 2) {
  reset($inpfile);
  $key = key($inpfile);
?>
      <span class="small">Only 1 run (<?php echo $key; ?>)</span>
      <input id="runid" type="hidden" value="<?php echo $key; ?>" />
<?php } else { ?>
      <span class="small">Select a specific run.</span>
      <label>Run ID</label>
      <select id="runid" onChange="updateRun();">
<?php
  foreach($inpfile as $key => $val) {
    if (count($val) == 0) {
      continue;
    }
    print "        <option>$key</option>\n";
  }
?>
      </select>
<?php } ?>

      <p></p>
      <h1>Graphs</h1>

      <label>Year</label>
      <select id="graphyear" onChange="updateGraphYear();">
      </select>
      <div class="spacer"></div>

<?php if ($xaxis) { ?>
      <label>X-axis</label>
      <select id="graphxvar">
      </select>
      <div class="spacer"></div>
<?php } else { ?>
      <input type="hidden" id="graphxvar" value="time"/>
<?php } ?>

<?php if ($xaxis) { ?>
      <label>Y-axis</label>
<?php } else { ?>
      <label>Variable</label>
<?php } ?>
      <select id="graphyvar">
      </select>
      <div class="spacer"></div>

      <input id="home" type="button" value="Plot run/year/variable" onclick="showGraph();" />

      <a href="../shiny/workflowPlot/?workflow_id=<?php echo $workflowid; ?>" target="_blank">Open SHINY</a>

      <p></p>
      <h1>Inputs</h1>

      <label>File</label>
      <select id="inpfile">
      </select>
      <div class="spacer"></div>

      <input id="home" type="button" value="Show Input File" onclick="showInputFile();" />

      <p></p>
      <h1>Outputs</h1>

      <label>File</label>
      <select id="outfile">
      </select>
      <div class="spacer"></div>

      <input id="home" type="button" value="Show Output File" onclick="showOutputFile();" />

      <p></p>
      <h1>PFTs</h1>
      <label>PFT</label>
      <select id="pft" onChange="updatePFT();">

<?php
  foreach($pfts as $key => $val) {
    if (count($val) == 0) {
      continue;
    }
    print "        <option>$key</option>\n";
  }
?>
      </select>
      <div class="spacer"></div>

      <label>Output</label>
      <select id="pftfile">
      </select>
      <div class="spacer"></div>

      <input id="home" type="button" value="Show PFT Output" onclick="showPFT();" />

      <p></p>
      <h1>PEcAn Files</h1>
      <select id="pecanfile">
<?php
  foreach($pecanfiles as $file) {
    print "        <option>$file</option>\n";
  }
?>
      </select>
      <div class="spacer"></div>
      <input id="home" type="button" value="Show File" onclick="showPEcAnFile();" />

      <div class="spacer"></div>
    </form>

    <form id="formprev" method="POST" action="history.php">
<?php if ($offline) { ?>
      <input name="offline" type="hidden" value="offline">
<?php } ?>
    </form>

    <form id="formnext" method="POST" action="02-modelsite.php">
<?php if ($offline) { ?>
      <input name="offline" type="hidden" value="offline">
<?php } ?>
    </form>

    <p></p>
    <span id="error" class="small">&nbsp;</span>
    <input id="prev" type="button" value="History" onclick="prevStep();" />
<?php if (!$authentication || (get_page_acccess_level() <= $min_run_level)) { ?>
    <input id="next" type="button" value="Start Over" onclick="nextStep();"/>
<?php } ?>
    <div class="spacer"></div>
<?php whoami(); ?>
<p>
  <a href="https://pecanproject.github.io/pecan-documentation/latest/" target="_blank">Documentation</a>
  <br>
  <a href="https://join.slack.com/t/pecanproject/shared_invite/enQtMzkyODUyMjQyNTgzLWEzOTM1ZjhmYWUxNzYwYzkxMWVlODAyZWQwYjliYzA0MDA0MjE4YmMyOTFhMjYyMjYzN2FjODE4N2Y4YWFhZmQ" target="_blank">Chat Room</a>
  <br>
  <a href="http://pecanproject.github.io/Report_an_issue.html" target="_blank">Bug Report</a>
</p>
  </div>
  <div id="output">
<?php if ($notes == "") {

}
?>
  <h2>Execution Status</h2>
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
    echo "      <td>RUNNING</td>\n";
  }
  echo "    <t/r>\n";
}
?>
  </table>
<?php if ($error) { ?>
  <h2>Error in run</h2>
  <p>There was an error in the execution of the workflow. Good places to look for what could
  have gone wrong is the workflow.Rout file (which can be found under PEcAn Files pull
  down) or at the output from the model (which can be found under the Outputs pull down).</p>
<?php } else if ($finished) { ?>
  <h2>Successful Run</h2>
  <p>The workflow finished running without any errors. You can now analyze the results. You
  can create graphs from the results, or look at the outputs generated during the various
  parts of the workflow execution.</p>
<?php } else { ?>
  <h2>Still running</h2>
  <p>It looks like the model is still running, you can still look at some of the intermediate
  results, however most of the results will not be available until the end. You can also go
  <a href="05-running.php?workflowid=<?php echo $workflowid . $hostname; ?>">back</a> to the
  running page which will update continuously.</p>
<?php } ?>
  <h2>Workflow Results</h2>
<?php if ($finished && !$error) { ?>
  <p>On the top left, if you have more than one run, you can select the specific run you are
  interested in. Once you have selected a run you can create graphs from the outputs of the
  run by selecting from the three pull down menus below. You can select the year you are
  interested in, and what you want to plot on the X and Y axis. Once you have made your
  selections, pressing the "Plot run/year/variable" button will create the graph.</p>
<?php } ?>
<?php if ($finished) { ?>
  <p>Under Inputs you can select the files that were used as the inputs for the model run,
  this includes the configuration files, as well as the job file that is actually executed.
  You will also find a README.txt file that gives an overview of the parameters specified
  for this actual model run.</p>
  <p>Under Outputs you will find any outputs generated by the model, such as the actual
  model outputs as well as the logfiles of the model run. You can look at these logfiles
  to see if the model run was successful. In case of a successful model execution you will
  also find the converted outputs of the model into
  <a targe="MsTMIP" href="http://nacp.ornl.gov/">MsTMIP</a> format.</p>
<?php } ?>
  <p>The next selection will list the PFTs and all the files that were used to generate the
  input data for the models based on the PFT and trait information available.</p>
  <p>The final selection will list PEcAn specific files, and include the actual workflow.R
  script that was executed, as well as the pecan.xml that contains all the parameters used
  during the workflow execution. You will also find here the output files generated by the
  workflow execution (workflow.Rout and workflow2.Rout).</p>
  </div>
  <div id="footer"><?php echo get_footer(); ?></div>
</div>
</body>
  <script type="text/javascript">
    updateRun();
    updatePFT();
  </script>
</html>

<?php
$pdo = null;

function recursive_scandir($dir, $base) {
  $files = array();
  foreach (array_diff(scandir($dir), array('.','..')) as $file) {
    if (is_dir("$dir/$file")) {
      if ($base == "") {
        $files = array_merge($files, recursive_scandir("$dir/$file", "$file"));
      } else {
        $files = array_merge($files, recursive_scandir("$dir/$file", "$base/$file"));
      }
    } else {
      if ($base == "") {
        $files[] = $file;
      } else {
        $files[] = "$base/$file";
      }
    }
  }
  return $files;
}
?>
